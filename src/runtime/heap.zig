// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const hash = @import("../utility/hash.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig");
const ByteVector = @import("./byte_vector.zig");

// const Activation = @import("./activation.zig");
const Activation = struct {
    activation_object: Value,
};

const Self = @This();
const UninitializedHeapScrubByte = 0xAB;

/// This is the space where newly created objects are placed. It is a fixed size
/// space, and objects that survive this space are placed in the from-space.
eden: Space,

/// This is the space where objects that survive the eden and previous scavenges
/// are placed. It is a fixed size space. When a scavenge cannot clean up enough
/// objects to leave memory for the survivors of a scavenge in this space, all
/// the objects in this space are moved to the old space where they reside until
/// a compaction happens.
from_space: Space,
/// This is a space with an identical size to the from space. When a scavenge
/// happens in the new space, the from space and this space are swapped.
to_space: Space,

/// This is the space where permanent objects reside. It can be expanded as
/// memory requirements of the program grows.
old_space: Space,

allocator: *Allocator,

// FIXME: Make eden + new space configurable at runtime
const EdenSize = 1 * 1024 * 1024;
const NewSpaceSize = 4 * 1024 * 1024;
const InitialOldSpaceSize = 16 * 1024 * 1024;

pub fn create(allocator: *Allocator) !*Self {
    const self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    try self.init(allocator);
    return self;
}

pub fn destroy(self: *Self) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn init(self: *Self, allocator: *Allocator) !void {
    self.allocator = allocator;

    self.old_space = try Space.init(allocator, InitialOldSpaceSize);
    errdefer self.old_space.deinit(allocator);

    self.from_space = try Space.init(allocator, NewSpaceSize);
    errdefer self.from_space.deinit(allocator);

    self.to_space = try Space.init(allocator, NewSpaceSize);
    errdefer self.to_space.deinit(allocator);

    self.eden = try Space.init(allocator, EdenSize);

    self.from_space.scavenge_target = &self.to_space;
    self.from_space.tenure_target = &self.old_space;
    self.eden.tenure_target = &self.from_space;
}

fn deinit(self: *Self) void {
    self.eden.deinit(self.allocator);
    self.from_space.deinit(self.allocator);
    self.to_space.deinit(self.allocator);
    self.old_space.deinit(self.allocator);
}

// Attempts to allocate `size` bytes in the object segment of the eden. If
// necessary, garbage collection is performed in the process.
// The given address must be a multiple of `@sizeOf(u64)`.
pub fn allocateInObjectSegment(self: *Self, size: usize) ![*]u64 {
    return try self.eden.allocateInObjectSegment(self.allocator, &[_]*Activation{}, size);
}

pub fn allocateInByteVectorSegment(self: *Self, size: usize) ![*]u64 {
    return try self.eden.allocateInByteVectorSegment(self.allocator, &[_]*Activation{}, size);
}

/// A mapping from an address to its size. This area of memory is checked for
/// any object references in the current space which are then copied during a
/// scavenge.
const RememberedSet = std.AutoArrayHashMapUnmanaged([*]u64, usize);
const Space = struct {
    /// The raw memory contents of the space. The space capacity can be learned
    /// with `memory.len`.
    memory: []u64,
    /// Points to the first free address in this space's object segment (which
    /// grows upwards in memory).
    object_cursor: [*]u64,
    /// Points to the first used address in this space's bytevector segment
    /// (which grows downwards in memory). May point to the byte after `memory`,
    /// in which case the bytevector segment is empty.
    byte_vector_cursor: [*]u64,
    /// The set of objects which reference an object in this space. When a
    /// constant or assignable slot from a previous space references this space,
    /// it is added to this set; when it starts referencing another space, it is
    /// removed from this space. During scavenging, this space is cleared and
    /// any references that were still pointing to this space at scavenge time
    /// are transferred to the target space.
    ///
    /// FIXME: The original Self VM used "cards" (i.e. a bitmap) to mark certain
    ///        regions of memory as pointing to a new space indiscriminate of
    ///        object size. Figure out whether that is faster than this
    ///        approach.
    remembered_set: RememberedSet,
    /// The scavenging target of this space. When the space runs out of memory
    /// and this space is set, the space will attempt to perform a scavenging
    /// operation towards this space. This space must have the same size as the
    /// current space and must be empty when the scavenging starts. After the
    /// scavenge is complete, this object swaps its memory and cursors with the
    /// other object.
    scavenge_target: ?*Space = null,
    /// The tenure target of this space. When the space runs out of memory and a
    /// scavenge did not clear enough memory, a tenuring operation is done in
    /// order to evacuate all the objects to a higher generation.
    tenure_target: ?*Space = null,

    pub fn init(allocator: *Allocator, size: usize) !Space {
        var memory = try allocator.alloc(u64, size / @sizeOf(u64));
        return Space{
            .memory = memory,
            .object_cursor = memory.ptr,
            .byte_vector_cursor = memory.ptr + memory.len,
            .remembered_set = .{},
        };
    }

    pub fn deinit(self: *Space, allocator: *Allocator) void {
        self.remembered_set.deinit(allocator);
        allocator.free(self.memory);
    }

    /// Return the amount of free memory in this space in bytes.
    pub fn freeMemory(self: *Space) usize {
        const memory_word_count = self.memory.len;
        const start_of_memory = self.memory.ptr;
        const end_of_memory = start_of_memory + memory_word_count;

        const object_size = @ptrToInt(self.object_cursor) - @ptrToInt(start_of_memory);
        const byte_vector_size = @ptrToInt(end_of_memory) - @ptrToInt(self.byte_vector_cursor);

        return memory_word_count * @sizeOf(u64) - object_size - byte_vector_size;
    }

    /// Performs Cheney's algorithm, copying alive objects to the given target.
    pub fn cheneyCommon(self: *Space, allocator: *Allocator, activation_stack: []const *Activation, target_space: *Space) Allocator.Error!void {
        // First see if the target space has enough space to potentially take
        // everything in this space.
        const space_size = self.memory.len * @sizeOf(u64);
        const required_size = space_size - self.freeMemory();
        if (required_size > target_space.freeMemory()) {
            // Make the other space scavenge first.
            try target_space.collectGarbage(allocator, required_size, activation_stack);

            if (space_size - self.freeMemory() > target_space.freeMemory()) {
                std.debug.panic("Even after a garbage collection, the target space doesn't have enough memory for me to perform a scavenge, sorry.", .{});
            }
        }
        // If we got here, then we have enough free memory on the target space
        // to perform our operations.

        // These are saved in order to race against them in the final phase
        // of the scavenge.
        var object_scan_cursor = target_space.object_cursor;

        // Go through the whole activation stack, copying activation objects
        // that are within this space
        for (activation_stack) |activation| {
            const activation_object_reference = activation.activation_object;
            std.debug.assert(activation_object_reference.isObjectReference());
            const activation_object_address = activation_object_reference.asObject().getAddress();

            // If the activation object address is not within the object segment
            // of our space, then we do not care about it.
            if (!self.objectSegmentContains(activation_object_address))
                continue;

            const new_address = copyObjectTo(allocator, activation_object_address, target_space);
            activation.activation_object = Value.fromObjectAddress(new_address);
        }

        var remembered_set_iterator = self.remembered_set.iterator();
        // Go through the remembered set, and copy any referenced objects.
        // Transfer the remembered set objects to the new space if it has a
        // remembered set of its own. (TODO old space shouldn't have one)
        while (remembered_set_iterator.next()) |entry| {
            const start_of_object = entry.key_ptr.*;
            const object_size = entry.value_ptr.*;
            const end_of_object = start_of_object + object_size;

            var cursor = start_of_object;
            var found_references: usize = 0;
            while (@ptrToInt(cursor) < @ptrToInt(end_of_object)) : (cursor += 1) {
                const word = cursor[0];
                const value = Value{ .data = word };

                if (value.isObjectReference()) {
                    const address = value.asObject().getAddress();

                    if (self.objectSegmentContains(address)) {
                        cursor.* = Value.fromObjectAddress(copyObjectTo(allocator, address, target_space)).data;
                        found_references += 1;
                    } else if (self.byteVectorSegmentContains(address)) {
                        cursor.* = Value.fromObjectAddress(copyByteVectorTo(allocator, address, target_space)).data;
                        found_references += 1;
                    }
                }
            }

            // Make sure that the object in the remembered set actually has a
            // purpose of being there, i.e. actually contains references to this
            // space.
            std.debug.assert(found_references > 0);

            try target_space.remembered_set.put(allocator, start_of_object, object_size);
        }

        // Try to catch up to the target space's object and byte vector cursors,
        // copying any other objects/byte vectors that still exist in this space
        while (@ptrToInt(object_scan_cursor) < @ptrToInt(target_space.object_cursor)) : (object_scan_cursor += 1) {
            const word = object_scan_cursor[0];
            const value = Value{ .data = word };

            if (value.isObjectReference()) {
                const address = value.asObjectAddress();

                if (self.objectSegmentContains(address)) {
                    object_scan_cursor[0] = Value.fromObjectAddress(copyObjectTo(allocator, address, target_space)).data;
                } else if (self.byteVectorSegmentContains(address)) {
                    object_scan_cursor[0] = Value.fromObjectAddress(copyByteVectorTo(allocator, address, target_space)).data;
                }
            }
        }

        std.debug.assert(object_scan_cursor == target_space.object_cursor);

        // Reset this space's pointers and remembered set, as it is now
        // effectively empty.
        self.object_cursor = self.memory.ptr;
        self.byte_vector_cursor = self.memory.ptr + self.memory.len;
        self.remembered_set.clearRetainingCapacity();
    }

    /// Performs a garbage collection operation on this space.
    ///
    /// This method performs either a scavenge or a tenure; if a scavenge target
    /// is defined for this space, it first attempts a scavenge. If not enough
    /// memory is cleaned up by a scavenge operation, or there wasn't a defined
    /// scavenge target, a *tenure* is attempted towards the tenure target of
    /// this space. If a tenure target is not specified either, then the heap is
    /// simply expanded to accomodate the new objects.
    pub fn collectGarbage(self: *Space, allocator: *Allocator, required_memory: usize, activation_stack: []const *Activation) !void {
        // See if we already have the required memory amount.
        if (self.freeMemory() >= required_memory) return;

        // See if we can perform a scavenge first.
        if (self.scavenge_target) |scavenge_target| {
            try self.cheneyCommon(allocator, activation_stack, scavenge_target);
            self.swapMemoryWith(scavenge_target);

            if (self.freeMemory() >= required_memory) return;
        }

        // Looks like the scavenge didn't give us enough memory. Let's attempt a
        // tenure.
        if (self.tenure_target) |tenure_target| {
            try self.cheneyCommon(allocator, activation_stack, tenure_target);

            // FIXME: Return an error instead of panicing when the allocation
            //        is too large for this space. How should we handle large
            //        allocations anyway?
            std.debug.assert(self.freeMemory() >= required_memory);
            return;
        }

        std.debug.panic("TODO expanding a space which doesn't have a tenure or scavenge target", .{});
    }

    /// Copy the given address as a new object in the target space. Creates a
    /// forwarding reference in this space; if more than one object was pointing
    /// to this object in the old space, then a special marker is placed in the
    /// location of the old object which tells the future calls of this function
    /// for the same object to just return the new location and avoid copying
    /// again.
    fn copyObjectTo(allocator: *Allocator, address: [*]u64, target_space: *Space) [*]u64 {
        const object = Object.fromAddress(address);
        if (object.isForwardingReference()) {
            const forward_address = object.getForwardAddress();
            return forward_address;
        }

        const object_size = object.getSizeInMemory();
        std.debug.assert(object_size % @sizeOf(u64) == 0);

        const object_size_in_words = object_size / @sizeOf(u64);
        // We must have enough space at this point.
        const new_address = target_space.allocateInObjectSegment(allocator, &[_]*Activation{}, object_size) catch unreachable;
        std.mem.copy(u64, new_address[0..object_size_in_words], address[0..object_size_in_words]);

        // Create a forwarding reference
        object.setForwardAddress(new_address);
        return new_address;
    }

    /// Same as copyObjectTo, but for byte vectors.
    fn copyByteVectorTo(allocator: *Allocator, address: [*]u64, target_space: *Space) [*]u64 {
        const byte_vector = ByteVector.fromAddress(address);
        const byte_vector_size = byte_vector.getSizeInMemory();
        std.debug.assert(byte_vector_size % @sizeOf(u64) == 0);

        const byte_vector_size_in_words = byte_vector_size / @sizeOf(u64);
        // We must have enough space at this point.
        const new_address = target_space.allocateInByteVectorSegment(allocator, &[_]*Activation{}, byte_vector_size) catch unreachable;
        std.mem.copy(u64, new_address[0..byte_vector_size_in_words], address[0..byte_vector_size_in_words]);

        return new_address;
    }

    fn swapMemoryWith(self: *Space, target_space: *Space) void {
        std.mem.swap([]u64, &self.memory, &target_space.memory);
        std.mem.swap([*]u64, &self.object_cursor, &target_space.object_cursor);
        std.mem.swap([*]u64, &self.byte_vector_cursor, &target_space.byte_vector_cursor);
    }

    fn objectSegmentContains(self: *Space, address: [*]u64) bool {
        return @ptrToInt(address) >= @ptrToInt(self.memory.ptr) and @ptrToInt(address) < @ptrToInt(self.object_cursor);
    }

    fn byteVectorSegmentContains(self: *Space, address: [*]u64) bool {
        return @ptrToInt(address) >= @ptrToInt(self.byte_vector_cursor) and @ptrToInt(address) < @ptrToInt(self.memory.ptr + self.memory.len);
    }

    /// Allocates the requested amount in bytes in the object segment of this
    /// space, garbage collecting if there is not enough space.
    pub fn allocateInObjectSegment(self: *Space, allocator: *Allocator, activation_stack: []const *Activation, size: usize) ![*]u64 {
        std.debug.assert(size % 8 == 0);
        if (self.freeMemory() < size) try self.collectGarbage(allocator, size, activation_stack);

        const start_of_object = self.object_cursor;
        self.object_cursor += size / @sizeOf(u64);

        if (builtin.mode == .Debug)
            std.mem.set(u8, @ptrCast([*]align(@alignOf(u64)) u8, start_of_object)[0..size], UninitializedHeapScrubByte);

        return start_of_object;
    }

    /// Allocates the requested amount in bytes in the byte vector segment of
    /// this space, garbage collecting if there is not enough space.
    pub fn allocateInByteVectorSegment(self: *Space, allocator: *Allocator, activation_stack: []const *Activation, size: usize) ![*]u64 {
        std.debug.assert(size % 8 == 0);
        if (self.freeMemory() < size) try self.collectGarbage(allocator, size, activation_stack);

        self.byte_vector_cursor -= size / @sizeOf(u64);

        if (builtin.mode == .Debug)
            std.mem.set(u8, @ptrCast([*]align(@alignOf(u64)) u8, self.byte_vector_cursor)[0..size], UninitializedHeapScrubByte);

        return self.byte_vector_cursor;
    }
};

test "allocate one object's worth of space on the heap" {
    const allocator = std.testing.allocator;

    var heap = try Self.create(allocator);
    defer heap.destroy();

    const eden_free_memory = heap.eden.freeMemory();
    _ = try heap.allocateInObjectSegment(16);
    try std.testing.expectEqual(eden_free_memory - 16, heap.eden.freeMemory());
}

test "fill up the eden with objects and attempt to allocate one more" {
    const allocator = std.testing.allocator;

    var heap = try Self.create(allocator);
    defer heap.destroy();

    const eden_free_memory = heap.eden.freeMemory();
    while (heap.eden.freeMemory() > 0) {
        _ = try heap.allocateInObjectSegment(8);
    }

    _ = try heap.allocateInObjectSegment(16);
    try std.testing.expectEqual(eden_free_memory - 16, heap.eden.freeMemory());
    // Expect the from space to be empty, as there were no object refs this
    // entire time
    try std.testing.expectEqual(heap.from_space.memory.ptr, heap.from_space.object_cursor);
}

test "link an object to another and perform scavenge" {
    const allocator = std.testing.allocator;

    var heap = try Self.create(allocator);
    defer heap.destroy();

    // The object being referenced
    var referenced_object_map = try Object.Map.Slots.create(heap, 1);
    var actual_name = try ByteVector.createFromString(heap, "actual");
    referenced_object_map.getSlots()[0].initConstant(actual_name, .NotParent, Value.fromInteger(0xDEADBEEF));
    var referenced_object = try Object.Slots.create(heap, referenced_object_map, &[_]Value{});

    // The "activation object", which is how we get a reference to the object in
    // the from space after the tenure is done
    var activation_object_map = try Object.Map.Slots.create(heap, 1);
    var reference_name = try ByteVector.createFromString(heap, "reference");
    activation_object_map.getSlots()[0].initMutable(activation_object_map, reference_name, .NotParent);
    var activation_object = try Object.Slots.create(heap, activation_object_map, &[_]Value{referenced_object.asValue()});

    // Create the activation
    var activation = Activation{ .activation_object = activation_object.asValue() };

    // Activate the garbage collection, tenuring from the eden to from space
    try heap.eden.collectGarbage(allocator, EdenSize, &[_]*Activation{&activation});

    // Find the new activation object
    var new_activation_object = activation.activation_object.asObject().asSlotsObject();
    try std.testing.expect(activation_object != new_activation_object);
    var new_activation_object_map = new_activation_object.getMap();
    try std.testing.expect(activation_object_map != new_activation_object_map);
    try std.testing.expect(activation_object_map.getSlots()[0].name.asObjectAddress() != new_activation_object_map.getSlots()[0].name.asObjectAddress());
    try std.testing.expectEqualStrings("reference", new_activation_object_map.getSlots()[0].name.asByteVector().getValues());

    // Find the new referenced object
    var new_referenced_object = new_activation_object.getAssignableSlotValueByName("reference").?.asObject().asSlotsObject();
    try std.testing.expect(referenced_object != new_referenced_object);
    var new_referenced_object_map = new_referenced_object.getMap();
    try std.testing.expect(referenced_object_map != new_referenced_object_map);
    try std.testing.expect(referenced_object_map.getSlots()[0].name.asObjectAddress() != new_referenced_object_map.getSlots()[0].name.asObjectAddress());
    try std.testing.expectEqualStrings("actual", new_referenced_object_map.getSlots()[0].name.asByteVector().getValues());

    // Verify that the map map is shared (aka forwarding addresses work)
    try std.testing.expectEqual(
        new_activation_object_map.map.header.getMap(),
        new_referenced_object_map.map.header.getMap(),
    );

    // Get the value we stored and compare it
    var referenced_object_value = new_referenced_object.getMap().getSlotByName("actual").?.value;
    try std.testing.expectEqual(@as(u64, 0xDEADBEEF), referenced_object_value.asInteger());
}
