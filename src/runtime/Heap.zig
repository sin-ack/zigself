// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const hash = @import("../utility/hash.zig");
const debug = @import("../debug.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig").Object;
const ByteArray = @import("./ByteArray.zig");
const Activation = @import("./Activation.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const ActivationStack = Activation.ActivationStack;

const GC_DEBUG = debug.GC_DEBUG;
const GC_TOKEN_DEBUG = debug.GC_TOKEN_DEBUG;
const GC_TOKEN_ALLOCATION_DEBUG = debug.GC_TOKEN_ALLOCATION_DEBUG;
const GC_TRACK_SOURCE_DEBUG = debug.GC_TRACK_SOURCE_DEBUG;
const REMEMBERED_SET_DEBUG = debug.REMEMBERED_SET_DEBUG;
const HEAP_HANDLE_MISS_DEBUG = debug.HEAP_HANDLE_MISS_DEBUG;
const CRASH_ON_OUT_OF_ORDER_HANDLE_FREES = debug.CRASH_ON_OUT_OF_ORDER_HANDLE_FREES;

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

/// Holds the handles to heap values. The values stored here will be scanned
/// during heap allocations.
handles: [HandleAreaSize]?[*]u64 = .{null} ** HandleAreaSize,
/// The index of the handle that was allocated most recently. We do a linear
/// search in a ring fashion from this offset to find a new slot.
// Begins from HandleAreaSize - 1 in order to search the first index first.
most_recent_handle_index: std.math.IntFittingRange(0, HandleAreaSize - 1) = HandleAreaSize - 1,

allocator: Allocator,
/// The virtual machine running the whole thing. We use this to scan the
/// argument and slot stacks during a scavenge/tenure.
vm: *VirtualMachine,

/// Tracks which return addresses track which addresses.
caller_tracked_mapping: if (GC_TRACK_SOURCE_DEBUG) std.AutoArrayHashMap(*[*]u64, usize) else void,

// FIXME: Make eden + new space configurable at runtime
const EdenSize = 1 * 1024 * 1024;
const NewSpaceSize = 4 * 1024 * 1024;
const InitialOldSpaceSize = 16 * 1024 * 1024;
// 2x the empirically determined maximum handles used during zig build test.
const HandleAreaSize = 32;

const Segment = enum { Object, ByteArray };
pub const AllocationToken = struct {
    heap: *Self,
    total_bytes: usize,
    bytes_left: usize,

    pub fn allocate(self: *@This(), segment: Segment, bytes: usize) [*]u64 {
        if (self.bytes_left < bytes) {
            std.debug.panic(
                "!!! Attempted to allocate {} bytes from {} byte-sized allocation token with {} bytes remaining!",
                .{ bytes, self.total_bytes, self.bytes_left },
            );
        }

        self.bytes_left -= bytes;
        if (GC_TOKEN_ALLOCATION_DEBUG) std.debug.print("AllocationToken.allocate: {}/{} bytes allocated (requested {})\n", .{ self.total_bytes - self.bytes_left, self.total_bytes, bytes });
        // NOTE: The only error this can raise is allocation failure during lazy allocation
        //       which eden does not do.
        return self.heap.eden.allocateInSegment(self.heap.allocator, segment, bytes) catch unreachable;
    }

    pub fn deinit(self: @This()) void {
        if (std.debug.runtime_safety) {
            if (self.bytes_left != 0) {
                std.debug.panic("!!! Only {} out of {} bytes consumed from the allocation token!", .{ self.total_bytes - self.bytes_left, self.total_bytes });
            }
        }
    }
};

pub fn create(allocator: Allocator, vm: *VirtualMachine) !*Self {
    const self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    try self.init(allocator, vm);
    return self;
}

pub fn destroy(self: *Self) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn init(self: *Self, allocator: Allocator, vm: *VirtualMachine) !void {
    var old_space = Space.lazyInit(self, "old space", InitialOldSpaceSize);
    errdefer old_space.deinit(allocator);

    var from_space = try Space.init(self, allocator, "from space", NewSpaceSize);
    errdefer from_space.deinit(allocator);

    var to_space = try Space.init(self, allocator, "to space", NewSpaceSize);
    errdefer to_space.deinit(allocator);

    var eden = try Space.init(self, allocator, "eden", EdenSize);
    errdefer eden.deinit(allocator);

    self.* = .{
        .vm = vm,
        .allocator = allocator,
        .caller_tracked_mapping = if (GC_TRACK_SOURCE_DEBUG) std.AutoArrayHashMap(*[*]u64, usize).init(allocator) else {},
        .eden = eden,
        .from_space = from_space,
        .to_space = to_space,
        .old_space = old_space,
    };

    self.from_space.scavenge_target = &self.to_space;
    self.from_space.tenure_target = &self.old_space;
    self.eden.tenure_target = &self.from_space;
}

fn deinit(self: *Self) void {
    self.eden.deinit(self.allocator);
    self.from_space.deinit(self.allocator);
    self.to_space.deinit(self.allocator);
    self.old_space.deinit(self.allocator);

    if (GC_TRACK_SOURCE_DEBUG) {
        self.caller_tracked_mapping.deinit();
    }
}

pub fn getAllocation(self: *Self, bytes: usize) !AllocationToken {
    if (GC_TOKEN_DEBUG) std.debug.print("Heap.getAllocation: Attempting to get a token of size {}\n", .{bytes});
    try self.eden.collectGarbage(self.allocator, bytes);

    if (bytes % @sizeOf(u64) != 0)
        std.debug.panic("!!! Attempted to allocate {} bytes which is not a multiple of @sizeOf(u64)!", .{bytes});

    return AllocationToken{ .heap = self, .total_bytes = bytes, .bytes_left = bytes };
}

/// Mark the given address within the heap as an object which needs to know when
/// it is finalized. The address must've just been allocated (i.e. still in
/// eden).
pub fn markAddressAsNeedingFinalization(self: *Self, address: [*]u64) !void {
    if (!self.eden.objectSegmentContains(address)) {
        std.debug.panic("!!! markAddressAsNeedingFinalization called on address which isn't in eden object segment", .{});
    }

    try self.eden.addToFinalizationSet(self.allocator, address);
}

fn allocateHandle(self: *Self) *?[*]u64 {
    var handle_index = self.most_recent_handle_index +% 1;
    while (handle_index != self.most_recent_handle_index) : (handle_index +%= 1) {
        const handle = &self.handles[handle_index];
        if (handle.* != null) {
            if (HEAP_HANDLE_MISS_DEBUG) std.debug.print("Heap.allocateHandle: Handle index {} was full, retrying\n", .{handle_index});
            continue;
        }

        self.most_recent_handle_index = handle_index;
        return handle;
    }

    @panic("!!! Could not find a free handle slot!");
}

fn freeHandle(self: *Self, handle: *?[*]u64) void {
    if (CRASH_ON_OUT_OF_ORDER_HANDLE_FREES) {
        if (handle != &self.handles[self.most_recent_handle_index]) {
            @panic("!!! Out-of-order handle free!");
        }
    }

    const new_most_recent_handle_index: @TypeOf(self.most_recent_handle_index) = @intCast(@divExact(@intFromPtr(handle) - @intFromPtr(&self.handles), @sizeOf([*]u64)));
    self.most_recent_handle_index = new_most_recent_handle_index -% 1;
    handle.* = null;
}

/// Track the given value, returning a Tracked. When a garbage collection
/// occurs, the value will be updated with the new location.
pub fn track(self: *Self, value: Value) !Tracked {
    if (value.isObjectReference()) {
        const handle = self.allocateHandle();
        handle.* = value.asObjectAddress();

        // XXX: The handle cannot be null at this point.
        const nonnull_handle: *[*]u64 = @ptrCast(handle);

        if (GC_TRACK_SOURCE_DEBUG) {
            const address = @returnAddress();
            try self.caller_tracked_mapping.put(nonnull_handle, address);
        }

        const tracked = Tracked.createWithObject(nonnull_handle);

        return tracked;
    } else {
        return Tracked.createWithLiteral(value);
    }
}

/// Untracks the given value.
pub fn untrack(self: *Self, tracked: Tracked) void {
    if (tracked.value == .Object) {
        if (GC_TRACK_SOURCE_DEBUG) {
            _ = self.caller_tracked_mapping.swapRemove(tracked.value.Object);
        }

        // XXX: The handle dies here, so this is alright.
        self.freeHandle(@ptrCast(tracked.value.Object));
    }
}

pub fn visitValues(
    self: *Self,
    // TODO: Write interfaces proposal for Zig
    visitor: anytype,
) !void {
    for (&self.handles) |*handle| {
        if (handle.*) |h| {
            var handle_as_value = Value.fromObjectAddress(h);
            try visitor.visit(&handle_as_value);
            handle.* = handle_as_value.asObjectAddress();
        }
    }

    try self.vm.visitValues(visitor);
}

/// Go through the whole heap, updating references to the given value with the
/// new value.
pub fn updateAllReferencesTo(self: *Self, old_value: Value, new_value: Value) !void {
    const Visitor = struct {
        old_value: Value,
        new_value: Value,

        pub fn visit(context: @This(), value: *Value) !void {
            if (value.data == context.old_value.data)
                value.* = context.new_value;
        }
    };

    self.visitValues(Visitor{ .old_value = old_value, .new_value = new_value }) catch unreachable;

    // Finally scan each heap space and update the references.
    try self.eden.updateAllReferencesTo(self.allocator, old_value.asObjectAddress(), new_value.asObjectAddress());
}

/// Figure out which spaces the referrer and target object are in, and add an
/// entry to the target object's space's remembered set. This ensures that the
/// object in the old space gets its references properly updated when the new
/// space gets garbage collected.
pub fn rememberObjectReference(self: *Self, referrer: Value, target: Value) !void {
    // FIXME: If we add an assignable slot to traits integer for instance, this
    //        will cause the assignment code to explode. What can we do there?
    std.debug.assert(referrer.isObjectReference());
    if (!target.isObjectReference()) return;

    if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Trying to create a reference {*} -> {*}\n", .{ referrer.asObjectAddress(), target.asObjectAddress() });

    const referrer_address = referrer.asObjectAddress();
    const target_address = target.asObjectAddress();

    var referrer_space: ?*Space = null;
    var target_space: ?*Space = null;

    if (self.eden.objectSegmentContains(referrer_address)) referrer_space = &self.eden;
    if (self.from_space.objectSegmentContains(referrer_address)) referrer_space = &self.from_space;
    if (self.old_space.objectSegmentContains(referrer_address)) referrer_space = &self.old_space;
    std.debug.assert(referrer_space != null);

    if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Referrer is in {s}\n", .{referrer_space.?.name});

    const referrer_space_is_newer = blk: {
        if (self.eden.objectSegmentContains(target_address)) {
            if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Target is in eden\n", .{});
            if (referrer_space.? == &self.eden) break :blk false;
            target_space = &self.eden;
        } else if (self.from_space.objectSegmentContains(target_address)) {
            if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Target is in from space\n", .{});
            if (referrer_space.? == &self.eden or referrer_space.? == &self.from_space) break :blk false;
            target_space = &self.from_space;
        } else if (self.old_space.objectSegmentContains(target_address)) {
            if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Target is in old space\n", .{});
            // Old space to old space references need not be updated, as the old
            // space is supposed to infinitely expand.
            break :blk false;
        }
        std.debug.assert(target_space != null);
        break :blk true;
    };
    if (!referrer_space_is_newer) {
        if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Referrer in same or older space than target, not creating a reference.\n", .{});
        return;
    }

    if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Adding to remembered set of {s}\n", .{target_space.?.name});
    try target_space.?.addToRememberedSet(self.allocator, referrer_address, referrer.asObject().getSizeInMemory());
}

pub fn printTrackLocationCounts(self: *Self) void {
    const debug_info = std.debug.getSelfDebugInfo() catch unreachable;
    const stderr = std.io.getStdErr();
    const tty_config = std.io.tty.detectConfig(stderr);

    var address_counts = std.AutoArrayHashMap(usize, usize).init(self.allocator);
    defer address_counts.deinit();

    var mapping_it = self.caller_tracked_mapping.iterator();
    while (mapping_it.next()) |entry| {
        var result = address_counts.getOrPut(entry.value_ptr.*) catch unreachable;
        if (result.found_existing) {
            result.value_ptr.* += 1;
        } else {
            result.value_ptr.* = 1;
        }
    }

    std.debug.print("Leftover trackings:\n", .{});
    var count_it = address_counts.iterator();
    while (count_it.next()) |entry| {
        std.debug.printSourceAtAddress(debug_info, stderr.writer(), entry.key_ptr.*, tty_config) catch unreachable;
        std.debug.print("Count: {}\n\n", .{entry.value_ptr.*});
    }
}

/// Return whether a garbage collection cycle would need to happen in order to
/// provide the required amount of bytes from the heap.
pub fn needsToGarbageCollectToProvide(self: Self, bytes: usize) bool {
    return self.eden.freeMemory() < bytes;
}

/// A mapping from an address to its size. This area of memory is checked for
/// any object references in the current space which are then copied during a
/// scavenge.
const RememberedSet = std.AutoArrayHashMapUnmanaged([*]u64, usize);
/// A set of objects which should be notified when they are not referenced
/// anymore. See `Space.finalization_set` for more information.
const FinalizationSet = std.AutoArrayHashMapUnmanaged([*]u64, void);
const Space = struct {
    /// A reference back to the heap.
    heap: *Self,

    /// If true, then memory is undefined, and the next time an allocation is
    /// requested the memory space should be allocated.
    lazy_allocate: bool = true,
    /// The size of this memory space.
    size: usize,
    /// The raw memory contents of the space. The space capacity can be learned
    /// with `memory.len`. Uninitialized while `lazy_allocate` is true.
    memory: []u64 = undefined,
    /// A slice of the current object segment in this space.
    object_segment: []u64 = undefined,
    /// A slice of the current byte array segment in this space.
    byte_array_segment: []u64 = undefined,
    /// The set of objects which reference an object in this space. When a
    /// constant or assignable slot from a previous space references this space,
    /// it is added to this set; when it starts referencing another space, it is
    /// removed from this space. During scavenging, this space is cleared and
    /// any references that were still pointing to this space at scavenge time
    /// are transferred to the target space.
    ///
    /// TODO: The original Self VM used "cards" (i.e. a bitmap) to mark certain
    ///       regions of memory as pointing to a new space indiscriminate of
    ///       object size. Figure out whether that is faster than this
    ///       approach.
    remembered_set: RememberedSet,
    /// The finalization set of a space represents the set of objects which
    /// should be notified when they haven't been copied after a scavenge
    /// operation. These objects need to perform additional steps once they are
    /// not scavenged (in other words, not referenced by anyone anymore).
    ///
    ///When an item in this set gets copied to the target space, it is removed
    ///from this set and added to the target set.
    finalization_set: FinalizationSet,
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
    /// The name of this space.
    name: [*:0]const u8,

    /// A link node for a newer generation space to scan in order to update
    /// references from the newer space to the older one.
    const NewerGenerationLink = struct {
        space: *Space,
        previous: ?*const NewerGenerationLink,
    };

    pub fn lazyInit(heap: *Self, comptime name: [*:0]const u8, size: usize) Space {
        return Space{
            .heap = heap,
            .name = name,
            .size = size,
            .remembered_set = .{},
            .finalization_set = .{},
        };
    }

    pub fn init(heap: *Self, allocator: Allocator, comptime name: [*:0]const u8, size: usize) !Space {
        var self = lazyInit(heap, name, size);
        try self.allocateMemory(allocator);
        return self;
    }

    fn allocateMemory(self: *Space, allocator: Allocator) !void {
        var memory = try allocator.alloc(u64, @divExact(self.size, @sizeOf(u64)));

        // REVIEW: When Zig fixes its zero-length arrays to not create a slice
        //         with an undefined address, get rid of this and just use
        //         memory[0..0].
        var runtime_zero: usize = 0;

        self.lazy_allocate = false;
        self.memory = memory;
        self.object_segment = memory[0..runtime_zero];
        self.byte_array_segment = (memory.ptr + memory.len)[0..runtime_zero];

        if (GC_DEBUG) std.debug.print("Heap.init: {s} is {*}-{*}\n", .{ self.name, self.memory.ptr, self.memory.ptr + self.memory.len });
    }

    pub fn deinit(self: *Space, allocator: Allocator) void {
        if (self.lazy_allocate)
            return;

        // Finalize everything that needs to be finalized.
        var finalization_it = self.finalization_set.iterator();
        while (finalization_it.next()) |entry| {
            var object = Value.fromObjectAddress(entry.key_ptr.*).asObject();
            object.finalize(allocator);
        }

        self.finalization_set.deinit(allocator);
        self.remembered_set.deinit(allocator);
        allocator.free(self.memory);
    }

    /// Return the amount of free memory in this space in bytes.
    pub fn freeMemory(self: Space) usize {
        const available_words = self.memory.len - self.object_segment.len - self.byte_array_segment.len;
        return available_words * @sizeOf(u64);
    }

    /// Copy the given address as a new object in the target space. Creates a
    /// forwarding reference in this space; if more than one object was pointing
    /// to this object in the old space, then a special marker is placed in the
    /// location of the old object which tells the future calls of this function
    /// for the same object to just return the new location and avoid copying
    /// again.
    fn copyObjectTo(self: *Space, allocator: Allocator, address: [*]u64, target_space: *Space) ![*]u64 {
        const object = Object.fromAddress(address);
        if (object.isForwarded()) {
            const forward_address = object.getForwardAddress();
            return forward_address;
        }

        const object_size = object.getSizeInMemory();
        std.debug.assert(object_size % @sizeOf(u64) == 0);

        const object_size_in_words = object_size / @sizeOf(u64);
        // We must have enough space at this point.
        const new_address = target_space.allocateInObjectSegment(allocator, object_size) catch unreachable;
        @memcpy(new_address[0..object_size_in_words], address[0..object_size_in_words]);

        // Add this object to the target space's finalization set if it is in
        // ours.
        if (self.finalizationSetContains(address)) {
            self.removeFromFinalizationSet(address) catch unreachable;
            try target_space.addToFinalizationSet(allocator, new_address);
        }

        // Create a forwarding reference
        object.forwardObjectTo(new_address);
        return new_address;
    }

    /// Same as copyObjectTo, but for byte arrays.
    fn copyByteArrayTo(allocator: Allocator, address: [*]u64, target_space: *Space) [*]u64 {
        const byte_array = ByteArray.fromAddress(address);
        const byte_array_size = byte_array.getSizeInMemory();
        std.debug.assert(byte_array_size % @sizeOf(u64) == 0);

        const byte_array_size_in_words = byte_array_size / @sizeOf(u64);
        // We must have enough space at this point.
        const new_address = target_space.allocateInByteArraySegment(allocator, byte_array_size) catch unreachable;
        @memcpy(new_address[0..byte_array_size_in_words], address[0..byte_array_size_in_words]);

        return new_address;
    }

    /// Copy the given address to the target space. If require_copy is true,
    /// panics if the given address wasn't in this space.
    fn copyAddress(self: *Space, allocator: Allocator, address: [*]u64, target_space: *Space, comptime require_copy: bool) !?[*]u64 {
        if (self.objectSegmentContains(address)) {
            return self.copyObjectTo(allocator, address, target_space);
        } else if (self.byteArraySegmentContains(address)) {
            return copyByteArrayTo(allocator, address, target_space);
        } else if (require_copy) {
            std.debug.panic("!!! copyAddress called with an address that's not allocated in this space!", .{});
        }

        return null;
    }

    /// Performs Cheney's algorithm, copying alive objects to the given target.
    pub fn cheneyCommon(
        self: *Space,
        allocator: Allocator,
        target_space: *Space,
        newer_generation_link: ?*const NewerGenerationLink,
    ) Allocator.Error!void {
        // First see if the target space has enough space to potentially take
        // everything in this space.
        const space_size = self.memory.len * @sizeOf(u64);
        const required_size = space_size - self.freeMemory();
        if (required_size > target_space.freeMemory()) {
            if (GC_DEBUG) std.debug.print("Space.cheneyCommon: Target space doesn't have enough memory to hold all of our objects, attempting to perform GC on it\n", .{});

            // Make the other space garbage collect first.
            const my_link = NewerGenerationLink{ .space = self, .previous = newer_generation_link };
            try target_space.collectGarbageInternal(allocator, required_size, &my_link);

            if (space_size - self.freeMemory() > target_space.freeMemory()) {
                std.debug.panic("Even after a garbage collection, the target space doesn't have enough memory for me to perform a scavenge, sorry.", .{});
            }

            if (GC_DEBUG) std.debug.print("Space.cheneyCommon: Target space successfully GC'd, now has {} bytes free\n", .{target_space.freeMemory()});
        }
        // If we got here, then we have enough free memory on the target space
        // to perform our operations.

        // This is saved to race against the target space's object segment
        // length after everything else has been copied in.
        var target_object_segment_index = target_space.object_segment.len;

        // Visit all the values in the VM and copy any addresses.
        const Visitor = struct {
            self: *Space,
            allocator: Allocator,
            target_space: *Space,

            pub fn visit(ctx: @This(), value: *Value) !void {
                if (value.isObjectReference()) {
                    if (try ctx.self.copyAddress(ctx.allocator, value.asObjectAddress(), ctx.target_space, false)) |new_address| {
                        value.* = Value.fromObjectAddress(new_address);
                    }
                }
            }
        };
        try self.heap.visitValues(Visitor{ .self = self, .allocator = allocator, .target_space = target_space });

        {
            var remembered_set_iterator = self.remembered_set.iterator();
            // Go through the remembered set, and copy any referenced objects.
            // Transfer the remembered set objects to the new space if it has a
            // remembered set of its own. (TODO old space shouldn't have one)
            while (remembered_set_iterator.next()) |entry| {
                const start_of_object = entry.key_ptr.*;
                const object_size_in_bytes = entry.value_ptr.*;
                const object_slice = start_of_object[0 .. object_size_in_bytes / @sizeOf(u64)];

                for (object_slice) |*word| {
                    const value = Value{ .data = word.* };
                    if (value.isObjectReference()) {
                        const address = value.asObjectAddress();
                        if (try self.copyAddress(allocator, address, target_space, false)) |new_address| {
                            word.* = Value.fromObjectAddress(new_address).data;
                        }
                    }
                }

                if (!target_space.objectSegmentContains(start_of_object)) {
                    try target_space.addToRememberedSet(allocator, start_of_object, object_size_in_bytes);
                }
            }
        }

        // Go through any memory regions in newer spaces, and copy any
        // referenced objects (and update these newer spaces' remembered sets).
        // This ensures that new->old references are also preserved.
        var newer_generation_link_it = newer_generation_link;
        while (newer_generation_link_it) |link| {
            if (GC_DEBUG) std.debug.print("Space.cheneyCommon: Scanning newer generation {s}\n", .{link.space.name});

            const newer_generation_space = link.space;

            // Update all the addresses in the newer space with the new
            // locations in the target space
            for (newer_generation_space.object_segment) |*word| {
                const value = Value{ .data = word.* };
                if (value.isObjectReference()) {
                    const address = value.asObjectAddress();
                    if (try self.copyAddress(allocator, address, target_space, false)) |new_address| {
                        word.* = Value.fromObjectAddress(new_address).data;
                    }
                }
            }

            newer_generation_link_it = link.previous;
        }

        if (GC_DEBUG) std.debug.print(
            "Space.cheneyCommon: Trying to catch up from object segment offset {} to {}\n",
            .{ target_object_segment_index, target_space.object_segment.len },
        );

        // Try to catch up to the target space's object and byte array cursors,
        // copying any other objects/byte arrays that still exist in this
        // space.
        while (target_object_segment_index < target_space.object_segment.len) : (target_object_segment_index += 1) {
            const word_ptr = &target_space.object_segment[target_object_segment_index];
            const value = Value{ .data = word_ptr.* };

            if (value.isObjectReference()) {
                const address = value.asObjectAddress();

                if (self.objectSegmentContains(address)) {
                    word_ptr.* = Value.fromObjectAddress(try self.copyObjectTo(allocator, address, target_space)).data;
                } else if (self.byteArraySegmentContains(address)) {
                    word_ptr.* = Value.fromObjectAddress(copyByteArrayTo(allocator, address, target_space)).data;
                }
            }
        }

        std.debug.assert(target_object_segment_index == target_space.object_segment.len);

        // Notify any object who didn't make it out of this space and wanted to
        // be notified about being finalized.
        for (self.finalization_set.keys()) |address| {
            const object = Object.fromAddress(address);
            object.finalize(allocator);
        }

        // Go through all the newer generations again, and remove or replace all
        // the items in the remembered set which point to this space.
        //
        // NOTE: Must happen AFTER all copying of objects is finished, as we
        //       need to know which objects survived the scavenge and which ones
        //       did not - the ones which didn't survive the scavenge will
        //       simply be removed from the remembered set, while the ones which
        //       did must be replaced with the new object in the target space.
        newer_generation_link_it = newer_generation_link;
        while (newer_generation_link_it) |link| {
            if (GC_DEBUG) std.debug.print("Space.cheneyCommon: Updating remembered set of newer generation {s}\n", .{link.space.name});

            const newer_generation_space = link.space;
            // NOTE: Must create a copy, as we're modifying entries in the
            //       remembered set
            var remembered_set_copy = try newer_generation_space.remembered_set.clone(allocator);
            defer remembered_set_copy.deinit(allocator);

            var remembered_set_iterator = remembered_set_copy.iterator();
            while (remembered_set_iterator.next()) |entry| {
                const object_address = entry.key_ptr.*;
                const object_size = entry.value_ptr.*;

                if (self.objectSegmentContains(object_address)) {
                    const object = Object.fromAddress(object_address);

                    if (object.isForwarded()) {
                        // Yes, the object's been copied. Replace the entry in
                        // the remembered set.
                        const new_address = object.getForwardAddress();
                        newer_generation_space.removeFromRememberedSet(object_address) catch unreachable;
                        // Hopefully the object size has not changed somehow
                        // during a GC. :^)
                        try newer_generation_space.addToRememberedSet(allocator, new_address, object_size);
                    } else {
                        // No, the object didn't survive the scavenge.
                        newer_generation_space.removeFromRememberedSet(object_address) catch unreachable;
                    }
                }
            }

            newer_generation_link_it = link.previous;
        }

        // REVIEW: When Zig fixes its zero-length arrays to not create a slice
        //         with an undefined address, get rid of this and just use
        //         memory[0..0].
        var runtime_zero: usize = 0;

        // Reset this space's segments, tracked set, remembered set and
        // finalization set, as it is now effectively empty.
        self.object_segment = self.memory[0..runtime_zero];
        self.byte_array_segment = (self.memory.ptr + self.memory.len)[0..runtime_zero];
        self.remembered_set.clearRetainingCapacity();
        self.finalization_set.clearRetainingCapacity();
    }

    /// Performs a garbage collection operation on this space.
    ///
    /// This method performs either a scavenge or a tenure; if a scavenge target
    /// is defined for this space, it first attempts a scavenge. If not enough
    /// memory is cleaned up by a scavenge operation, or there wasn't a defined
    /// scavenge target, a *tenure* is attempted towards the tenure target of
    /// this space. If a tenure target is not specified either, then the heap is
    /// simply expanded to accomodate the new objects.
    pub fn collectGarbage(self: *Space, allocator: Allocator, required_memory: usize) !void {
        try self.collectGarbageInternal(allocator, required_memory, null);
    }

    /// Performs the actual operation as described in collectGarbage. Takes an
    /// additional memory_link argument, which describes a set of memory areas
    /// to scan after a garbage collection is done for stale references.
    fn collectGarbageInternal(
        self: *Space,
        allocator: Allocator,
        required_memory: usize,
        newer_generation_link: ?*const NewerGenerationLink,
    ) !void {
        // See if we already have the required memory amount.
        if (self.freeMemory() >= required_memory) return;

        if (GC_DEBUG) std.debug.print("Space.collectGarbage: Attempting to garbage collect in {s}\n", .{self.name});

        // See if we can perform a scavenge first.
        if (self.scavenge_target) |scavenge_target| {
            if (GC_DEBUG) std.debug.print("Space.collectGarbage: Attempting to perform a scavenge to my scavenge target, {s}\n", .{scavenge_target.name});
            try self.cheneyCommon(allocator, scavenge_target, newer_generation_link);
            self.swapMemoryWith(scavenge_target);

            if (self.freeMemory() >= required_memory) {
                if (GC_DEBUG) std.debug.print("Space.collectGarbage: Scavenging was sufficient. {} bytes now free in {s}.\n", .{ self.freeMemory(), self.name });
                return;
            }
        }

        // Looks like the scavenge didn't give us enough memory. Let's attempt a
        // tenure.
        if (self.tenure_target) |tenure_target| {
            if (GC_DEBUG) std.debug.print("Space.collectGarbage: Attempting to tenure to {s}\n", .{tenure_target.name});
            try self.cheneyCommon(allocator, tenure_target, newer_generation_link);

            // FIXME: Return an error instead of panicing when the allocation
            //        is too large for this space. How should we handle large
            //        allocations anyway?
            if (self.freeMemory() < required_memory) {
                std.debug.panic("!!! Could not free enough space in {s} even after tenuring! ({} bytes free, {} required)\n", .{ self.name, self.freeMemory(), required_memory });
            }

            if (GC_DEBUG) {
                const tenure_target_previous_free_memory: isize = @intCast(tenure_target.freeMemory());
                const tenure_target_current_free_memory: isize = @intCast(tenure_target.freeMemory());
                const free_memory_diff = tenure_target_previous_free_memory - tenure_target_current_free_memory;

                std.debug.print("Space.collectGarbage: Successfully tenured, {} bytes now free in {s}.\n", .{ self.freeMemory(), self.name });
                if (free_memory_diff < 0) {
                    std.debug.print("Space.collectGarbage: (Tenure target {s} LOST {} bytes, target did a GC?)\n", .{ tenure_target.name, -free_memory_diff });
                } else {
                    std.debug.print("Space.collectGarbage: (Tenure target {s} gained {} bytes)\n", .{ tenure_target.name, free_memory_diff });
                }
            }
            return;
        }

        if (GC_TRACK_SOURCE_DEBUG) {
            self.heap.printTrackLocationCounts();
        }

        std.debug.panic("TODO expanding a space which doesn't have a tenure or scavenge target", .{});
    }

    fn swapMemoryWith(self: *Space, target_space: *Space) void {
        std.mem.swap([]u64, &self.memory, &target_space.memory);
        std.mem.swap([]u64, &self.object_segment, &target_space.object_segment);
        std.mem.swap([]u64, &self.byte_array_segment, &target_space.byte_array_segment);
        std.mem.swap(RememberedSet, &self.remembered_set, &target_space.remembered_set);
        std.mem.swap(FinalizationSet, &self.finalization_set, &target_space.finalization_set);
    }

    fn objectSegmentContains(self: *Space, address: [*]u64) bool {
        const start_of_object_segment = @intFromPtr(self.object_segment.ptr);
        const end_of_object_segment = @intFromPtr(self.object_segment.ptr + self.object_segment.len);
        const address_value = @intFromPtr(address);

        return address_value >= start_of_object_segment and address_value < end_of_object_segment;
    }

    fn byteArraySegmentContains(self: *Space, address: [*]u64) bool {
        const start_of_byte_array_segment = @intFromPtr(self.byte_array_segment.ptr);
        const end_of_byte_array_segment = @intFromPtr(self.byte_array_segment.ptr + self.byte_array_segment.len);
        const address_value = @intFromPtr(address);

        return address_value >= start_of_byte_array_segment and address_value < end_of_byte_array_segment;
    }

    /// Allocates the requested amount in bytes in the object segment of this
    /// space. Panics if there isn't enough memory.
    fn allocateInObjectSegment(self: *Space, allocator: Allocator, size: usize) ![*]u64 {
        if (self.lazy_allocate)
            try self.allocateMemory(allocator);

        const size_in_words = @divExact(size, @sizeOf(u64));
        const current_object_segment_offset = self.object_segment.len;
        self.object_segment.len += size_in_words;
        const start_of_object: [*]u64 = @ptrCast(&self.object_segment[current_object_segment_offset]);

        if (builtin.mode == .Debug) {
            const start_of_object_aligned: [*]align(@alignOf(u64)) u8 = @ptrCast(start_of_object);
            @memset(start_of_object_aligned[0..size], UninitializedHeapScrubByte);
        }

        return start_of_object;
    }

    /// Allocates the requested amount in bytes in the byte array segment of
    /// this space. Panics if there isn't enough memory.
    fn allocateInByteArraySegment(self: *Space, allocator: Allocator, size: usize) ![*]u64 {
        if (self.lazy_allocate)
            try self.allocateMemory(allocator);

        const size_in_words = @divExact(size, @sizeOf(u64));
        self.byte_array_segment.ptr -= size_in_words;
        self.byte_array_segment.len += size_in_words;

        if (builtin.mode == .Debug) {
            const array_segment_ptr_aligned: [*]align(@alignOf(u64)) u8 = @ptrCast(self.byte_array_segment.ptr);
            @memset(array_segment_ptr_aligned[0..size], UninitializedHeapScrubByte);
        }

        return self.byte_array_segment.ptr;
    }

    pub fn allocateInSegment(self: *Space, allocator: Allocator, segment: Segment, size: usize) Allocator.Error![*]u64 {
        return switch (segment) {
            .Object => self.allocateInObjectSegment(allocator, size),
            .ByteArray => self.allocateInByteArraySegment(allocator, size),
        };
    }

    /// Allocates the requested amount of bytes in the appropriate segment of
    /// this space.
    /// Adds the given address to the finalization set of this space.
    pub fn addToFinalizationSet(self: *Space, allocator: Allocator, address: [*]u64) !void {
        try self.finalization_set.put(allocator, address, {});
    }

    /// Returns whether the finalization set contains the given address.
    pub fn finalizationSetContains(self: *Space, address: [*]u64) bool {
        return self.finalization_set.contains(address);
    }

    pub const RemoveFromFinalizationSetError = error{AddressNotInFinalizationSet};
    /// Removes the given address from the finalization set of this space.
    /// Returns AddressNotInFinalizationSet if the address was not in the
    /// finalization set.
    pub fn removeFromFinalizationSet(self: *Space, address: [*]u64) !void {
        if (!self.finalization_set.swapRemove(address)) {
            return RemoveFromFinalizationSetError.AddressNotInFinalizationSet;
        }
    }

    /// Adds the given address into the remembered set of this space.
    pub fn addToRememberedSet(self: *Space, allocator: Allocator, address: [*]u64, size: usize) !void {
        try self.remembered_set.put(allocator, address, size);
    }

    pub const RemoveFromRememberedSetError = error{AddressNotInRememberedSet};
    /// Removes the given address from the remembered set of this space. Returns
    /// AddressNotInRememberedSet if the address was not in the remembered set.
    pub fn removeFromRememberedSet(self: *Space, address: [*]u64) !void {
        if (!self.remembered_set.swapRemove(address)) {
            return RemoveFromRememberedSetError.AddressNotInRememberedSet;
        }
    }

    pub fn updateAllReferencesToImpl(
        self: *Space,
        allocator: Allocator,
        old_address: [*]u64,
        new_address: [*]u64,
    ) !void {
        if (self.lazy_allocate) {
            // This space hasn't been allocated yet, so there's no reason to
            // check it.
            return;
        }

        const new_address_value = Value.fromObjectAddress(new_address);
        const old_address_as_reference = Value.fromObjectAddress(old_address).data;
        const new_address_as_reference = new_address_value.data;

        for (self.object_segment) |*word| {
            if (word.* == old_address_as_reference)
                word.* = new_address_as_reference;
        }

        // If the new object is in the current space and should be finalized,
        // then put it in the finalization set.
        if (self.objectSegmentContains(new_address) and new_address_value.asObject().canFinalize())
            try self.finalization_set.put(allocator, new_address, {});
    }

    /// Update all references to the given object in the current space and
    /// its tenure target.
    pub fn updateAllReferencesTo(
        self: *Space,
        allocator: Allocator,
        old_address: [*]u64,
        new_address: [*]u64,
    ) Allocator.Error!void {
        try self.updateAllReferencesToImpl(allocator, old_address, new_address);

        if (self.tenure_target) |tenure_target| {
            try tenure_target.updateAllReferencesTo(allocator, old_address, new_address);
        }
    }
};

/// A tracked heap value. This value is updated whenever garbage collection
/// occurs and the object moves.
pub const Tracked = struct {
    value: union(enum) {
        Object: *[*]u64,
        Literal: Value,
    },

    pub fn createWithObject(handle: *[*]u64) Tracked {
        return Tracked{ .value = .{ .Object = handle } };
    }

    pub fn createWithLiteral(value: Value) Tracked {
        std.debug.assert(!value.isObjectReference());
        return Tracked{ .value = .{ .Literal = value } };
    }

    pub fn untrack(self: Tracked, heap: *Self) void {
        if (self.value == .Object) {
            heap.untrack(self);
        }
    }

    pub fn getValue(self: Tracked) Value {
        return switch (self.value) {
            .Object => |t| Value.fromObjectAddress(t.*),
            .Literal => |t| t,
        };
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
    try std.testing.expectEqual(heap.from_space.memory.ptr, heap.from_space.object_segment.ptr);
}

test "link an object to another and perform scavenge" {
    const allocator = std.testing.allocator;

    var heap = try Self.create(allocator);
    defer heap.destroy();

    // The object being referenced
    var referenced_object_map = try Object.Map.Slots.create(heap, 1);
    var actual_name = try ByteArray.createFromString(heap, "actual");
    referenced_object_map.getSlots()[0].initConstant(actual_name, .NotParent, Value.fromUnsignedInteger(0xDEADBEEF));
    var referenced_object = try Object.Slots.create(heap, referenced_object_map, &[_]Value{});

    // The "activation object", which is how we get a reference to the object in
    // the from space after the tenure is done
    var activation_object_map = try Object.Map.Slots.create(heap, 1);
    var reference_name = try ByteArray.createFromString(heap, "reference");
    activation_object_map.getSlots()[0].initMutable(Object.Map.Slots, activation_object_map, reference_name, .NotParent);
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
    try std.testing.expectEqualStrings("reference", new_activation_object_map.getSlots()[0].name.asByteArray().getValues());

    // Find the new referenced object
    var new_referenced_object = new_activation_object.getAssignableSlotValueByName("reference").?.asObject().asSlotsObject();
    try std.testing.expect(referenced_object != new_referenced_object);
    var new_referenced_object_map = new_referenced_object.getMap();
    try std.testing.expect(referenced_object_map != new_referenced_object_map);
    try std.testing.expect(referenced_object_map.getSlots()[0].name.asObjectAddress() != new_referenced_object_map.getSlots()[0].name.asObjectAddress());
    try std.testing.expectEqualStrings("actual", new_referenced_object_map.getSlots()[0].name.asByteArray().getValues());

    // Verify that the map map is shared (aka forwarding addresses work)
    try std.testing.expectEqual(
        new_activation_object_map.map.object.getMap(),
        new_referenced_object_map.map.object.getMap(),
    );

    // Get the value we stored and compare it
    var referenced_object_value = new_referenced_object.getMap().getSlotByName("actual").?.value;
    try std.testing.expectEqual(@as(u64, 0xDEADBEEF), referenced_object_value.asUnsignedInteger());
}
