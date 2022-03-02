// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig");
const ByteArray = @import("./byte_array.zig");

/// The allocator object that will be used throughout the virtual machine's
/// lifetime.
allocator: Allocator,
/// The object heap.
heap: *Heap,
/// A mapping from argument counts to the related block message names.
/// Since block names will not be unique, this mapping allows us to store
/// a single instance of each message name for the respective block arities.
block_message_names: std.AutoArrayHashMapUnmanaged(u8, Heap.Tracked),

// References to global objects

/// The root of the current Self world.
lobby_object: Heap.Tracked,

/// The global nil object used to represent the default assignable slot value.
/// Can also be used in place of "nothing".
global_nil: Heap.Tracked,
/// The global truth value.
global_true: Heap.Tracked,
/// The global falsity value.
global_false: Heap.Tracked,

// Primitive object traits

array_traits: Heap.Tracked,
block_traits: Heap.Tracked,
float_traits: Heap.Tracked,
string_traits: Heap.Tracked,
integer_traits: Heap.Tracked,

const Self = @This();

/// Creates the virtual machine, including the heap and the global objects.
pub fn create(allocator: Allocator) !*Self {
    var heap = try Heap.create(allocator);
    errdefer heap.destroy();

    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    self.allocator = allocator;
    self.heap = heap;
    self.block_message_names = .{};

    const empty_map = try Object.Map.Slots.create(heap, 0);

    self.lobby_object = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());

    self.global_nil = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.global_true = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.global_false = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());

    self.array_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.block_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.float_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.string_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.integer_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());

    return self;
}

pub fn destroy(self: *Self) void {
    self.lobby_object.untrack(self.heap);
    self.global_nil.untrack(self.heap);
    self.global_true.untrack(self.heap);
    self.global_false.untrack(self.heap);
    self.array_traits.untrack(self.heap);
    self.block_traits.untrack(self.heap);
    self.float_traits.untrack(self.heap);
    self.string_traits.untrack(self.heap);
    self.integer_traits.untrack(self.heap);

    {
        var it = self.block_message_names.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.untrack(self.heap);
        }
    }
    self.block_message_names.deinit(self.allocator);

    self.heap.destroy();
    self.allocator.destroy(self);
}

pub fn getTrue(self: Self) Value {
    return self.global_true.getValue();
}

pub fn getFalse(self: Self) Value {
    return self.global_false.getValue();
}

pub fn nil(self: Self) Value {
    return self.global_nil.getValue();
}

pub fn lobby(self: Self) Value {
    return self.lobby_object.getValue();
}

/// Return a block message name with the given argument count, creating it
/// if it does not exist.
///
/// A block message name looks like: `value:With:With:With:...`.
pub fn getOrCreateBlockMessageName(self: *Self, argument_count: u8) !Heap.Tracked {
    const result = try self.block_message_names.getOrPut(self.allocator, argument_count);
    if (result.found_existing) {
        return result.value_ptr.*;
    } else {
        const byte_array = try ByteArray.createUninitialized(self.heap, requiredSizeForBlockMessageName(argument_count));
        writeBlockMessageName(byte_array.getValues(), argument_count);

        const tracked_value = try self.heap.track(byte_array.asValue());
        result.value_ptr.* = tracked_value;
        return tracked_value;
    }
}

fn writeBlockMessageName(name: []u8, argument_count: u8) void {
    std.debug.assert(name.len == requiredSizeForBlockMessageName(argument_count));
    std.mem.copy(u8, name, "value");

    if (argument_count > 0) {
        name[5] = ':';

        var remaining_buffer = name[6..];
        while (remaining_buffer.len > 0) {
            std.mem.copy(u8, remaining_buffer, "With:");
            remaining_buffer = remaining_buffer[5..];
        }
    }
}

fn requiredSizeForBlockMessageName(argument_count: u8) usize {
    var needed_space: usize = 5; // value
    if (argument_count > 0) {
        needed_space += 1; // :
        needed_space += 5 * (argument_count - 1); // Any other With:s needed
    }

    return needed_space;
}
