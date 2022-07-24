// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const ByteArray = @import("../ByteArray.zig");

// FIXME: This isn't thread safe!
var singleton_byte_array_map: ?Heap.Tracked = null;

fn getOrCreateByteArrayMap(heap: *Heap) !Value {
    if (singleton_byte_array_map) |map| return map.getValue();

    const map = try Object.Map.Slots.create(heap, 0);
    singleton_byte_array_map = try heap.track(map.asValue());
    return map.asValue();
}

fn requiredSizeForByteArrayMap() usize {
    return if (singleton_byte_array_map != null) 0 else Object.Map.Slots.requiredSizeForAllocation(0);
}

pub const ByteArrayObject = packed struct {
    header: Object.Header,
    byte_array: Value,

    /// Create an initialized byte array object from the given values.
    pub fn createWithValues(heap: *Heap, actor_id: u31, values: []const u8) !*ByteArrayObject {
        const self = try createUninitialized(heap, actor_id, values.len);
        std.mem.copy(u8, self.getValues(), values);
        return self;
    }

    /// Create an uninitialized byte array object with the given size.
    pub fn createUninitialized(heap: *Heap, actor_id: u31, size: usize) !*ByteArrayObject {
        const byte_array = try ByteArray.createUninitialized(heap, size);
        return try create(heap, actor_id, byte_array);
    }

    /// Create a byte array object with an existing byte array.
    pub fn create(heap: *Heap, actor_id: u31, byte_array: ByteArray) !*ByteArrayObject {
        const byte_array_map = try getOrCreateByteArrayMap(heap);

        const size = requiredSizeForAllocation(null);
        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*ByteArrayObject, memory_area);
        self.init(actor_id, byte_array_map, byte_array);

        return self;
    }

    fn init(self: *ByteArrayObject, actor_id: u31, map: Value, byte_array: ByteArray) void {
        self.header.init(.ByteArray, actor_id, map);
        self.byte_array = byte_array.asValue();
    }

    pub fn asObjectAddress(self: *ByteArrayObject) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *ByteArrayObject) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getValues(self: *ByteArrayObject) []u8 {
        return self.getByteArray().getValues();
    }

    pub fn getLength(self: *ByteArrayObject) u64 {
        return self.getByteArray().header.length.get();
    }

    pub fn getByteArray(self: *ByteArrayObject) ByteArray {
        return self.byte_array.asByteArray();
    }

    pub fn clone(self: *ByteArrayObject, heap: *Heap, actor_id: u31) !*ByteArrayObject {
        return createWithValues(heap, actor_id, self.getValues());
    }

    pub fn getSizeInMemory(self: *ByteArrayObject) usize {
        _ = self;
        return requiredSizeForAllocation(self.getLength());
    }

    /// Call with null if the byte array object has already been allocated.
    pub fn requiredSizeForAllocation(length: ?usize) usize {
        const required_size = requiredSizeForByteArrayMap() + @sizeOf(ByteArrayObject);

        if (length) |l| {
            return required_size + ByteArray.requiredSizeForAllocation(l);
        }

        return required_size;
    }
};
