// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const ByteArray = @import("../ByteArray.zig");
const stage2_compat = @import("../../utility/stage2_compat.zig");

// FIXME: This isn't thread safe!
var singleton_byte_array_map: ?Heap.Tracked = null;

fn getOrCreateByteArrayMap(token: *Heap.AllocationToken) !Value {
    if (singleton_byte_array_map) |map| return map.getValue();

    const map = Object.Map.Slots.create(token, 0);
    singleton_byte_array_map = try token.heap.track(map.asValue());
    return map.asValue();
}

fn requiredSizeForByteArrayMap() usize {
    return if (singleton_byte_array_map != null) 0 else Object.Map.Slots.requiredSizeForAllocation(0);
}

pub const ByteArrayObject = extern struct {
    header: Object.Header,
    byte_array: Value,

    pub const Ptr = stage2_compat.HeapPtr(ByteArrayObject, .Mutable);

    /// Create an initialized byte array object from the given values.
    pub fn createWithValues(token: *Heap.AllocationToken, actor_id: u31, values: []const u8) !ByteArrayObject.Ptr {
        const self = try createUninitialized(token, actor_id, values.len);
        std.mem.copy(u8, self.getValues(), values);
        return self;
    }

    /// Create an uninitialized byte array object with the given size.
    pub fn createUninitialized(token: *Heap.AllocationToken, actor_id: u31, size: usize) !ByteArrayObject.Ptr {
        const byte_array = ByteArray.createUninitialized(token, size);
        return try create(token, actor_id, byte_array);
    }

    /// Create a byte array object with an existing byte array.
    pub fn create(token: *Heap.AllocationToken, actor_id: u31, byte_array: ByteArray) !ByteArrayObject.Ptr {
        const byte_array_map = try getOrCreateByteArrayMap(token);

        const size = requiredSizeForAllocation(null);
        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(ByteArrayObject.Ptr, memory_area);
        self.init(actor_id, byte_array_map, byte_array);

        return self;
    }

    fn init(self: ByteArrayObject.Ptr, actor_id: u31, map: Value, byte_array: ByteArray) void {
        self.header.init(.ByteArray, actor_id, map);
        self.byte_array = byte_array.asValue();
    }

    pub fn asObjectAddress(self: ByteArrayObject.Ptr) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: ByteArrayObject.Ptr) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getValues(self: ByteArrayObject.Ptr) []u8 {
        return self.getByteArray().getValues();
    }

    pub fn getLength(self: ByteArrayObject.Ptr) u64 {
        return self.getByteArray().header.length.get();
    }

    pub fn getByteArray(self: ByteArrayObject.Ptr) ByteArray {
        return self.byte_array.asByteArray();
    }

    pub fn clone(self: ByteArrayObject.Ptr, token: *Heap.AllocationToken, actor_id: u31) ByteArrayObject.Ptr {
        // NOTE: By the time we reach here the byte array map will have been
        //       allocated already, so there's no path in which we can error
        //       without a panic.
        return createWithValues(token, actor_id, self.getValues()) catch unreachable;
    }

    pub fn getSizeInMemory(self: ByteArrayObject.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation(null);
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
