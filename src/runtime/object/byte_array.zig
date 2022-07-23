// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const ByteArray = @import("../ByteArray.zig");

pub const ByteArrayObject = packed struct {
    header: Object.Header,

    pub fn create(heap: *Heap, actor_id: u31, map: *Object.Map.ByteArray) !*ByteArrayObject {
        const size = requiredSizeForAllocation();

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*ByteArrayObject, memory_area);
        self.init(actor_id, map.asValue());

        return self;
    }

    fn init(self: *ByteArrayObject, actor_id: u31, map: Value) void {
        self.header.init(.ByteArray, actor_id, map);
    }

    pub fn asObjectAddress(self: *ByteArrayObject) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *ByteArrayObject) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getMap(self: *ByteArrayObject) *Object.Map.ByteArray {
        return self.header.getMap().asByteArrayMap();
    }

    pub fn getValues(self: *ByteArrayObject) []u8 {
        return self.header.getMap().asByteArrayMap().getValues();
    }

    pub fn getByteArray(self: *ByteArrayObject) ByteArray {
        return self.getMap().getByteArray();
    }

    pub fn clone(self: *ByteArrayObject, heap: *Heap, actor_id: u31) !*ByteArrayObject {
        return create(heap, actor_id, self.getMap());
    }

    pub fn getSizeInMemory(self: *ByteArrayObject) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ByteArrayObject);
    }
};
