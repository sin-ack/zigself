// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");

pub const ByteVectorObject = packed struct {
    header: Object.Header,

    pub fn create(heap: *Heap, map: *Object.Map.ByteVector) !*ByteVectorObject {
        const size = requiredSizeForAllocation();

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*ByteVectorObject, memory_area);
        self.init(map.asValue());

        return self;
    }

    fn init(self: *ByteVectorObject, map: Value) void {
        self.header.init(.ByteVector, map);
    }

    pub fn asValue(self: *ByteVectorObject) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getValues(self: *ByteVectorObject) []u8 {
        return self.header.getMap().asByteVectorMap().getValues();
    }

    pub fn getSizeInMemory(self: *ByteVectorObject) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ByteVectorObject);
    }
};
