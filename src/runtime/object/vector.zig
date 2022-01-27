// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");

pub const VectorObject = packed struct {
    header: Object.Header,

    pub fn create(heap: *Heap, map: *Object.Map.Vector) !*VectorObject {
        const size = requiredSizeForAllocation();

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*VectorObject, memory_area);
        self.init(map.asValue());

        return self;
    }

    fn init(self: *VectorObject, map: Value) void {
        self.header.init(.Vector, map);
    }

    pub fn asObjectAddress(self: *VectorObject) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *VectorObject) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getMap(self: *VectorObject) *Object.Map.Vector {
        return self.header.getMap().asVectorMap();
    }

    pub fn getValues(self: *VectorObject) []Value {
        return self.header.getMap().asVectorMap().getValues();
    }

    pub fn clone(self: *VectorObject, heap: *Heap) !*VectorObject {
        return create(heap, self.getMap());
    }

    pub fn getSizeInMemory(self: *VectorObject) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(VectorObject);
    }
};
