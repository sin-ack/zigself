// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");

pub const VectorObject = packed struct {
    header: Object.Header,

    /// Create a new vector with the given values and filling extra items with
    /// the filler value. If filler value is null, expects values to be at least
    /// as long as the size described in the map. If values is longer than the
    /// size N specified in the map, copies the first N items.
    pub fn createWithValues(heap: *Heap, map: *Object.Map.Vector, values: []Value, filler: ?Value) !*VectorObject {
        if (filler == null and values.len < map.getSize()) {
            std.debug.panic(
                "!!! Vector.createWithValues given values slice that's too short, and no filler was given!",
                .{},
            );
        }

        const size = requiredSizeForAllocation(map.getSize());

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*VectorObject, memory_area);
        self.init(map, values, filler);

        return self;
    }

    fn init(self: *VectorObject, map: *Object.Map.Vector, values: []Value, filler: ?Value) void {
        self.header.init(.Vector, map.asValue());

        const values_to_copy = values[0..std.math.min(values.len, map.getSize())];
        std.mem.copy(Value, self.getValues(), values_to_copy);

        if (map.getSize() > values.len) {
            std.mem.set(Value, self.getValues()[values.len..], filler.?);
        }
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

    pub fn getSize(self: *VectorObject) usize {
        return self.getMap().getSize();
    }

    pub fn getValues(self: *VectorObject) []Value {
        const object_memory = @ptrCast([*]u8, self);
        const start_of_items = object_memory + @sizeOf(VectorObject);

        return std.mem.bytesAsSlice(Value, start_of_items[0 .. self.getSize() * @sizeOf(Value)]);
    }

    pub fn clone(self: *VectorObject, heap: *Heap) !*VectorObject {
        return createWithValues(heap, self.getMap(), self.getValues(), null);
    }

    pub fn getSizeInMemory(self: *VectorObject) usize {
        return requiredSizeForAllocation(self.getSize());
    }

    pub fn requiredSizeForAllocation(size: usize) usize {
        return @sizeOf(VectorObject) + size * @sizeOf(Value);
    }
};
