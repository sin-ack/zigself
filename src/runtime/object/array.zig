// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const stage2_compat = @import("../../utility/stage2_compat.zig");

pub const ArrayObject = extern struct {
    header: Object.Header align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(ArrayObject, .Mutable);

    /// Create a new array with the given values and filling extra items with
    /// the filler value. If filler value is null, expects values to be at least
    /// as long as the size described in the map. If values is longer than the
    /// size N specified in the map, copies the first N items.
    pub fn createWithValues(token: *Heap.AllocationToken, actor_id: u31, map: Object.Map.Array.Ptr, values: []Value, filler: ?Value) ArrayObject.Ptr {
        if (filler == null and values.len < map.getSize()) {
            std.debug.panic(
                "!!! Array.createWithValues given values slice that's too short, and no filler was given!",
                .{},
            );
        }

        const size = requiredSizeForAllocation(map.getSize());

        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(ArrayObject.Ptr, memory_area);
        self.init(actor_id, map, values, filler);

        return self;
    }

    fn init(self: ArrayObject.Ptr, actor_id: u31, map: *Object.Map.Array, values: []Value, filler: ?Value) void {
        self.header.init(.Array, actor_id, map.asValue());

        const values_to_copy = values[0..std.math.min(values.len, map.getSize())];
        std.mem.copy(Value, self.getValues(), values_to_copy);

        if (map.getSize() > values.len) {
            std.mem.set(Value, self.getValues()[values.len..], filler.?);
        }
    }

    pub fn asObjectAddress(self: ArrayObject.Ptr) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: ArrayObject.Ptr) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getMap(self: ArrayObject.Ptr) Object.Map.Array.Ptr {
        return self.header.getMap().asArrayMap();
    }

    pub fn getSize(self: ArrayObject.Ptr) usize {
        return self.getMap().getSize();
    }

    pub fn getValues(self: ArrayObject.Ptr) []Value {
        const object_memory = @ptrCast([*]u8, self);
        const start_of_items = object_memory + @sizeOf(ArrayObject);

        return std.mem.bytesAsSlice(Value, start_of_items[0 .. self.getSize() * @sizeOf(Value)]);
    }

    pub fn clone(self: ArrayObject.Ptr, token: *Heap.AllocationToken, actor_id: u31) ArrayObject.Ptr {
        return createWithValues(token, actor_id, self.getMap(), self.getValues(), null);
    }

    pub fn getSizeInMemory(self: ArrayObject.Ptr) usize {
        return requiredSizeForAllocation(self.getSize());
    }

    pub fn requiredSizeForAllocation(size: usize) usize {
        return @sizeOf(ArrayObject) + size * @sizeOf(Value);
    }
};
