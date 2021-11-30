// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Map = Object.Map;
const hash = @import("../../utility/hash.zig");
const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");

/// A slots object. A slots object does not contain all the slots that are
/// actually in the object; for that, the map must be consulted. The slot
/// object begins with a header followed by Values for each of the
/// assignable slots.
pub const Slots = packed struct {
    header: Object.Header,

    pub fn create(heap: *Heap, map: *Map.Slots, assignable_slot_values: []Value) !*Slots {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Slots, memory_area);
        self.init(map, assignable_slot_values);

        return self;
    }

    fn init(self: *Slots, map: *Map.Slots, assignable_slot_values: []Value) void {
        self.header.init(.Slots, map.asValue());
        std.mem.copy(Value, self.getAssignableSlotValues(), assignable_slot_values);
    }

    pub fn getMap(self: *Slots) *Map.Slots {
        return self.header.getMap().asSlotsMap();
    }

    pub fn getSizeInMemory(self: *Slots) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn asValue(self: *Slots) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    /// Returns a slice of `Value`s for the assignable slots that are after
    pub fn getAssignableSlotValues(self: *Slots) []Value {
        const slots_header_size = @sizeOf(Slots);
        const object_memory = @ptrCast([*]u8, self);
        const assignable_slot_count = self.getMap().getAssignableSlotCount();

        return std.mem.bytesAsSlice(
            Value,
            object_memory[slots_header_size .. slots_header_size + assignable_slot_count * @sizeOf(Value)],
        );
    }

    /// Attempts to find the pointer to the `Value` for the assignable slot
    /// for the given hash. If the assignable slot does not exist, null is
    /// returned.
    pub fn getAssignableSlotValueByHash(self: *Slots, hash_value: u32) ?*Value {
        var values = self.getAssignableSlotValues();
        var cursor: usize = 0;

        for (self.getMap().getSlots()) |slot| {
            if (slot.isMutable()) {
                if (slot.hash == hash_value) {
                    return &values[cursor];
                } else {
                    cursor += 1;
                }
            }
        }

        return null;
    }

    /// Attempts to find the pointer to the `Value` for the assignable slot
    /// with the given name. If the assignable slot does not exist, null is
    /// returned.
    pub fn getAssignableSlotValueByName(self: *Slots, name: []const u8) ?*Value {
        const name_hash = hash.stringHash(name);
        return self.getAssignableSlotValueByHash(name_hash);
    }

    fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return @sizeOf(Object.Header) + assignable_slot_count * @sizeOf(Value);
    }
};
