// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const AstGen = @import("../AstGen.zig");
const Slot = @import("../slot.zig").Slot;
const Value = @import("../value.zig").Value;

pub const AssignableSlotValues = std.BoundedArray(Heap.Tracked, AstGen.MaximumAssignableSlots);

/// This struct allows one to build out a map's slots and eventually construct
/// an object using it. It holds the assignable slot values and assigns an index
/// to them. It additionally tracks argument slot offsets.
///
/// The MapBuilder takes an initialized but un-filled map as its initial value
/// (as it cannot predict what values a map might take to initialize). It then
/// accepts slots until the map's slot size is reached.
///
/// Finally one may obtain an object with the stored assignable slots attached
/// to it using createObject().
pub fn MapBuilder(comptime MapType: type, comptime ObjectType: type) type {
    return struct {
        heap: *Heap,
        map: Heap.Tracked,
        assignable_slot_values: AssignableSlotValues,

        slot_index: usize = 0,
        assignable_slot_index: usize = 0,
        argument_slot_index: usize = 0,

        const Self = @This();

        // Marker for typechecking.
        pub const is_map_builder = true;

        pub fn init(heap: *Heap, map: *MapType) !Self {
            const tracked_map = try heap.track(map.asValue());

            return Self{
                .heap = heap,
                .map = tracked_map,
                .assignable_slot_values = AssignableSlotValues.init(0) catch unreachable,
            };
        }

        pub fn deinit(self: Self) void {
            for (self.assignable_slot_values.slice()) |value| {
                value.untrack(self.heap);
            }
            self.map.untrack(self.heap);
        }

        pub fn addSlot(self: *Self, slot: Slot) !void {
            const map = @ptrCast(*MapType, self.map.getValue().asObjectAddress());
            try slot.writeContentsTo(
                self.heap,
                map.getSlots(),
                &self.assignable_slot_values,
                &self.slot_index,
                &self.assignable_slot_index,
                &self.argument_slot_index,
            );

            // NOTE: Method and block maps do not count the argument slot count
            //       towards their assignable slot counts, because the argument
            //       slot values don't exist on the method and block objects.
            map.setAssignableSlotCount(@intCast(u8, self.assignable_slot_index));
        }

        pub fn createObject(self: *Self) !*ObjectType {
            try self.heap.ensureSpaceInEden(
                ObjectType.requiredSizeForAllocation(@intCast(u8, self.assignable_slot_index)),
            );

            var slot_values: [AstGen.MaximumAssignableSlots]Value = undefined;
            const slot_values_slice = slot_values[0..self.assignable_slot_index];
            self.writeAssignableSlotValuesTo(slot_values_slice);

            return try ObjectType.create(
                self.heap,
                @ptrCast(*MapType, self.map.getValue().asObjectAddress()),
                slot_values_slice,
            );
        }

        /// Write the current assignable slots to the given assignable slots slice.
        /// Assumes that a garbage collection will not happen before the values'
        /// use.
        pub fn writeAssignableSlotValuesTo(self: *Self, slot_values: []Value) void {
            for (self.assignable_slot_values.constSlice()) |tracked_value, i|
                slot_values[i] = tracked_value.getValue();
        }
    };
}
