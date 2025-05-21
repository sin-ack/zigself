// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const heap = @import("Heap.zig");
const Slot = @import("slot.zig").Slot;
const Value = @import("value.zig").Value;
const AstGen = @import("bytecode/AstGen.zig");
const context = @import("context.zig");

pub const AssignableSlotValues = std.BoundedArray(Value, AstGen.MaximumAssignableSlots);

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
        token: *heap.AllocationToken,
        map: MapType.Ptr,
        assignable_slot_values: AssignableSlotValues = undefined,

        slot_index: usize = 0,
        assignable_slot_index: usize = 0,
        argument_slot_index: usize = 0,

        const Self = @This();

        // Marker for typechecking.
        pub const is_map_builder = true;

        /// Initialize the map builder in-place. Usage:
        ///
        ///     var map_builder: MapBuilder(MapType, ObjectType) = undefined;
        ///     map_builder.initInPlace(token, map);
        ///
        pub fn initInPlace(self: *Self, token: *heap.AllocationToken, map: MapType.Ptr) void {
            self.* = .{
                .token = token,
                .map = map,
            };
            // XXX: No matter what I do here, I cannot make the Zig compiler not
            //      copy ~2KB of uninitialized memory if we call
            //      BoundedArray.init or manually initialize the struct.
            //      Instead we only set the field that matters. This is very
            //      brittle; I hope RLS gets fixed.
            self.assignable_slot_values.len = 0;
        }

        pub fn addSlot(self: *Self, slot: Slot) void {
            slot.writeContentsTo(
                self.map.getSlots(),
                &self.assignable_slot_values,
                &self.slot_index,
                &self.assignable_slot_index,
                &self.argument_slot_index,
            );

            // NOTE: Method and block maps do not count the argument slot count
            //       towards their assignable slot counts, because the argument
            //       slot values don't exist on the method and block objects.
            self.map.setAssignableSlotCount(@intCast(self.assignable_slot_index));
        }

        pub fn createObject(self: *Self) ObjectType.Ptr {
            var slot_values: [AstGen.MaximumAssignableSlots]Value = undefined;
            const slot_values_slice = slot_values[0..self.assignable_slot_index];
            self.writeAssignableSlotValuesTo(slot_values_slice);

            return ObjectType.create(
                self.token,
                context.getActor().id,
                self.map,
                slot_values_slice,
            );
        }

        /// Write the current assignable slots to the given assignable slots slice.
        /// Assumes that a garbage collection will not happen before the values'
        /// use.
        pub fn writeAssignableSlotValuesTo(self: *Self, slot_values: []Value) void {
            @memcpy(slot_values, self.assignable_slot_values.constSlice());
        }
    };
}
