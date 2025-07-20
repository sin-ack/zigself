// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");

const Heap = @import("./Heap.zig");
const hash = @import("../utility/hash.zig");
const Value = @import("./value.zig").Value;
const AstGen = @import("./bytecode/AstGen.zig");
const pointer = @import("../utility/pointer.zig");
const ByteArray = @import("objects/byte_array.zig").ByteArray;
const map_builder = @import("map_builder.zig");
const ObjectValue = @import("./value.zig").ObjectValue;

/// The properties of a slot. This is shared by both ProtoSlot and Slot.
const SlotProperties = extern struct {
    /// The properties and hash of the slot, as a 62-bit integer.
    ///
    /// The top 32 bits are the slot hash, while the bottom 30 bits are the
    /// various properties of the slot.
    properties: Value align(@alignOf(u64)),

    const ParentShift = 0;
    const AssignableShift = 1;
    const ArgumentShift = 2;
    const IndexAssignedShift = 3;

    const ParentBit: u62 = 1 << ParentShift;
    const AssignableBit: u62 = 1 << AssignableShift;
    const ArgumentBit: u62 = 1 << ArgumentShift;
    // Used for sanity checks.
    const IndexAssignedBit: u62 = 1 << IndexAssignedShift;

    /// Whether this slot is a parent slot. A parent slot's value is included in
    /// the lookup after all the regular slots are checked.
    pub const ParentFlag = enum { Parent, NotParent };
    /// Whether this slot is assignable. Assignable slots have their values
    /// stored on the slots object itself instead of the map, so that assigning
    /// to them does not require modifying the map.
    pub const AssignableFlag = enum { Assignable, Constant };
    /// Whether this slot is an argument slot. Argument slot values will only
    /// exist on activation objects.
    pub const ArgumentFlag = enum { Argument, NotArgument };

    pub fn init(
        name: []const u8,
        parent: ParentFlag,
        assignable: AssignableFlag,
        argument: ArgumentFlag,
    ) SlotProperties {
        const name_hash = hash.stringHash(name);
        var properties: u62 = @as(u62, name_hash) << 30;
        if (parent == .Parent)
            properties |= ParentBit;
        if (assignable == .Assignable)
            properties |= AssignableBit;
        if (argument == .Argument)
            properties |= ArgumentBit;

        return .{ .properties = Value.fromUnsignedInteger(properties) };
    }

    pub fn isParent(self: SlotProperties) bool {
        return self.properties.unsafeAsUnsignedInteger() & ParentBit > 0;
    }

    pub fn isAssignable(self: SlotProperties) bool {
        return self.properties.unsafeAsUnsignedInteger() & AssignableBit > 0;
    }

    pub fn isArgument(self: SlotProperties) bool {
        return self.properties.unsafeAsUnsignedInteger() & ArgumentBit > 0;
    }

    fn isIndexAssigned(self: SlotProperties) bool {
        return self.properties.unsafeAsUnsignedInteger() & IndexAssignedBit > 0;
    }

    fn setIndexAssigned(self: *SlotProperties) void {
        if (self.isIndexAssigned())
            @panic("Attempting to set the index assigned bit twice!");
        self.properties = Value.fromUnsignedInteger(
            self.properties.unsafeAsUnsignedInteger() | IndexAssignedBit,
        );
    }

    pub fn getHash(self: SlotProperties) u32 {
        return @intCast(self.properties.unsafeAsUnsignedInteger() >> 30);
    }

    pub fn format(
        self: SlotProperties,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print(
            "SlotProperties{{ .hash = {x}, .parent = {s}, .assignable = {s}, .argument = {s} }}",
            .{
                self.getHash(),
                if (self.isParent()) "true" else "false",
                if (self.isAssignable()) "true" else "false",
                if (self.isArgument()) "true" else "false",
            },
        );
    }
};

/// A slot as it exists on the heap. The slot caries various properties, as well
/// as the hash of the slot name it holds and the value that is currently on
/// this slot.
pub const Slot = extern struct {
    /// The slot name.
    name: ObjectValue(ByteArray) align(@alignOf(u64)),
    properties: SlotProperties align(@alignOf(u64)),
    /// The value stored in this slot.
    ///
    /// For assignable slots, this will be the assignable slot index at which the
    /// current value is stored.
    ///
    /// For argument slots, the value will be the argument index, and the
    /// activation object that is created with the map containing this slot will
    /// have the slot at the given index (block and method objects will not
    /// contain the argument slots).
    value: Value align(@alignOf(u64)),

    pub const ConstSlice = pointer.HeapSlice(Slot, .Const);
    pub const Slice = pointer.HeapSlice(Slot, .Mutable);
    pub const ConstPtr = pointer.HeapPtr(Slot, .Const);
    pub const Ptr = pointer.HeapPtr(Slot, .Mutable);

    pub fn initRegular(name: ByteArray.Ptr, assignable: SlotProperties.AssignableFlag, value: Value) Slot {
        return init(name, .NotParent, assignable, .NotArgument, value);
    }

    pub fn initParent(name: ByteArray.Ptr, assignable: SlotProperties.AssignableFlag, value: Value) Slot {
        return init(name, .Parent, assignable, .NotArgument, value);
    }

    pub fn initArgument(name: ByteArray.Ptr) Slot {
        // FIXME: Somehow obtain vm.global_nil
        return init(name, .NotParent, .Assignable, .Argument, Value.fromUnsignedInteger(0));
    }

    fn init(
        name: ByteArray.Ptr,
        parent: SlotProperties.ParentFlag,
        assignable: SlotProperties.AssignableFlag,
        argument: SlotProperties.ArgumentFlag,
        value: Value,
    ) Slot {
        const properties = SlotProperties.init(name.getValues(), parent, assignable, argument);

        return .{
            .name = .init(name),
            .properties = properties,
            .value = value,
        };
    }

    pub fn isParent(self: Slot) bool {
        return self.properties.isParent();
    }

    pub fn isAssignable(self: Slot) bool {
        return self.properties.isAssignable();
    }

    pub fn isArgument(self: Slot) bool {
        return self.properties.isArgument();
    }

    /// Assign an index to the current slot, and return the value previously
    /// stored in it. Only available for assignable slots.
    pub fn assignIndex(self: Slot.Ptr, index: u8) Value {
        if (builtin.mode == .Debug) {
            std.debug.assert(self.isAssignable());
            std.debug.assert(!self.properties.isIndexAssigned());
        }

        const previous_value = self.value;
        self.value = Value.fromUnsignedInteger(index);

        if (builtin.mode == .Debug) {
            self.properties.setIndexAssigned();
        }

        return previous_value;
    }

    /// Return the index this slot was assigned.
    pub fn getIndex(self: Slot) u8 {
        if (builtin.mode == .Debug) {
            std.debug.assert(self.isAssignable());
            std.debug.assert(self.properties.isIndexAssigned());
        }

        return @intCast(self.value.unsafeAsUnsignedInteger());
    }

    pub fn getName(self: Slot) []const u8 {
        return self.name.get().getValues();
    }

    pub fn getHash(self: Slot) u32 {
        return self.properties.getHash();
    }

    /// Creates a copy of this slot with the original value, reverting any index
    /// assignment. Requires access to the object which holds this slot.
    pub fn copy(self: Slot, holder: anytype) Slot {
        if (self.isArgument()) {
            return initArgument(self.name.get());
        }

        const slot_value = if (self.isAssignable())
            holder.getAssignableSlotValue(self).*
        else
            self.value;

        if (self.isParent()) {
            return initParent(self.name.get(), if (self.isAssignable()) .Assignable else .Constant, slot_value);
        }

        return initRegular(self.name.get(), if (self.isAssignable()) .Assignable else .Constant, slot_value);
    }

    fn GetSlotWithMyNameResult(comptime previous_slots_type: type) type {
        if (previous_slots_type == Slot.Slice)
            return ?Slot.Ptr;
        if (previous_slots_type == Slot.ConstSlice)
            return ?Slot.ConstPtr;
        @compileError("getSlotWithMyName must receive either []Slot or []const Slot as previous_slots");
    }

    /// Return a pointer to a slot from the previous slots slice which has the
    /// same name (hash) as this slot.
    fn getSlotWithMyName(self: Slot, previous_slots: anytype) GetSlotWithMyNameResult(@TypeOf(previous_slots)) {
        // NOTE: Walking backwards to find the first slot that overwrote the
        //       earlier ones.
        for (previous_slots, 0..) |_, index| {
            const slot = &previous_slots[previous_slots.len - 1 - index];

            if (self.getHash() == slot.getHash())
                return slot;
        }

        return null;
    }

    /// Return how many slot spaces this slot needs on the map for its contents.
    /// Takes a slice of slots that come before this slot.
    pub fn requiredSlotSpace(self: Slot, previous_slots: Slot.ConstSlice) u16 {
        return if (self.getSlotWithMyName(previous_slots) != null) 0 else 1;
    }

    /// Return how many assignable slot value spaces this slot needs on the
    /// object for its contents. Takes a slice of slots that come before this
    /// slot. May return a negative value if this slot overrides a previous
    /// assignable slot and does not consume an assignable slot value space
    /// itself.
    pub fn requiredAssignableSlotValueSpace(self: Slot, previous_slots: Slot.ConstSlice) i15 {
        var diff: i15 = 0;
        if (self.getSlotWithMyName(previous_slots)) |previous_slot| {
            if (previous_slot.isAssignable() and !previous_slot.isArgument())
                diff -= 1;
        }

        if (self.isAssignable() and !self.isArgument())
            diff += 1;
        return diff;
    }

    /// Write the contents of this slot to the given slot map. Also write the
    /// assignable slot values of this slot to the given assignable slot values
    /// list. slot_index, assignable_slot_index and argument_slot_index are
    /// both in and out parameters, and are incremented by the amount of slots
    /// written.
    pub fn writeContentsTo(
        self: Slot,
        target_slots: Slot.Slice,
        assignable_slot_values: *map_builder.AssignableSlotValues,
        slot_index: *usize,
        assignable_slot_index: *usize,
        argument_slot_index: *usize,
    ) void {
        const previous_slots = target_slots[0..slot_index.*];
        var current_slot_ptr: Slot.Ptr = undefined;

        // If a previous slot with the same name exists then we need to undo the
        // changes from the previous slot.
        if (self.getSlotWithMyName(previous_slots)) |previous_slot| {
            current_slot_ptr = previous_slot;

            if (previous_slot.isArgument())
                @panic("TODO: Handle overriding of argument slots");

            if (previous_slot.isAssignable()) {
                const overwritten_assignable_slot_index = previous_slot.value.unsafeAsUnsignedInteger();
                _ = assignable_slot_values.orderedRemove(@intCast(overwritten_assignable_slot_index));

                // Go through all the previous slots, and subtract 1 from all
                // the slots which have assignable slot indices larger than the
                // current one (to sync with the slot value removal).
                for (previous_slots) |*slot| {
                    if (!slot.isArgument() and slot.isAssignable()) {
                        const other_assignable_slot_index = slot.value.unsafeAsUnsignedInteger();
                        if (other_assignable_slot_index > overwritten_assignable_slot_index)
                            slot.value = Value.fromUnsignedInteger(@intCast(other_assignable_slot_index - 1));
                    }
                }

                assignable_slot_index.* -= 1;
            }
        } else {
            current_slot_ptr = &target_slots[slot_index.*];
            slot_index.* += 1;
        }

        current_slot_ptr.* = self;

        if (self.isArgument()) {
            std.debug.assert(argument_slot_index.* < AstGen.MaximumArguments);

            _ = current_slot_ptr.assignIndex(@intCast(argument_slot_index.*));
            argument_slot_index.* += 1;
        } else if (self.isAssignable()) {
            std.debug.assert(assignable_slot_index.* + argument_slot_index.* < AstGen.MaximumAssignableSlots);

            const value = current_slot_ptr.assignIndex(@intCast(assignable_slot_index.*));
            assignable_slot_values.appendAssumeCapacity(value);
            assignable_slot_index.* += 1;
        }
    }
};
