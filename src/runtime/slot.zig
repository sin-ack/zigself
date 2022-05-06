// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const Heap = @import("./Heap.zig");
const hash = @import("../utility/hash.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./Object.zig");
const AstGen = @import("./AstGen.zig");
const ByteArray = @import("./ByteArray.zig");
const map_builder = @import("./object/map_builder.zig");

/// The properties of a slot. This is shared by both ProtoSlot and Slot.
const SlotProperties = packed struct {
    /// The properties and hash of the slot, as a 62-bit integer.
    ///
    /// The top 32 bits are the slot hash, while the bottom 30 bits are the
    /// various properties of the slot.
    properties: Value,

    const ParentShift = 0;
    const AssignableShift = 1;
    const ArgumentShift = 2;
    const InheritedShift = 3;
    const InheritedChildShift = 4;
    const IndexAssignedShift = 5;

    const ParentBit: u62 = 1 << ParentShift;
    const AssignableBit: u62 = 1 << AssignableShift;
    const ArgumentBit: u62 = 1 << ArgumentShift;
    const InheritedBit: u62 = 1 << InheritedShift;
    const InheritedChildBit: u62 = 1 << InheritedChildShift;
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
    /// Whether this slot is inherited. Inherited slots copy their value's slots
    /// to their holder object and update the holder object whenever the value
    /// object has its slots changed.
    pub const InheritedFlag = enum { Inherited, NotInherited };
    /// Whether this slot was added to the object through an inherited slot.
    pub const InheritedChildFlag = enum { InheritedChild, NotInheritedChild };

    pub fn init(
        name: []const u8,
        parent: ParentFlag,
        assignable: AssignableFlag,
        argument: ArgumentFlag,
        inherited: InheritedFlag,
        inherited_child: InheritedChildFlag,
    ) SlotProperties {
        const name_hash = hash.stringHash(name);
        var properties: u62 = @as(u62, name_hash) << 30;
        if (parent == .Parent)
            properties |= ParentBit;
        if (assignable == .Assignable)
            properties |= AssignableBit;
        if (argument == .Argument)
            properties |= ArgumentBit;
        if (inherited == .Inherited)
            properties |= InheritedBit;
        if (inherited_child == .InheritedChild)
            properties |= InheritedChildBit;

        return .{ .properties = Value.fromUnsignedInteger(properties) };
    }

    pub fn isParent(self: SlotProperties) bool {
        return self.properties.asUnsignedInteger() & ParentBit > 0;
    }

    pub fn isAssignable(self: SlotProperties) bool {
        return self.properties.asUnsignedInteger() & AssignableBit > 0;
    }

    pub fn isArgument(self: SlotProperties) bool {
        return self.properties.asUnsignedInteger() & ArgumentBit > 0;
    }

    pub fn isInherited(self: SlotProperties) bool {
        return self.properties.asUnsignedInteger() & InheritedBit > 0;
    }

    pub fn isInheritedChild(self: SlotProperties) bool {
        return self.properties.asUnsignedInteger() & InheritedChildBit > 0;
    }

    fn setInheritedChild(self: *SlotProperties) void {
        self.properties = Value.fromUnsignedInteger(
            self.properties.asUnsignedInteger() | InheritedChildBit,
        );
    }

    fn isIndexAssigned(self: SlotProperties) bool {
        return self.properties.asUnsignedInteger() & IndexAssignedBit > 0;
    }

    fn setIndexAssigned(self: *SlotProperties) void {
        if (self.isIndexAssigned())
            @panic("Attempting to set the index assigned bit twice!");
        self.properties = Value.fromUnsignedInteger(
            self.properties.asUnsignedInteger() | IndexAssignedBit,
        );
    }

    pub fn getHash(self: SlotProperties) u32 {
        return @intCast(u32, self.properties.asUnsignedInteger() >> 30);
    }
};

/// A slot as it exists on the heap. The slot caries various properties, as well
/// as the hash of the slot name it holds and the value that is currently on
/// this slot.
pub const Slot = packed struct {
    /// The slot name.
    name: Value,
    properties: SlotProperties,
    /// The value stored in this slot.
    ///
    /// For assignable slots, this will be the assignable slot index at which the
    /// current value is stored.
    ///
    /// For argument slots, the value will be the argument index, and the
    /// activation object that is created with the map containing this slot will
    /// have the slot at the given index (block and method objects will not
    /// contain the argument slots).
    value: Value,

    pub fn initConstant(name: ByteArray, parent: SlotProperties.ParentFlag, value: Value) Slot {
        return init(name, parent, .Constant, .NotArgument, .NotInherited, value);
    }

    pub fn initAssignable(name: ByteArray, parent: SlotProperties.ParentFlag, value: Value) Slot {
        return init(name, parent, .Assignable, .NotArgument, .NotInherited, value);
    }

    pub fn initArgument(name: ByteArray) Slot {
        // FIXME: Somehow obtain vm.nil()
        return init(name, .NotParent, .Assignable, .Argument, .NotInherited, Value.fromUnsignedInteger(0));
    }

    pub fn initInherited(name: ByteArray, value: Value) Slot {
        std.debug.assert(value.isObjectReference() and value.asObject().isSlotsObject());
        return init(name, .NotParent, .Constant, .NotArgument, .Inherited, value);
    }

    fn init(
        name: ByteArray,
        parent: SlotProperties.ParentFlag,
        assignable: SlotProperties.AssignableFlag,
        argument: SlotProperties.ArgumentFlag,
        inherited: SlotProperties.InheritedFlag,
        value: Value,
    ) Slot {
        const properties = SlotProperties.init(name.getValues(), parent, assignable, argument, inherited, .NotInheritedChild);

        return .{
            .name = name.asValue(),
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

    pub fn isInherited(self: Slot) bool {
        return self.properties.isInherited();
    }

    pub fn isInheritedChild(self: Slot) bool {
        return self.properties.isInheritedChild();
    }

    /// Assign an index to the current slot, and return the value previously
    /// stored in it. Only available for assignable slots.
    pub fn assignIndex(self: *Slot, index: u8) Value {
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

        return @intCast(u8, self.value.asUnsignedInteger());
    }

    pub fn getHash(self: Slot) u32 {
        return self.properties.getHash();
    }

    /// Creates a copy of this slot with the original value, reverting any index
    /// assignment. Requires access to the object which holds this slot.
    pub fn copy(self: Slot, holder: anytype) Slot {
        const name = self.name.asByteArray();

        if (self.isInherited()) {
            return initInherited(name, self.value);
        }

        if (self.isArgument()) {
            return initArgument(name);
        }

        if (self.isAssignable()) {
            const slot_value = holder.getAssignableSlotValue(self);
            return initAssignable(name, if (self.isParent()) .Parent else .NotParent, slot_value.*);
        }

        return initConstant(name, if (self.isParent()) .Parent else .NotParent, self.value);
    }

    fn GetSlotWithMyNameResult(comptime previous_slots_type: type) type {
        if (previous_slots_type == []Slot)
            return ?*Slot;
        if (previous_slots_type == []const Slot)
            return ?*const Slot;
        @compileError("getSlotWithMyName must receive either []Slot or []const Slot as previous_slots");
    }

    const TraverseInheritedSlots = enum { Traverse, DontTraverse };
    /// Return a pointer to a slot from the previous slots slice which has the
    /// same name (hash) as this slot. If traverse_inherited_slots is Traverse,
    /// then also looks at the slots of inherited slot values.
    fn getSlotWithMyName(self: Slot, comptime traverse_inherited_slots: TraverseInheritedSlots, previous_slots: anytype) GetSlotWithMyNameResult(@TypeOf(previous_slots)) {
        // NOTE: Walking backwards to find the first slot that overwrote the
        //       earlier ones.
        for (previous_slots) |_, index| {
            const slot = &previous_slots[previous_slots.len - 1 - index];

            if (traverse_inherited_slots == .Traverse and slot.isInherited()) {
                const inherited_slots = slot.value.asObject().asSlotsObject().getSlots();
                for (inherited_slots) |_, inherited_index| {
                    const inherited_slot = &inherited_slots[inherited_slots.len - 1 - inherited_index];
                    if (self.getHash() == inherited_slot.getHash())
                        return inherited_slot;
                }
            }

            if (self.getHash() == slot.getHash())
                return slot;
        }

        return null;
    }

    /// Return how many slot spaces this slot needs on the map for its contents.
    /// Takes a slice of slots that come before this slot.
    pub fn requiredSlotSpace(self: Slot, previous_slots: []const Slot) u32 {
        if (self.isInherited()) {
            // FIXME: Turn this into a runtime error instead of a panic.
            if (self.getSlotWithMyName(.Traverse, previous_slots) != null)
                @panic("Name collision is not allowed for inherited slots");

            // 1 for the slot itself.
            var slot_count: u32 = 1;

            next_slot: for (self.value.asObject().asSlotsObject().getSlots()) |inherited_slot| {
                if (inherited_slot.isInherited()) {
                    // We don't want to inherit the inherited slots in the inherited object.
                    // (Try saying that 3 times fast...)
                    continue :next_slot;
                }

                if (inherited_slot.getSlotWithMyName(.Traverse, previous_slots) != null)
                    continue :next_slot;

                slot_count += 1;
            }

            return slot_count;
        }

        return if (self.getSlotWithMyName(.Traverse, previous_slots) != null) 0 else 1;
    }

    /// Return how many assignable slot value spaces this slot needs on the
    /// object for its contents. Takes a slice of slots that come before this
    /// slot. May return a negative value if this slot overrides a previous
    /// assignable slot and does not consume an assignable slot value space
    /// itself.
    pub fn requiredAssignableSlotValueSpace(self: Slot, previous_slots: []const Slot) i32 {
        if (self.isInherited()) {
            // FIXME: Turn this into a runtime error instead of a panic.
            if (self.getSlotWithMyName(.Traverse, previous_slots) != null)
                @panic("Name collision is not allowed for inherited slots");

            var slot_count: i32 = 0;

            next_slot: for (self.value.asObject().asSlotsObject().getSlots()) |inherited_slot| {
                if (inherited_slot.isInherited()) {
                    // We don't want to inherit the inherited slots in the inherited object.
                    // (Try saying that 3 times fast...)
                    continue :next_slot;
                }

                if (inherited_slot.getSlotWithMyName(.Traverse, previous_slots)) |previous_slot| {
                    if (previous_slot.isAssignable() and !previous_slot.isArgument())
                        slot_count -= 1;
                }

                if (inherited_slot.isAssignable() and !inherited_slot.isArgument())
                    slot_count += 1;
            }

            return slot_count;
        }

        var diff: i32 = 0;
        if (self.getSlotWithMyName(.Traverse, previous_slots)) |previous_slot| {
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
        heap: *Heap,
        target_slots: []Slot,
        assignable_slot_values: *map_builder.AssignableSlotValues,
        slot_index: *usize,
        assignable_slot_index: *usize,
        argument_slot_index: *usize,
    ) Allocator.Error!void {
        var previous_slots = target_slots[0..slot_index.*];
        var current_slot_ptr: *Slot = undefined;

        // If a previous slot with the same name exists then we need to undo the
        // changes from the previous slot.
        if (self.getSlotWithMyName(.DontTraverse, previous_slots)) |previous_slot| {
            current_slot_ptr = previous_slot;

            // FIXME: Turn this into a runtime error instead of a panic.
            if (previous_slot.isInherited())
                @panic("Name collision is not allowed for inherited slots");
            if (previous_slot.isArgument())
                @panic("TODO: Handle overriding of argument slots");

            if (previous_slot.isAssignable()) {
                const overwritten_assignable_slot_index = previous_slot.value.asUnsignedInteger();
                assignable_slot_values.orderedRemove(overwritten_assignable_slot_index).untrack(heap);

                // Go through all the previous slots, and subtract 1 from all
                // the slots which have assignable slot indices larger than the
                // current one (to sync with the slot value removal).
                for (previous_slots) |*slot| {
                    if (!slot.isArgument() and slot.isAssignable()) {
                        const other_assignable_slot_index = slot.value.asUnsignedInteger();
                        if (other_assignable_slot_index > overwritten_assignable_slot_index)
                            slot.value = Value.fromUnsignedInteger(other_assignable_slot_index - 1);
                    }
                }

                assignable_slot_index.* -= 1;
            }
        } else {
            current_slot_ptr = &target_slots[slot_index.*];
            slot_index.* += 1;
        }

        current_slot_ptr.* = self;

        if (self.isInherited()) {
            const inherited_object = self.value.asObject().asSlotsObject();
            next_slot: for (inherited_object.getSlots()) |inherited_slot| {
                if (inherited_slot.isInherited()) {
                    // We don't want to inherit the inherited slots in the inherited object.
                    // (Try saying that 3 times fast...)
                    continue :next_slot;
                }

                // Create a copy in order to fold the assignable slot value back
                // into the slot.
                var inherited_slot_copy = inherited_slot.copy(inherited_object);
                inherited_slot_copy.properties.setInheritedChild();
                try inherited_slot_copy.writeContentsTo(
                    heap,
                    target_slots,
                    assignable_slot_values,
                    slot_index,
                    assignable_slot_index,
                    argument_slot_index,
                );
                previous_slots = target_slots[0..slot_index.*];
            }
        } else if (self.isArgument()) {
            std.debug.assert(argument_slot_index.* < AstGen.MaximumArguments);

            _ = current_slot_ptr.assignIndex(@intCast(u8, argument_slot_index.*));
            argument_slot_index.* += 1;
        } else if (self.isAssignable()) {
            std.debug.assert(assignable_slot_index.* + argument_slot_index.* < AstGen.MaximumAssignableSlots);

            const value = current_slot_ptr.assignIndex(@intCast(u8, assignable_slot_index.*));
            assignable_slot_values.appendAssumeCapacity(try heap.track(value));
            assignable_slot_index.* += 1;
        }
    }
};
