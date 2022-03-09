// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");

const Heap = @import("./heap.zig");
const hash = @import("../utility/hash.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig");
const ByteArray = @import("./byte_array.zig");

/// A slot as it exists on the heap. The slot caries various
pub const Slot = packed struct {
    /// The slot name.
    name: Value,
    /// The properties and hash of the slot, as a 62-bit integer.
    ///
    /// The top 32 bits are the slot hash, while the bottom 30 bits are the
    /// various properties of the slot.
    properties: Value,
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

    pub fn initConstant(name: ByteArray, parent: ParentFlag, value: Value) Slot {
        return init(name, parent, .Constant, .NotArgument, value);
    }

    pub fn initAssignable(name: ByteArray, parent: ParentFlag, value: Value) Slot {
        return init(name, parent, .Assignable, .NotArgument, value);
    }

    pub fn initArgument(name: ByteArray) Slot {
        // FIXME: Somehow obtain vm.nil()
        return init(name, .NotParent, .Assignable, .Argument, Value.fromUnsignedInteger(0));
    }

    fn init(
        name: ByteArray,
        parent: ParentFlag,
        assignable: AssignableFlag,
        argument: ArgumentFlag,
        value: Value,
    ) Slot {
        const name_hash = hash.stringHash(name.getValues());
        var properties: u62 = @as(u62, name_hash) << 30;
        if (parent == .Parent)
            properties |= ParentBit;
        if (assignable == .Assignable)
            properties |= AssignableBit;
        if (argument == .Argument)
            properties |= ArgumentBit;

        return .{
            .name = name.asValue(),
            .properties = Value.fromUnsignedInteger(properties),
            .value = value,
        };
    }

    pub fn isParent(self: Slot) bool {
        return self.properties.asUnsignedInteger() & ParentBit > 0;
    }

    pub fn isAssignable(self: Slot) bool {
        return self.properties.asUnsignedInteger() & AssignableBit > 0;
    }

    pub fn isArgument(self: Slot) bool {
        return self.properties.asUnsignedInteger() & ArgumentBit > 0;
    }

    fn isIndexAssigned(self: Slot) bool {
        return self.properties.asUnsignedInteger() & IndexAssignedBit > 0;
    }

    fn setIndexAssigned(self: *Slot) void {
        if (self.isIndexAssigned())
            @panic("Attempting to set the index assigned bit twice!");
        self.properties = Value.fromUnsignedInteger(
            self.properties.asUnsignedInteger() | IndexAssignedBit,
        );
    }

    /// Assign an index to the current slot, and return the value previously
    /// stored in it. Only available for assignable slots.
    pub fn assignIndex(self: *Slot, index: u8) Value {
        // Have to be behind this because assert removal in ReleaseFast is
        // unreliable.
        if (builtin.mode == .Debug) {
            std.debug.assert(self.isAssignable());
            std.debug.assert(!self.isIndexAssigned());
        }

        const previous_value = self.value;
        self.value = Value.fromUnsignedInteger(index);

        if (builtin.mode == .Debug) {
            self.setIndexAssigned();
        }

        return previous_value;
    }

    /// Return the index this slot was assigned.
    pub fn getIndex(self: Slot) u8 {
        if (builtin.mode == .Debug) {
            std.debug.assert(self.isAssignable());
            std.debug.assert(self.isIndexAssigned());
        }

        return @intCast(u8, self.value.asUnsignedInteger());
    }

    pub fn getHash(self: Slot) u32 {
        return @intCast(u32, self.properties.asUnsignedInteger() >> 30);
    }

    /// Creates a copy of this slot with the original value, reverting any index
    /// assignment. Requires access to the object which holds this slot.
    pub fn copy(self: Slot, holder: anytype) Slot {
        const name = self.name.asByteArray();

        if (self.isArgument()) {
            return initArgument(name);
        }

        if (self.isAssignable()) {
            const slot_value = holder.getAssignableSlots()[self.value.asUnsignedInteger()];
            return initAssignable(name, if (self.isParent()) .Parent else .NotParent, slot_value);
        }

        return initConstant(name, if (self.isParent()) .Parent else .NotParent, self.value);
    }
};
