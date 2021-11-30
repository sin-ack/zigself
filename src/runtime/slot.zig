// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const Heap = @import("./heap.zig");
const hash = @import("../utility/hash.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig");
const ByteVector = @import("./byte_vector.zig");

const ParentShift = 2;
const ParentBit: u32 = 1 << ParentShift;

const MutableShift = 3;
const MutableBit: u32 = 1 << MutableShift;

pub const SlotParentFlag = enum { Parent, NotParent };
pub const SlotMutableFlag = enum { Mutable, Constant };

pub const Slot = packed struct {
    name: Value,
    /// A bitfield describing the properties of this slot. The bottom two bits
    /// are always zero.
    ///
    /// Bit 2: Whether this slot is a parent slot; that is, whether the value
    /// will have its slots considered after a lookup on the object's regular
    /// slots fail.
    ///
    /// Bit 3: Whether this slot is mutable or a constant. If the slot is
    /// constant, then the `value` field will contain the value of this slot,
    /// and it will not change across all the object which have this slot in
    /// their map; if the slot is mutable, then each slot object pointing to the
    /// map this slot is contained within will carry a `Value` object
    /// corresponding to this slot.
    properties: u32,
    /// The hash of the slot name.
    hash: u32,
    value: Value,

    /// Initalizes this slot to a constant value.
    pub fn initConstant(self: *Slot, name: ByteVector, parent_flag: SlotParentFlag, value: Value) void {
        self.init(name);
        self.value = value;

        self.setParent(parent_flag);
    }

    /// Initalizes this slot to a mutable value.
    pub fn initMutable(self: *Slot, map: *Object.Map.Slots, name: ByteVector, parent_flag: SlotParentFlag) void {
        self.init(name);

        self.setParent(parent_flag);
        self.setMutable(map, .Mutable);
    }

    fn init(self: *Slot, name: ByteVector) void {
        // TODO: Verify that this slot hasn't been initialized yet!
        self.name = name.asValue();
        self.hash = hash.stringHash(name.getValues());
        self.properties = 0;
    }

    pub fn isParent(self: *const Slot) bool {
        return self.properties & ParentBit > 0;
    }

    pub fn setParent(self: *Slot, flag: SlotParentFlag) void {
        self.properties = switch (flag) {
            .Parent => self.properties | ParentBit,
            .NotParent => self.properties & ~ParentBit,
        };
    }

    pub fn isMutable(self: *const Slot) bool {
        return self.properties & MutableBit > 0;
    }

    /// Sets whether this slot is mutable or not. If the mutable state of the
    /// slot differs from what it is being set to, the map's assignable slot
    /// count is updated.
    pub fn setMutable(self: *Slot, map: *Object.Map.Slots, flag: SlotMutableFlag) void {
        switch (flag) {
            .Mutable => {
                if (self.properties & MutableBit > 0) return;
                self.properties |= MutableBit;
                map.setAssignableSlotCount(map.getAssignableSlotCount() + 1);
            },
            .Constant => {
                if (self.properties & MutableBit == 0) return;
                self.properties &= ~MutableBit;
                map.setAssignableSlotCount(map.getAssignableSlotCount() - 1);
            },
        }
    }
};
