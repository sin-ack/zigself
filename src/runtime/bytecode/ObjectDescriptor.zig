// Copyright (c) 2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! Describes the slots an object can have. This is used in the bytecode to create
//! slots, method and block objects.
//!
//! Regular and parent slots will have an initial value assigned to them. This
//! value is pushed to the argument stack in regular order (so the first default
//! value is at the bottom of the stack, and the last default value is at the
//! top). To initialize an object the slots should be created with the default
//! values in the reverse order. No argument should be popped for argument
//! slots.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// The slots of the object to be created.
slots: []const SlotDescriptor,
/// The number of slots requiring an assignable slot value.
slots_requiring_assignable_slot_value: u15 = 0,
/// The number of assignable slots in total.
assignable_slots: u16 = 0,
/// The number of argument slots.
argument_slots: u8 = 0,
/// The number of slots with an initial value present on the argument stack.
slots_with_initial_value: u32 = 0,

const ObjectDescriptor = @This();
pub const SlotDescriptor = struct {
    /// The name of the slot. Owned by the object descriptor.
    name: []const u8,
    /// The type of the slot.
    type: SlotType,
    /// The index of the assignable slot value in the object, if this
    /// slot is assignable. If this is null, the slot is not assignable.
    assignable_index: ?usize = null,

    /// Create a new regular slot. If assignable_index is not null, the slot
    /// is assignable and the index of the assignable slot value is set in
    /// the resulting object.
    pub fn initRegular(allocator: Allocator, name: []const u8, assignable_index: ?usize) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, name);
        return .{ .name = name_copy, .type = .Regular, .assignable_index = assignable_index };
    }

    /// Create a new parent slot. If assignable_index is not null, the slot
    /// is assignable and the index of the assignable slot value is set in
    /// the resulting object.
    pub fn initParent(allocator: Allocator, name: []const u8, assignable_index: ?usize) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, name);
        return .{ .name = name_copy, .type = .Parent, .assignable_index = assignable_index };
    }

    /// Create a new argument slot. This slot is always assignable and
    /// the assignable index is set to the given value.
    pub fn initArgument(allocator: Allocator, name: []const u8, assignable_index: usize) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, name);
        return .{ .name = name_copy, .type = .Argument, .assignable_index = assignable_index };
    }

    /// Deinitialize the slot, freeing resources.
    pub fn deinit(self: SlotDescriptor, allocator: Allocator) void {
        allocator.free(self.name);
    }

    /// Create a copy of the slot.
    pub fn copy(self: SlotDescriptor, allocator: Allocator) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, self.name);
        return .{ .name = name_copy, .type = self.type, .assignable_index = self.assignable_index };
    }

    /// Return whether this slot requires an accompanying assignable slot value
    /// on the object in addition to the slot on the map itself.
    pub fn requiresAssignableSlotValue(self: SlotDescriptor) bool {
        // NOTE: While argument slots are assignable, they are not counted as
        //       requiring an assignable slot value. Since they would only
        //       ever contain nil, we skip them when creating method and block
        //       objects and only include them in activation objects.
        return self.assignable_index != null and self.hasInitialValue();
    }

    /// Return whether this slot has an initial value on the argument stack
    /// while initializing the object.
    pub fn hasInitialValue(self: SlotDescriptor) bool {
        return self.type != .Argument;
    }
};
const SlotType = enum {
    /// A regular slot. Can be constant or assignable.
    Regular,
    /// A parent slot. When a message is sent to an object, if the slot is not found
    /// on the object itself, its parents are searched for the slot. Can be constant
    /// or assignable.
    Parent,
    /// An argument slot. Cannot be directly created, but is used to describe arguments
    /// to methods and blocks. Can only be assignable.
    Argument,
};

pub fn init(slots: []const SlotDescriptor) ObjectDescriptor {
    var descriptor: ObjectDescriptor = .{ .slots = slots };
    for (slots) |slot| {
        if (slot.assignable_index != null) descriptor.assignable_slots += 1;
        if (slot.requiresAssignableSlotValue()) descriptor.slots_requiring_assignable_slot_value += 1;
        if (slot.type == .Argument) descriptor.argument_slots += 1;
        if (slot.hasInitialValue()) descriptor.slots_with_initial_value += 1;
    }

    return descriptor;
}

pub fn deinit(self: ObjectDescriptor, allocator: Allocator) void {
    for (self.slots) |slot| {
        slot.deinit(allocator);
    }
    allocator.free(self.slots);
}

/// Create a new copy of the object descriptor with the same slots.
pub fn copy(self: ObjectDescriptor, allocator: Allocator) !ObjectDescriptor {
    var new_slots: std.ArrayList(SlotDescriptor) = try .initCapacity(allocator, self.slots.len);
    defer {
        for (new_slots.items) |*slot| {
            slot.deinit(allocator);
        }
        new_slots.deinit(allocator);
    }

    for (self.slots) |slot| {
        const slot_copy = try slot.copy(allocator);
        new_slots.appendAssumeCapacity(slot_copy);
    }

    const new_slots_slice = try new_slots.toOwnedSlice(allocator);
    return .init(new_slots_slice);
}
