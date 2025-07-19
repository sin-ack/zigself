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

slots: []const SlotDescriptor,

const ObjectDescriptor = @This();
pub const SlotDescriptor = struct {
    /// The name of the slot. Owned by the object descriptor.
    name: []const u8,
    /// The type of the slot.
    type: SlotType,
    /// Whether the slot can be assigned to.
    assignable: bool,

    /// Create a new regular slot.
    pub fn initRegular(allocator: Allocator, name: []const u8, assignable: bool) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, name);
        return .{ .name = name_copy, .type = .Regular, .assignable = assignable };
    }

    /// Create a new parent slot.
    pub fn initParent(allocator: Allocator, name: []const u8, assignable: bool) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, name);
        return .{ .name = name_copy, .type = .Parent, .assignable = assignable };
    }

    /// Create a new argument slot.
    pub fn initArgument(allocator: Allocator, name: []const u8) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, name);
        return .{ .name = name_copy, .type = .Argument, .assignable = true };
    }

    /// Deinitialize the slot, freeing resources.
    pub fn deinit(self: SlotDescriptor, allocator: Allocator) void {
        allocator.free(self.name);
    }

    /// Create a copy of the slot.
    pub fn copy(self: SlotDescriptor, allocator: Allocator) !SlotDescriptor {
        const name_copy = try allocator.dupe(u8, self.name);
        return .{ .name = name_copy, .type = self.type, .assignable = self.assignable };
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
    return .{ .slots = slots };
}

pub fn deinit(self: ObjectDescriptor, allocator: Allocator) void {
    for (self.slots) |slot| {
        slot.deinit(allocator);
    }
    allocator.free(self.slots);
}

/// Create a new copy of the object descriptor with the same slots.
pub fn copy(self: ObjectDescriptor, allocator: Allocator) !ObjectDescriptor {
    var new_slots: std.ArrayListUnmanaged(SlotDescriptor) = try .initCapacity(allocator, self.slots.len);
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
