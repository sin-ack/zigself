// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const Selector = @import("Selector.zig");
const ActorObject = @import("objects/actor.zig").Actor;
const MethodObject = @import("objects/method.zig").Method;

/// A "value slot", which is a value that is stored somewhere in the object.
/// Note that this doesn't necessarily map 1:1 to physical slots;
/// a value slot is a more abstract concept, and objects can have "virtual
/// value slots" not backed by a physical slot in the object.
pub const ValueSlot = union(enum) {
    /// A constant value slot. Cannot be assigned to.
    Constant: Value,
    /// An assignable value slot.
    Assignable: struct {
        // FIXME: Deduplicate this with `LookupTarget.object`
        /// The object that contains the value.
        object: Object.Ptr,
        /// The pointer to the value. Can be safely written to. Note that if
        /// you write to this, you must also record an entry in the remembered
        /// set from `object` to the written value on the heap.
        value_ptr: *align(@alignOf(u64)) Value,
        /// The selector corresponding to the slot this value is stored in.
        /// This is used to determine whether a get or an assign operation
        /// is being performed.
        selector: Selector,
    },
};

/// The target (object, index) pair in which a value was found during a lookup
/// operation.
pub const LookupTarget = struct {
    /// The object that contains the value.
    object: Object.Ptr,
    /// The value slot that was found.
    value_slot: ValueSlot,
    /// The index of this value slot in the object.
    value_slot_index: usize,
};

/// The result of a lookup operation.
pub const LookupResult = union(enum) {
    /// Nothing matching the selector was found.
    Nothing: void,

    /// A regular slot was matched (constant or assignable slot).
    /// Note that if you obtain an assignable slot from this target and assign
    /// to it, you must remember the object reference on the heap.
    Found: LookupTarget,

    /// A regular slot was matched, but it cannot be cached for some reason.
    /// This should only be used in very specific cases where a target object
    /// cannot be determined or it would be unsafe to cache the target.
    FoundUncacheable: ValueSlot,

    /// A message send to another actor through the use of an actor proxy. This
    /// will be returned when the user performs a lookup on an actor proxy
    /// object.
    // NOTE: Since actor messages perform a send to a remote actor, the send
    //       CANNOT be cached, so no LookupTarget is provided.
    ActorMessage: struct {
        target_actor: ActorObject.Ptr,
        method: MethodObject.Ptr,
    },

    pub const nothing = LookupResult{ .Nothing = {} };
};
