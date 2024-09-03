// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const ActorObject = @import("objects/actor.zig").Actor;
const MethodObject = @import("objects/method.zig").Method;

/// The result of a lookup operation.
pub const LookupResult = union(enum) {
    /// Nothing matching the selector was found.
    Nothing: void,
    /// A regular slot was matched (either a method which should be activated,
    /// or something else which can be returned as-is).
    Regular: Value,
    /// An assignable slot was found and the passed selector is intended to
    /// assign to it. The value pointer can be safely assigned to. Keep in
    /// mind that if you do this, then you will have to also remember the
    /// object reference in the heap.
    Assignment: struct {
        object: Object.Ptr,
        value_ptr: *Value,
    },
    /// A message send to another actor through the use of an actor proxy. This
    /// will be returned when the user performs a lookup on an actor proxy
    /// object.
    ActorMessage: struct {
        target_actor: ActorObject.Ptr,
        method: MethodObject.Ptr,
    },

    pub const nothing = LookupResult{ .Nothing = {} };
};
