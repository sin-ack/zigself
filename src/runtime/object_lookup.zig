// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const hash = @import("../utility/hash.zig");
const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const ActorObject = @import("objects/actor.zig").Actor;
const MethodObject = @import("objects/method.zig").Method;
const VirtualMachine = @import("VirtualMachine.zig");

pub const VisitedValueLink = struct { previous: ?*const VisitedValueLink = null, value: Value };

// Well-known hashes
pub const self_hash = hash.stringHash("self");
pub const parent_hash = hash.stringHash("parent");
pub const value_hash = hash.stringHash("value");

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

/// The hash information of a selector.
pub const SelectorHash = struct {
    /// The hash of the selector itself.
    regular: u32,
    /// If this is not null, then this selector is of the form `foo:` and this
    /// hash is the hash of `foo`. It is intended to be compared with assignable
    /// slots in assignment lookups.
    assignment_target: ?u32,

    pub fn init(selector: []const u8) SelectorHash {
        const regular_hash = hash.stringHash(selector);
        // If the only : in this selector is at the end, then compute an
        // assignment target hash for it.
        const assignment_target_hash = if (std.mem.indexOfScalar(u8, selector, ':') == selector.len - 1)
            hash.stringHash(selector[0 .. selector.len - 1])
        else
            null;

        return .{ .regular = regular_hash, .assignment_target = assignment_target_hash };
    }

    pub fn lookupObject(self: SelectorHash, vm: *VirtualMachine, object: Object.Ptr) LookupResult {
        return self.chainedLookupObject(vm, object, null);
    }

    pub fn chainedLookupObject(self: SelectorHash, vm: *VirtualMachine, object: Object.Ptr, previously_visited: ?*const VisitedValueLink) LookupResult {
        return object.lookup(vm, self, previously_visited);
    }
};
