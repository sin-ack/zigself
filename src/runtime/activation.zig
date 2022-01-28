// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("./value.zig").Value;
const Script = @import("../language/script.zig");
const Range = @import("../language/location_range.zig");
const weak_ref = @import("../utility/weak_ref.zig");

const Self = @This();
const WeakBlock = weak_ref.WeakPtrBlock(Self);
pub const Weak = weak_ref.WeakPtr(Self);

/// Used for keeping track of the message that was sent to start the current
/// activation. This is then used in stack traces.
pub const ActivationCreationContext = struct {
    message: []const u8,
    script: Script.Ref,
    range: Range,
};

allocator: Allocator,
activation_object: Value,
creation_context: ActivationCreationContext,
/// Will be used as the target activation that a non-local return needs to rise
/// to. Must be non-null when a non-local return is encountered, and when
/// non-null, must point to an activation where
/// `nonlocal_return_target_activation` is null.
nonlocal_return_target_activation: ?*Self = null,
/// This is the parent activation for the current block activation. The
/// activation object of this activation will be used as the parent of the
/// block's activation. Not used with methods, since we don't want to inherit
/// previous activation objects in methods (that would make the language
/// dynamically scoped :^).
parent_activation: ?*Self = null,
/// Used for bound activations of blocks.
weak: WeakBlock,

/// Creates a new activation.
/// Borrows a ref for `activation_object` from the caller.
/// Takes ownership of `creator_message`.
/// Borrows a ref for `creator_script` from the caller.
pub fn create(
    allocator: Allocator,
    activation_object: Value,
    creator_message: []const u8,
    creator_range: Range,
    creator_script: Script.Ref,
) !*Self {
    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    try self.init(allocator, activation_object, creator_message, creator_script, creator_range);

    return self;
}

pub fn destroy(self: *Self) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn init(
    self: *Self,
    allocator: Allocator,
    activation_object: Value,
    creator_message: []const u8,
    creator_script: Script.Ref,
    creator_range: Range,
) !void {
    std.debug.assert(activation_object.isObjectReference());

    self.* = Self{
        .weak = try WeakBlock.init(allocator, self),
        .allocator = allocator,
        .activation_object = activation_object,
        .creation_context = .{
            .message = creator_message,
            .script = creator_script,
            .range = creator_range,
        },
    };
}

fn deinit(self: *Self) void {
    self.weak.deinit();
    self.creation_context.script.unref();
    self.allocator.free(self.creation_context.message);
}

/// Make a weak reference to this activation.
pub fn makeWeakRef(self: *Self) Weak {
    return Weak.init(self);
}
