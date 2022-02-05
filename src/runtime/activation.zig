// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
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
    should_untrack_message_name_on_deinit: bool,
    message: Heap.Tracked,
    script: Script.Ref,
    range: Range,
};

pub const ActivationStack = struct {
    stack: []Self = &[_]Self{},
    depth: usize = 0,

    pub fn init(allocator: Allocator, max_depth: usize) !ActivationStack {
        const stack = try allocator.alloc(Self, max_depth);

        return ActivationStack{ .stack = stack };
    }

    pub fn deinit(self: ActivationStack, allocator: Allocator) void {
        allocator.free(self.stack);
    }

    pub fn getStack(self: ActivationStack) []Self {
        return self.stack[0..self.depth];
    }

    pub fn getNewActivationSlot(self: *ActivationStack) *Self {
        // NOTE: Will trigger a crash if maximum stack depth was exceeded.
        const activation = &self.stack[self.depth];
        self.depth += 1;
        return activation;
    }

    pub fn popActivation(self: *ActivationStack) *Self {
        self.depth -= 1;
        const activation = &self.stack[self.depth];
        return activation;
    }
};

allocator: Allocator,
heap: *Heap,
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

pub fn initInPlace(
    self: *Self,
    allocator: Allocator,
    heap: *Heap,
    activation_object: Value,
    creator_message: Heap.Tracked,
    creator_range: Range,
    creator_script: Script.Ref,
    should_untrack_message_name_on_deinit: bool,
) !void {
    std.debug.assert(activation_object.isObjectReference());

    self.* = Self{
        .weak = try WeakBlock.init(allocator, self),
        .allocator = allocator,
        .heap = heap,
        .activation_object = activation_object,
        .creation_context = .{
            .should_untrack_message_name_on_deinit = should_untrack_message_name_on_deinit,
            .message = creator_message,
            .script = creator_script,
            .range = creator_range,
        },
    };
}

pub fn deinit(self: *Self) void {
    self.weak.deinit();
    self.creation_context.script.unref();

    if (self.creation_context.should_untrack_message_name_on_deinit) {
        self.creation_context.message.untrack(self.heap);
    }
}

/// Make a weak reference to this activation.
pub fn makeWeakRef(self: *Self) Weak {
    return Weak.init(self);
}
