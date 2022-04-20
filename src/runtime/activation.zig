// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Script = @import("../language/script.zig");
const SourceRange = @import("../language/source_range.zig");

const InterpreterContext = @import("./interpreter.zig").InterpreterContext;

const Self = @This();

// FIXME: This isn't thread safe!
var id: u64 = 0;

pub fn newActivationID() u64 {
    id += 1;
    return id;
}

/// Used for keeping track of the message that was sent to start the current
/// activation. This is then used in stack traces.
pub const ActivationCreationContext = struct {
    should_untrack_message_name_on_deinit: bool,
    message: Heap.Tracked,
    source_range: SourceRange,
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

    pub fn getCurrent(self: ActivationStack) ?*Self {
        if (self.depth == 0) return null;
        return &self.stack[self.depth - 1];
    }

    /// Unwinds the stack until the given activation is reached.
    pub fn restoreTo(self: *ActivationStack, activation: *Self) void {
        if (std.debug.runtime_safety) {
            std.debug.assert(self.isActivationWithin(activation));
        }

        const current_activation = self.getCurrent().?;
        std.debug.assert(@ptrToInt(current_activation) >= @ptrToInt(activation));

        const distance = @divExact(@ptrToInt(current_activation) - @ptrToInt(activation), @sizeOf(Self));
        const target_depth = self.depth - distance;
        while (self.depth != target_depth) : (self.depth -= 1) {
            self.stack[self.depth - 1].deinit();
        }
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

    pub fn isActivationWithin(self: ActivationStack, activation: *Self) bool {
        const start_of_slice = self.stack.ptr;
        const end_of_slice = start_of_slice + self.depth;

        return @ptrToInt(activation) >= @ptrToInt(start_of_slice) and
            @ptrToInt(activation) < @ptrToInt(end_of_slice);
    }
};

/// A reference to an activation. The pointer and saved ID values are stored as
/// Values, which makes this struct object heap-safe.
pub const ActivationRef = packed struct {
    pointer: Value,
    saved_id: Value,

    pub fn init(activation: *Self) ActivationRef {
        const ptr = @ptrToInt(activation);
        return .{
            .pointer = Value.fromUnsignedInteger(ptr),
            .saved_id = Value.fromUnsignedInteger(activation.activation_id),
        };
    }

    fn getPointer(self: ActivationRef) *Self {
        return @intToPtr(*Self, self.pointer.asUnsignedInteger());
    }

    pub fn isAlive(self: ActivationRef, context: *InterpreterContext) bool {
        const activation_ptr = self.getPointer();

        // Is this activation outside the currently-valid activation stack?
        if (!context.activation_stack.isActivationWithin(activation_ptr)) return false;
        // Does the ID we saved match the currently-stored ID on the activation
        // we point to?
        if (self.saved_id.asUnsignedInteger() != activation_ptr.activation_id) return false;

        // It's alive.
        return true;
    }

    pub fn get(self: ActivationRef, context: *InterpreterContext) ?*Self {
        return if (self.isAlive(context)) self.getPointer() else null;
    }
};

activation_id: u64,
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

// --- Activation execution state ---

/// The index of the statement that is currently being executed. This is an
/// offset into the statement slice of the activation object.
statement_index: usize = 0,

pub fn initInPlace(
    self: *Self,
    heap: *Heap,
    activation_object: Value,
    creator_message: Heap.Tracked,
    source_range: SourceRange,
    should_untrack_message_name_on_deinit: bool,
) !void {
    std.debug.assert(activation_object.isObjectReference());

    self.* = Self{
        .activation_id = newActivationID(),
        .heap = heap,
        .activation_object = activation_object,
        .creation_context = .{
            .should_untrack_message_name_on_deinit = should_untrack_message_name_on_deinit,
            .message = creator_message,
            .source_range = source_range.copy(),
        },
    };
}

pub fn deinit(self: *Self) void {
    self.creation_context.source_range.deinit();

    if (self.creation_context.should_untrack_message_name_on_deinit) {
        self.creation_context.message.untrack(self.heap);
    }
}

pub fn takeRef(self: *Self) ActivationRef {
    return ActivationRef.init(self);
}

/// Return the result of the `self` message for the current context.
pub fn selfObject(self: *Self) Value {
    return self.activation_object;
}

/// Return the script that this activation's method or block is defined in. In
/// order to keep a reference the caller should ref the script.
pub fn script(self: *Self) Script.Ref {
    return self.creation_context.source_range.script;
}
