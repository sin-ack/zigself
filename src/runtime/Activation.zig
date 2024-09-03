// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("./Actor.zig");
const value = @import("./value.zig");
const Value = value.Value;
const context = @import("context.zig");
const bytecode = @import("./bytecode.zig");
const SourceRange = @import("./SourceRange.zig");
const IntegerValue = value.IntegerValue;
const MethodObject = @import("objects/method.zig").Method;
const ActivationObject = @import("objects/activation.zig").Activation;

/// The ID of the activation which is used with ActivationRef in order to check
/// whether the activation is still alive or not.
activation_id: u64,
/// The activation object which holds the receiver for this activation and the
/// arguments + assignable slots defined on the method or block. This is the
/// response to the "self" object in an activation.
activation_object: ActivationObject.Value,
/// The location to which the result of the activation (obtained via the
/// exit_activation instruction) is written.
target_location: bytecode.RegisterLocation,
/// This is the parent activation for the current block activation. The
/// activation object of this activation will be used as the receiver of the
/// block's own activation object. Not used with methods, since we don't want to
/// inherit previous activation objects in methods (that would make the language
/// dynamically scoped :^).
parent_activation: ?ActivationRef = null,
/// Will be used as the target activation that a non-local return needs to rise
/// to. Must be non-null when a non-local return is encountered, and when
/// non-null, must point to an activation where
/// `nonlocal_return_target_activation` is null.
nonlocal_return_target_activation: ?ActivationRef = null,

// --- Activation creation info ---

/// The VM stack snapshot at the time which this activation was created.
stack_snapshot: Actor.StackSnapshot,
/// The message that created this activation as a byte array.
creator_message: Value,
/// This is the source range which caused the creation of this message.
created_from: SourceRange,

// --- Activation execution state ---

/// The index of the instruction that is currently being executed (the "program
/// counter").
instruction_index: u32 = 0,

const Activation = @This();

/// Creates a copy of `created_from`.
pub fn initInPlace(
    self: *Activation,
    activation_object: ActivationObject.Value,
    target_location: bytecode.RegisterLocation,
    creator_message: Value,
    created_from: SourceRange,
) void {
    self.* = .{
        .activation_id = newActivationID(),
        .activation_object = activation_object,
        .target_location = target_location,
        .stack_snapshot = context.getVM().takeStackSnapshot(),
        .creator_message = creator_message,
        .created_from = created_from.copy(),
    };
}

pub fn deinit(self: *Activation) void {
    self.created_from.deinit();
}

pub fn takeRef(self: *Activation, stack: ActivationStack) ActivationRef {
    return ActivationRef.init(self, stack);
}

/// Return the result of the `self` message for the current context.
pub fn selfObject(self: Activation) ActivationObject.Value {
    return self.activation_object;
}

/// Return the executable that this activation was created from.
pub fn creationExecutable(self: Activation) bytecode.Executable.Ref {
    return self.created_from.executable;
}

/// Return the executable that this activation's method or block is defined in.
pub fn definitionExecutable(self: Activation) bytecode.Executable.Ref {
    return self.activation_object.get().getDefinitionExecutable();
}

pub fn advanceInstruction(self: *Activation) u32 {
    self.instruction_index += 1;
    return self.instruction_index;
}

/// Resets the instruction index of this activation, and returns the new value.
pub fn restart(self: *Activation) u32 {
    self.instruction_index = 0;
    return 0;
}

pub fn format(
    activation: Activation,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    const creator_message_byte_array = activation.creator_message.asByteArray().?;
    try std.fmt.format(writer, "Activation{{ '{s}' created from {}, instruction_index = {}, target_location = {} }}", .{
        creator_message_byte_array.getValues(),
        activation.created_from,
        activation.instruction_index,
        activation.target_location,
    });
}

pub const ActivationStack = struct {
    stack: std.ArrayListUnmanaged(Activation) = .{},

    pub fn deinit(self: *ActivationStack, allocator: Allocator) void {
        self.stack.deinit(allocator);
    }

    pub fn getStack(self: ActivationStack) []Activation {
        return self.stack.items;
    }

    pub fn getDepth(self: ActivationStack) usize {
        return self.stack.items.len;
    }

    pub fn getCurrent(self: ActivationStack) *Activation {
        const depth = self.getDepth();
        std.debug.assert(depth > 0);
        return &self.stack.items[depth - 1];
    }

    /// Unwinds the stack until the given activation is reached.
    pub fn restoreTo(self: *ActivationStack, activation: *Activation) void {
        if (std.debug.runtime_safety) {
            std.debug.assert(self.isActivationWithin(activation));
        }

        const current_activation = self.getCurrent();
        std.debug.assert(@intFromPtr(current_activation) >= @intFromPtr(activation));

        const distance = @divExact(@intFromPtr(current_activation) - @intFromPtr(activation), @sizeOf(Activation));
        var current_depth = self.getDepth();
        const target_depth = current_depth - distance;
        while (current_depth != target_depth) : (current_depth -= 1) {
            self.stack.items[current_depth - 1].deinit();
        }

        self.stack.shrinkRetainingCapacity(target_depth);
    }

    pub fn getNewActivationSlot(self: *ActivationStack, allocator: Allocator) !*Activation {
        try self.stack.ensureUnusedCapacity(allocator, 1);
        self.stack.items.len += 1;
        const activation = &self.stack.items[self.getDepth() - 1];
        return activation;
    }

    pub fn popActivation(self: *ActivationStack) *Activation {
        const new_depth = self.getDepth() - 1;

        // NOTE: We intentionally return the activation that's out of the
        //       bounds of the stack. This value is used very briefly.
        const activation = &self.stack.items[new_depth];
        self.stack.shrinkRetainingCapacity(new_depth);

        return activation;
    }

    pub fn clear(self: *ActivationStack) void {
        self.stack.clearRetainingCapacity();
    }

    fn isActivationWithin(self: ActivationStack, activation: *Activation) bool {
        const start_of_slice = self.stack.items.ptr;
        const end_of_slice = start_of_slice + self.stack.items.len;

        return @intFromPtr(activation) >= @intFromPtr(start_of_slice) and
            @intFromPtr(activation) < @intFromPtr(end_of_slice);
    }

    pub fn offsetOf(self: ActivationStack, activation: *Activation) usize {
        std.debug.assert(self.isActivationWithin(activation));

        const start_of_slice = self.stack.items.ptr;
        return @divExact(@intFromPtr(activation) - @intFromPtr(start_of_slice), @sizeOf(Activation));
    }

    pub fn pushEntrypointActivation(self: *ActivationStack, new_executable: bytecode.Executable.Ref) !void {
        try self.pushEntrypointActivationInner(.zero, new_executable, new_executable);
    }

    /// Pushes an entrypoint activation for this executable, with the creation
    /// context pointing at another executable. This is used when executing
    /// scripts by _RunScript, for example.
    pub fn pushSubEntrypointActivation(
        self: *ActivationStack,
        target_location: bytecode.RegisterLocation,
        new_executable: bytecode.Executable.Ref,
    ) !void {
        std.debug.assert(self.getDepth() > 0);
        try self.pushEntrypointActivationInner(target_location, self.getCurrent().definitionExecutable(), new_executable);
    }

    fn pushEntrypointActivationInner(
        self: *ActivationStack,
        target_location: bytecode.RegisterLocation,
        current_executable: bytecode.Executable.Ref,
        new_executable: bytecode.Executable.Ref,
    ) !void {
        const source_range = SourceRange.initNoRef(current_executable, .{ .start = 0, .end = 1 });

        const vm = context.getVM();
        var token = try vm.heap.getAllocation(
            MethodObject.requiredSizeForCreatingTopLevelContext() +
                ActivationObject.requiredSizeForAllocation(0, 0),
        );
        defer token.deinit();

        const toplevel_context_method = try MethodObject.createTopLevelContextForExecutable(&token, new_executable, new_executable.value.getEntrypointBlock());
        const activation_slot = try self.getNewActivationSlot(vm.allocator);
        toplevel_context_method.activateMethod(&token, context.getActor().id, vm.lobby(), &.{}, target_location, source_range, activation_slot);
    }
};

/// A reference to an activation. The pointer and saved ID values are stored as
/// Values, which makes this struct object heap-safe.
pub const ActivationRef = packed struct {
    offset: IntegerValue(.Unsigned),
    saved_id: IntegerValue(.Unsigned),

    pub fn init(activation: *Activation, stack: ActivationStack) ActivationRef {
        return .{
            .offset = IntegerValue(.Unsigned).init(@intCast(stack.offsetOf(activation))),
            .saved_id = IntegerValue(.Unsigned).init(@intCast(activation.activation_id)),
        };
    }

    fn getPointer(self: ActivationRef, stack: ActivationStack) *Activation {
        return &stack.stack.items[@intCast(self.offset.get())];
    }

    pub fn isAlive(self: ActivationRef, stack: ActivationStack) bool {
        const offset = self.offset.get();

        // Is this activation outside the currently-valid activation stack?
        if (stack.getDepth() <= offset) return false;

        const activation_ptr = self.getPointer(stack);
        // Does the ID we saved match the currently-stored ID on the activation
        // we point to?
        if (self.saved_id.get() != activation_ptr.activation_id) return false;

        // It's alive.
        return true;
    }

    pub fn get(self: ActivationRef, stack: ActivationStack) ?*Activation {
        return if (self.isAlive(stack)) self.getPointer(stack) else null;
    }
};

// FIXME: This isn't thread safe!
var id: u64 = 0;

pub fn newActivationID() u64 {
    id += 1;
    return id;
}
