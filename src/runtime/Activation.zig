// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("./Actor.zig");
const Value = @import("./value.zig").Value;
const Executable = @import("./astcode/Executable.zig");
const SourceRange = @import("./SourceRange.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const RegisterLocation = @import("./astcode/register_location.zig").RegisterLocation;

/// The ID of the activation which is used with ActivationRef in order to check
/// whether the activation is still alive or not.
activation_id: u64,
/// The activation object which holds the receiver for this activation and the
/// arguments + assignable slots defined on the method or block. This is the
/// response to the "self" object in an activation.
activation_object: Value,
/// The location to which the result of the activation (obtained via the
/// exit_activation instruction) is written.
target_location: RegisterLocation,
/// This is the parent activation for the current block activation. The
/// activation object of this activation will be used as the receiver of the
/// block's own activation object. Not used with methods, since we don't want to
/// inherit previous activation objects in methods (that would make the language
/// dynamically scoped :^).
parent_activation: ?*Self = null,
/// Will be used as the target activation that a non-local return needs to rise
/// to. Must be non-null when a non-local return is encountered, and when
/// non-null, must point to an activation where
/// `nonlocal_return_target_activation` is null.
nonlocal_return_target_activation: ?*Self = null,

// --- Activation creation info ---

/// The message that created this activation as a byte array.
creator_message: Value,
/// This is the source range which caused the creation of this message.
created_from: SourceRange,

// --- Activation execution state ---

/// The registers on which the instructions operate. This slice is owned and
/// will be destroyed when the activation is deinitialized.
registers: []Value,
/// The index of the instruction that is currently being executed (the "program
/// counter").
pc: u32 = 0,

const Self = @This();

/// Creates a copy of `created_from`.
pub fn initInPlace(
    self: *Self,
    activation_object: Value,
    target_location: RegisterLocation,
    registers: []Value,
    creator_message: Value,
    created_from: SourceRange,
) !void {
    std.debug.assert(activation_object.isObjectReference());

    self.* = .{
        .activation_id = newActivationID(),
        .activation_object = activation_object,
        .target_location = target_location,
        .creator_message = creator_message,
        .created_from = created_from.copy(),
        .registers = registers,
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.created_from.deinit();
    allocator.free(self.registers);
}

pub fn takeRef(self: *Self) ActivationRef {
    return ActivationRef.init(self);
}

/// Return the result of the `self` message for the current context.
pub fn selfObject(self: Self) Value {
    return self.activation_object;
}

/// Return the executable that this activation was created from.
pub fn creationExecutable(self: Self) Executable.Ref {
    return self.created_from.executable;
}

/// Return the executable that this activation's method or block is defined in.
pub fn definitionExecutable(self: Self) Executable.Ref {
    const activation_object = self.activation_object.asObject().asActivationObject();
    return activation_object.getDefinitionExecutable();
}

pub fn advanceInstruction(self: *Self) void {
    self.pc += 1;
}

/// Resets the instruction index of this activation.
pub fn restart(self: *Self) void {
    self.pc = 0;
}

pub fn format(
    activation: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    const creator_message_byte_array = activation.creator_message.asByteArray();
    try std.fmt.format(writer, "Activation{{ '{s}' created from {}, pc = {}, target_location = {} }}", .{
        creator_message_byte_array.getValues(),
        activation.created_from,
        activation.pc,
        activation.target_location,
    });
}

pub const ActivationStack = struct {
    stack: []Self,
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

    pub fn getCurrent(self: ActivationStack) *Self {
        std.debug.assert(self.depth > 0);
        return &self.stack[self.depth - 1];
    }

    /// Unwinds the stack until the given activation is reached.
    pub fn restoreTo(self: *ActivationStack, allocator: Allocator, activation: *Self) void {
        if (std.debug.runtime_safety) {
            std.debug.assert(self.isActivationWithin(activation));
        }

        const current_activation = self.getCurrent();
        std.debug.assert(@ptrToInt(current_activation) >= @ptrToInt(activation));

        const distance = @divExact(@ptrToInt(current_activation) - @ptrToInt(activation), @sizeOf(Self));
        const target_depth = self.depth - distance;
        while (self.depth != target_depth) : (self.depth -= 1) {
            self.stack[self.depth - 1].deinit(allocator);
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

    pub fn setRegisters(self: *ActivationStack, vm: *VirtualMachine) void {
        vm.registers = self.getCurrent().registers;
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

    pub fn isAlive(self: ActivationRef, stack: *ActivationStack) bool {
        const activation_ptr = self.getPointer();

        // Is this activation outside the currently-valid activation stack?
        if (!stack.isActivationWithin(activation_ptr)) return false;
        // Does the ID we saved match the currently-stored ID on the activation
        // we point to?
        if (self.saved_id.asUnsignedInteger() != activation_ptr.activation_id) return false;

        // It's alive.
        return true;
    }

    pub fn get(self: ActivationRef, stack: *ActivationStack) ?*Self {
        return if (self.isAlive(stack)) self.getPointer() else null;
    }
};

// FIXME: This isn't thread safe!
var id: u64 = 0;

pub fn newActivationID() u64 {
    id += 1;
    return id;
}
