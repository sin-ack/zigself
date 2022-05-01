// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../debug.zig");
const Value = @import("./value.zig").Value;
const Completion = @import("./Completion.zig");
const Executable = @import("./bytecode/Executable.zig");
const Activation = @import("./Activation.zig");
const interpreter = @import("./interpreter.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const ActivationStack = Activation.ActivationStack;

const ACTIVATION_EXIT_DEBUG = debug.ACTIVATION_EXIT_DEBUG;

activation_stack: ActivationStack,

const Self = @This();

pub const MaximumStackDepth = 2048;

pub fn create(allocator: Allocator) !*Self {
    const activation_stack = try ActivationStack.init(allocator, MaximumStackDepth);
    errdefer activation_stack.deinit(allocator);

    const self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    try self.init(activation_stack);
    return self;
}

pub fn destroy(self: *Self, allocator: Allocator) void {
    self.deinit(allocator);
    allocator.destroy(self);
}

fn init(self: *Self, activation_stack: ActivationStack) !void {
    self.activation_stack = activation_stack;
}

fn deinit(self: *Self, allocator: Allocator) void {
    for (self.activation_stack.getStack()) |*activation| {
        activation.deinit(allocator);
    }
    self.activation_stack.deinit(allocator);
}

pub fn execute(self: *Self, vm: *VirtualMachine, executable: Executable.Ref) !Completion {
    // FIXME: This won't work once we have more than one actor running.
    vm.heap.setActivationStack(&self.activation_stack);
    defer vm.heap.setActivationStack(null);

    try executable.value.pushEntrypointActivation(vm, &self.activation_stack);
    return try self.executeActivationStack(vm, null);
}

/// Execute the activation stack of this actor until the given activation (or if
/// `until` is null, until all activations have been resolved).
pub fn executeActivationStack(self: *Self, vm: *VirtualMachine, until: ?*Activation) !Completion {
    while (true) {
        // FIXME: Avoid re-fetching these everytime.
        var activation = self.activation_stack.getCurrent();
        const activation_object = activation.activation_object.asObject().asActivationObject();
        const executable = activation_object.getDefinitionExecutable();
        const block = activation_object.getBytecodeBlock();
        const opcode = block.getOpcode(activation.pc);

        if (try interpreter.execute(vm, self, until, executable, opcode.*)) |completion| {
            switch (completion.data) {
                // If a normal completion is returned, then the last activation
                // has been exited and a result is reached.
                .Normal => return completion,
                // TODO remove this
                .NonlocalReturn => unreachable,
                .Restart => activation.restart(),
                .RuntimeError => return completion,
            }
        } else {
            activation.advanceInstruction();
        }
    }

    unreachable;
}

pub const ActivationExitState = enum { LastActivation, NotLastActivation };

pub fn exitCurrentActivation(self: *Self, vm: *VirtualMachine, last_activation: ?*Activation, value: Value) ActivationExitState {
    if (ACTIVATION_EXIT_DEBUG) std.debug.print("Actor.exitCurrentActivation: Exiting this activation\n", .{});
    const current_activation = self.activation_stack.getCurrent();
    return self.exitActivation(vm, last_activation, current_activation, value);
}

/// Exit the given activation with the given value and write it to the register
/// for the opcode that initiated the activation. Returns
/// ActivationExitState.LastActivation if the last activation has been exited.
pub fn exitActivation(self: *Self, vm: *VirtualMachine, last_activation: ?*Activation, target_activation: *Activation, value: Value) ActivationExitState {
    if (ACTIVATION_EXIT_DEBUG) {
        std.debug.print("Actor.exitActivation: Exiting activation\n", .{});
        for (self.activation_stack.getStack()) |*a, i| {
            const pointer = if (a == target_activation)
                @as(u8, '>')
            else if (a == last_activation)
                @as(u8, '-')
            else
                @as(u8, ' ');
            std.debug.print(" {c} #{} {}\n", .{ pointer, i, a });
        }
    }

    const target_location = target_activation.target_location;

    // Restore to the given activation + 1 more to exit this one
    self.activation_stack.restoreTo(vm.allocator, target_activation);
    self.activation_stack.popActivation().deinit(vm.allocator);

    if (last_activation) |last| {
        if (self.activation_stack.getCurrent() == last) {
            self.activation_stack.setRegisters(vm);
            return .LastActivation;
        }
    } else {
        if (self.activation_stack.depth == 0)
            return .LastActivation;
    }

    self.activation_stack.setRegisters(vm);
    vm.writeRegister(target_location, value);
    return .NotLastActivation;
}
