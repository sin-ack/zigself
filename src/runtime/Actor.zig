// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../debug.zig");
const Value = @import("./value.zig").Value;
const Completion = @import("./Completion.zig");
const Executable = @import("./lowcode/Executable.zig");
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
        activation.deinit();
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
        const inst = block.getInstruction(activation.pc);

        if (try interpreter.execute(vm, self, until, executable, inst.*)) |completion| {
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

pub fn exitCurrentActivation(self: *Self, vm: *VirtualMachine, last_activation: ?*Activation) ActivationExitState {
    if (ACTIVATION_EXIT_DEBUG) std.debug.print("Actor.exitCurrentActivation: Exiting this activation\n", .{});
    const current_activation = self.activation_stack.getCurrent();
    return self.exitActivation(vm, last_activation, current_activation);
}

/// Exit the given activation and write the result of the return register to the
/// register for the instruction that initiated the activation. Returns
/// ActivationExitState.LastActivation if the last activation has been exited.
pub fn exitActivation(self: *Self, vm: *VirtualMachine, last_activation: ?*Activation, target_activation: *Activation) ActivationExitState {
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
    self.activation_stack.restoreTo(target_activation);
    const activation_stack_snapshot = self.activation_stack.getCurrent().stack_snapshot;
    self.activation_stack.popActivation().deinit();

    // Restore each register until the current activation's saved register stack height
    // FIXME: Factor this out
    const activation_saved_register_height = activation_stack_snapshot.saved_register_height;
    const saved_register_stack = vm.saved_register_stack.allItems();
    const saved_register_count = saved_register_stack.len;
    for (saved_register_stack[activation_saved_register_height..]) |_, i| {
        const saved_register = saved_register_stack[saved_register_count - 1 - i];
        vm.writeRegister(saved_register.register, saved_register.value);
    }

    // Restore the stack snapshot of the activation we just exited
    vm.restoreStackSnapshot(activation_stack_snapshot);

    if (last_activation) |last| {
        if (self.activation_stack.getCurrent() == last) {
            return .LastActivation;
        }
    } else {
        if (self.activation_stack.depth == 0)
            return .LastActivation;
    }

    vm.writeRegister(target_location, vm.readRegister(.ret));
    return .NotLastActivation;
}
