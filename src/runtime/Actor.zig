// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("./slot.zig").Slot;
const debug = @import("../debug.zig");
const Value = @import("./value.zig").Value;
const Stack = @import("./stack.zig").Stack;
const Range = @import("../language/Range.zig");
const Object = @import("./Object.zig");
const Completion = @import("./Completion.zig");
const Executable = @import("./lowcode/Executable.zig");
const Activation = @import("./Activation.zig");
const interpreter = @import("./interpreter.zig");
const SourceRange = @import("./SourceRange.zig");
const RegisterFile = @import("./lowcode/RegisterFile.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const ActivationStack = Activation.ActivationStack;
const RegisterLocation = @import("./lowcode/register_location.zig").RegisterLocation;

const ACTIVATION_EXIT_DEBUG = debug.ACTIVATION_EXIT_DEBUG;

// The actor object that this actor is represented by in Self code.
actor_object: Value,

entrypoint_selector: ?Value = null,
yield_reason: YieldReason = .None,
activation_stack: ActivationStack,

/// The mailbox stores the messages that were sent to this actor through actor
/// proxy objects.
mailbox: Mailbox = .{},

register_file: RegisterFile = .{},
argument_stack: Stack(Value, "Argument stack", ValueSentinel) = .{},
slot_stack: Stack(Slot, "Slot stack", SlotSentinel) = .{},
saved_register_stack: Stack(SavedRegister, "Saved register stack", null) = .{},

/// Whether the next created method is going to be an inline method.
next_method_is_inline: bool = false,

/// The currently active source range. This is updated by the source_range
/// instruction.
range: Range = .{ .start = 0, .end = 0 },

const Self = @This();
const Mailbox = std.TailQueue(Message);

// Sentinel values for the stacks
pub const ValueSentinel = Value{ .data = 0xCCCCCCCCCCCCCCCC };
pub const SlotSentinel = Slot{ .name = ValueSentinel, .properties = .{ .properties = ValueSentinel }, .value = ValueSentinel };

pub const MaximumStackDepth = 2048;

/// A snapshot of the current heights of each stack in the VM which can be
/// restored after a non-local return.
pub const StackSnapshot = struct {
    argument_height: usize,
    slot_height: usize,
    saved_register_height: usize,

    /// Bump just the argument stack height. This is necessary because the stack
    /// snapshot for an activation is created while the stack still contains the
    /// arguments for the activation, meaning the stack will be higher than it
    /// actually is when the activation is entered.
    pub fn bumpArgumentHeight(self: *StackSnapshot, actor: *Self) void {
        self.argument_height = actor.argument_stack.height();
    }
};

/// A saved register, which is restored at activation exit.
pub const SavedRegister = struct {
    /// The register to restore the value to.
    register: RegisterLocation,
    /// The value which should be restored.
    value: Value,
};

/// The result of actor execution.
pub const ActorResult = union(enum) {
    ActorSwitch,
    Completion: Completion,
};

/// The reason this actor has yielded.
// NOTE: Keep in sync with objects/actor.self!
pub const YieldReason = enum(u32) {
    /// This actor hasn't yielded.
    None = 0,
    /// The actor has received a runtime error.
    RuntimeError = 1,
    /// The actor has blocked on a primitive.
    Blocked = 2,
    /// The _ActorYield primitive was sent.
    Yielded = 3,
    /// The actor has finished its execution normally.
    Dead = 4,
    /// The actor has sent _ActorSpawn: which has spawned another actor. The
    /// return value of _ActorResume will be the newly spawned actor object.
    ActorSpawned = 5,
};

/// A single message sent to this actor from another actor.
pub const Message = struct {
    /// The actor object that sent this message.
    sender: Value,
    /// The method that will be executed on the actor context. Belongs to this
    /// actor.
    method: Value,
    /// The arguments of this message as an owned slice.
    arguments: []Value,
    /// The SourceRange which spawned this message.
    source_range: SourceRange,

    pub fn deinit(self: *Message, allocator: Allocator) void {
        allocator.free(self.arguments);
    }
};

/// Spawn a new actor, activating the given method on it.
pub fn spawn(
    vm: *VirtualMachine,
    actor_context: Value,
    method: *Object.Method,
    source_range: SourceRange,
    target_location: RegisterLocation,
) !*Self {
    std.debug.assert(method.getArgumentSlotCount() == 0);

    const self = try create(vm, actor_context);
    errdefer self.destroy(vm.allocator);

    const new_activation = self.activation_stack.getNewActivationSlot();
    try method.activateMethod(vm, actor_context, &.{}, target_location, source_range, new_activation);

    return self;
}

pub fn create(vm: *VirtualMachine, actor_context: Value) !*Self {
    const activation_stack = try ActivationStack.init(vm.allocator, MaximumStackDepth);
    errdefer activation_stack.deinit(vm.allocator);

    const self = try vm.allocator.create(Self);
    errdefer vm.allocator.destroy(self);

    const actor_object = try Object.Actor.create(vm.heap, self, actor_context);

    self.init(activation_stack, actor_object);
    return self;
}

pub fn destroy(self: *Self, allocator: Allocator) void {
    self.deinit(allocator);
    allocator.destroy(self);
}

fn init(self: *Self, activation_stack: ActivationStack, actor_object: *Object.Actor) void {
    self.* = .{
        .activation_stack = activation_stack,
        .actor_object = actor_object.asValue(),
    };

    self.register_file.init();
}

fn deinit(self: *Self, allocator: Allocator) void {
    self.clearMailbox(allocator);

    self.argument_stack.deinit(allocator);
    self.slot_stack.deinit(allocator);
    self.saved_register_stack.deinit(allocator);

    for (self.activation_stack.getStack()) |*activation| {
        activation.deinit();
    }
    self.activation_stack.deinit(allocator);
}

pub fn execute(self: *Self, vm: *VirtualMachine) !ActorResult {
    const actor_context = self.actor_object.asObject().asActorObject().context;
    const current_activation = self.activation_stack.getCurrent();

    // Go through the mailbox and activate all the messages that have been sent
    // so far.
    {
        var it = self.mailbox.first;
        while (it) |node| : (it = node.next) {
            const method = node.data.method.asObject().asMethodObject();

            const new_activation = self.activation_stack.getNewActivationSlot();
            try method.activateMethod(vm, actor_context, node.data.arguments, .zero, node.data.source_range, new_activation);

            switch (try self.executeUntil(vm, current_activation)) {
                .ActorSwitch => {
                    return ActorResult{
                        .Completion = try Completion.initRuntimeError(vm, node.data.source_range, "Actor message caused actor switch", .{}),
                    };
                },
                .Completion => |completion| switch (completion.data) {
                    .Normal => {},
                    .RuntimeError => return ActorResult{ .Completion = completion },
                    else => unreachable,
                },
            }

            std.debug.assert(self.activation_stack.getCurrent() == current_activation);
        }
    }

    self.clearMailbox(vm.allocator);

    // Execute the activation stack of this actor normally.
    return try self.executeUntil(vm, null);
}

/// Execute the activation stack of this actor until the given activation (or if
/// `until` is null, until all activations have been resolved).
pub fn executeUntil(self: *Self, vm: *VirtualMachine, until: ?*Activation) !ActorResult {
    var activation = self.activation_stack.getCurrent();
    var activation_object = activation.activation_object.asObject().asActivationObject();
    var executable = activation_object.getDefinitionExecutable();
    var block = activation_object.getBytecodeBlock();

    while (true) {
        const inst = block.getInstruction(activation.pc);
        const execution_result = try interpreter.execute(vm, self, until, executable, inst.*);
        switch (execution_result) {
            .ActorSwitch => {
                return ActorResult{ .ActorSwitch = {} };
            },
            .ActivationChange => {
                activation = self.activation_stack.getCurrent();
                activation_object = activation.activation_object.asObject().asActivationObject();
                executable = activation_object.getDefinitionExecutable();
                block = activation_object.getBytecodeBlock();
            },
            .Completion => |completion| {
                switch (completion.data) {
                    // If a normal completion is returned, then the last activation
                    // has been exited and a result is reached.
                    .Normal, .RuntimeError => return ActorResult{ .Completion = completion },
                    .Restart => activation.restart(),
                }
            },
            .Success => {
                activation.advanceInstruction();
            },
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

    if (last_activation) |last| {
        if (self.activation_stack.getCurrent() == last) {
            return .LastActivation;
        }
    } else {
        if (self.activation_stack.depth == 0)
            return .LastActivation;
    }

    // Restore each register until the current activation's saved register stack height
    // FIXME: Factor this out
    const activation_saved_register_height = activation_stack_snapshot.saved_register_height;
    const saved_register_stack = self.saved_register_stack.allItems();
    const saved_register_count = saved_register_stack.len;
    for (saved_register_stack[activation_saved_register_height..]) |_, i| {
        const saved_register = saved_register_stack[saved_register_count - 1 - i];
        vm.writeRegister(saved_register.register, saved_register.value);
    }

    // Restore the stack snapshot of the activation we just exited
    vm.restoreStackSnapshot(activation_stack_snapshot);

    vm.writeRegister(target_location, vm.readRegister(.ret));
    return .NotLastActivation;
}

pub fn takeStackSnapshot(self: Self) StackSnapshot {
    return .{
        .argument_height = self.argument_stack.height(),
        .slot_height = self.slot_stack.height(),
        .saved_register_height = self.saved_register_stack.height(),
    };
}

pub fn restoreStackSnapshot(self: *Self, snapshot: StackSnapshot) void {
    self.argument_stack.restoreTo(snapshot.argument_height);
    self.slot_stack.restoreTo(snapshot.slot_height);
    self.saved_register_stack.restoreTo(snapshot.saved_register_height);
}

pub fn readRegister(self: Self, location: RegisterLocation) Value {
    return self.register_file.read(location);
}

pub fn writeRegister(self: *Self, location: RegisterLocation, value: Value) void {
    self.register_file.write(location, value);
}

pub fn visitValues(
    self: *Self,
    context: anytype,
    visitor: fn (ctx: @TypeOf(context), value: *Value) Allocator.Error!void,
) !void {
    try visitor(context, &self.actor_object);

    // Go through the register file.
    try self.register_file.visitValues(context, visitor);
    if (self.entrypoint_selector) |*value|
        try visitor(context, value);

    // Go through the activation stack.
    for (self.activation_stack.getStack()) |*activation| {
        try visitor(context, &activation.activation_object);
        try visitor(context, &activation.creator_message);
    }

    // Go through the slot, argument and saved register stacks.
    for (self.slot_stack.allItems()) |*slot| {
        if (std.meta.eql(SlotSentinel, slot.*))
            continue;

        try visitor(context, &slot.name);
        try visitor(context, &slot.value);
    }

    for (self.argument_stack.allItems()) |*argument| {
        if (std.meta.eql(ValueSentinel, argument.*))
            continue;

        try visitor(context, argument);
    }

    for (self.saved_register_stack.allItems()) |*saved_register| {
        try visitor(context, &saved_register.value);
    }

    {
        var it = self.mailbox.first;
        while (it) |node| : (it = node.next) {
            try visitor(context, &node.data.sender);
            try visitor(context, &node.data.method);
            for (node.data.arguments) |*argument| {
                try visitor(context, argument);
            }
        }
    }
}

/// Unwinds all the stacks to their starting point. Used after an error is
/// received at the top level.
pub fn unwindStacks(self: *Self) void {
    for (self.activation_stack.getStack()) |*activation| {
        activation.deinit();
    }
    self.activation_stack.depth = 0;

    self.argument_stack.restoreTo(0);
    self.slot_stack.restoreTo(0);
    self.saved_register_stack.restoreTo(0);
}

pub fn putMessageInMailbox(
    self: *Self,
    allocator: Allocator,
    sender: *Object.Actor,
    method: *Object.Method,
    arguments: []Value,
    source_range: SourceRange,
) !void {
    const node = try allocator.create(Mailbox.Node);
    node.* = .{
        .data = .{
            .sender = sender.asValue(),
            .method = method.asValue(),
            .arguments = arguments,
            .source_range = source_range,
        },
    };

    self.mailbox.append(node);
}

/// Clear all items in the mailbox.
pub fn clearMailbox(self: *Self, allocator: Allocator) void {
    var it = self.mailbox.first;
    var next = it;
    while (it) |node| : (it = next) {
        next = node.next;

        node.data.deinit(allocator);
        allocator.destroy(node);
    }

    self.mailbox = .{};
}
