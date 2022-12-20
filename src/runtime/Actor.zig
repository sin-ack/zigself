// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./Heap.zig");
const Slot = @import("./slot.zig").Slot;
const debug = @import("../debug.zig");
const Value = value_import.Value;
const Stack = @import("./stack.zig").Stack;
const Range = @import("../language/Range.zig");
const bytecode = @import("./bytecode.zig");
const Completion = @import("./Completion.zig");
const Activation = @import("./Activation.zig");
const ActorObject = @import("objects/actor.zig").Actor;
const interpreter = @import("./interpreter.zig");
const SourceRange = @import("./SourceRange.zig");
const MethodObject = @import("objects/method.zig").Method;
const value_import = @import("./value.zig");
const ManagedObject = @import("objects/managed.zig").Managed;
const VirtualMachine = @import("./VirtualMachine.zig");
const ByteArrayObject = @import("objects/byte_array.zig").ByteArray;
const ActivationStack = Activation.ActivationStack;

const ACTIVATION_EXIT_DEBUG = debug.ACTIVATION_EXIT_DEBUG;

/// The actor object that this actor is represented by in Self code.
actor_object: ActorObject.Value,

/// The selector that will be sent to the actor context after the actor spawn
/// message has been sent to the actor. This selector must be set via
/// _ActorSetEntrypoint: in the actor spawn method.
entrypoint_selector: ?ByteArrayObject.Value = null,
/// The reason this actor has yielded.
yield_reason: YieldReason = .None,
/// The file descriptor managed object that this actor was blocked on, if any.
blocked_fd: ?ManagedObject.Value = null,
/// The activation stack stores the list of activations that are currently on
/// this actor. When an activation is exited, execution flow returns to the
/// previous activation on the stack. If a non-local return happens, however,
/// the control flow instead returns to the non-local return target.
activation_stack: ActivationStack = .{},

/// The actor object that sent the current message. Can be queried inside
/// messages with _ActorSender. It is an error to query it outside of messages.
message_sender: ?ActorObject.Value = null,

/// The mailbox stores the messages that were sent to this actor through actor
/// proxy objects.
mailbox: Mailbox = .{},

/// The register file stores the register values for this actor. Lowcode
/// execution uses these registers to perform its operations.
register_file: bytecode.lowcode.RegisterFile = .{},
argument_stack: Stack(Value, "Argument stack", ValueSentinel) = .{},
slot_stack: Stack(Slot, "Slot stack", SlotSentinel) = .{},
saved_register_stack: Stack(SavedRegister, "Saved register stack", null) = .{},

/// Whether the next created method is going to be an inline method.
next_method_is_inline: bool = false,

/// The currently active source range. This is updated by the source_range
/// instruction.
range: Range = .{ .start = 0, .end = 0 },

/// The ID of this actor, which determines the ownership of each object in the
/// system.
id: u31,

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
    register: bytecode.RegisterLocation,
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
    sender: ActorObject.Value,
    /// The method that will be executed on the actor context. Belongs to this
    /// actor.
    method: MethodObject.Value,
    /// The arguments of this message as an owned slice.
    arguments: []Value,
    /// The SourceRange which spawned this message.
    source_range: SourceRange,

    pub fn deinit(self: *Message, allocator: Allocator) void {
        allocator.free(self.arguments);
    }
};

pub fn create(vm: *VirtualMachine, token: *Heap.AllocationToken, actor_context: Value) !*Self {
    const self = try vm.allocator.create(Self);
    errdefer vm.allocator.destroy(self);

    // NOTE: If we're not in actor mode, then we belong to the global actor (which is this actor for the
    //       first call to create); otherwise, we are always owned by the genesis actor.
    const owning_actor_id = if (vm.isInActorMode()) vm.genesis_actor.?.id else 0;

    const actor_object = try ActorObject.create(vm.getMapMap(), token, owning_actor_id, self, actor_context);

    self.init(actor_object);
    return self;
}

pub fn destroy(self: *Self, allocator: Allocator) void {
    self.deinit(allocator);
    allocator.destroy(self);
}

fn init(self: *Self, actor_object: ActorObject.Ptr) void {
    self.* = .{
        .id = newActorID(),
        .actor_object = ActorObject.Value.init(actor_object),
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

pub fn activateMethod(
    self: *Self,
    vm: *VirtualMachine,
    token: *Heap.AllocationToken,
    method: MethodObject.Ptr,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    return try self.activateMethodWithContext(vm, token, self.actor_object.get().context, method, target_location, source_range);
}

pub fn activateMethodWithContext(
    self: *Self,
    vm: *VirtualMachine,
    token: *Heap.AllocationToken,
    context: Value,
    method: MethodObject.Ptr,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    const activation_slot = try self.activation_stack.getNewActivationSlot(vm.allocator);
    method.activateMethod(vm, token, self.id, context, &.{}, target_location, source_range, activation_slot);
}

pub fn execute(self: *Self, vm: *VirtualMachine) !ActorResult {
    const current_activation_ref = self.activation_stack.getCurrent().takeRef(self.activation_stack);

    // Go through the mailbox and activate all the messages that have been sent
    // so far.
    {
        var it = self.mailbox.first;
        while (it) |node| : (it = node.next) {
            var method = node.data.method.get();

            var token = try vm.heap.getAllocation(method.requiredSizeForActivation());
            defer token.deinit();
            method = node.data.method.get();

            const actor_context = self.actor_object.get().context;
            const new_activation = try self.activation_stack.getNewActivationSlot(vm.allocator);
            method.activateMethod(vm, &token, self.id, actor_context, node.data.arguments, .zero, node.data.source_range, new_activation);

            self.message_sender = node.data.sender;

            switch (try self.executeUntil(vm, current_activation_ref)) {
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

            std.debug.assert(self.activation_stack.getCurrent() == current_activation_ref.get(self.activation_stack).?);
        }
    }

    self.message_sender = null;
    self.clearMailbox(vm.allocator);

    self.blocked_fd = null;

    // Execute the activation stack of this actor normally.
    return try self.executeUntil(vm, null);
}

/// Execute the activation stack of this actor until the given activation (or if
/// `until` is null, until all activations have been resolved).
pub fn executeUntil(self: *Self, vm: *VirtualMachine, until: ?Activation.ActivationRef) !ActorResult {
    var activation = self.activation_stack.getCurrent();
    var activation_object = activation.activation_object.get();
    var executable = activation_object.getDefinitionExecutable();
    var block = activation_object.getBytecodeBlock();

    while (true) {
        var context = interpreter.InterpreterContext{
            .vm = vm,
            .actor = self,
            .last_activation_ref = until,
            .activation = activation,
            .executable = executable,
            .block = block,
        };

        const execution_result = try interpreter.execute(&context);

        switch (execution_result) {
            .ActorSwitch => {
                return ActorResult{ .ActorSwitch = {} };
            },
            .ActivationChange => {
                activation = self.activation_stack.getCurrent();
                activation_object = activation.activation_object.get();
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
        }
    }

    unreachable;
}

pub const ActivationExitState = enum { LastActivation, NotLastActivation };

pub fn exitCurrentActivation(self: *Self, vm: *VirtualMachine, last_activation_ref: ?Activation.ActivationRef) ActivationExitState {
    if (ACTIVATION_EXIT_DEBUG) std.debug.print("Actor.exitCurrentActivation: Exiting this activation\n", .{});
    const current_activation = self.activation_stack.getCurrent();
    return self.exitActivation(vm, last_activation_ref, current_activation);
}

/// Exit the given activation and write the result of the return register to the
/// register for the instruction that initiated the activation. Returns
/// ActivationExitState.LastActivation if the last activation has been exited.
pub fn exitActivation(
    self: *Self,
    vm: *VirtualMachine,
    last_activation_ref: ?Activation.ActivationRef,
    target_activation: *Activation,
) ActivationExitState {
    const last_activation = if (last_activation_ref) |ref| ref.get(self.activation_stack).? else null;

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

    if (last_activation == null and self.activation_stack.getDepth() == 0)
        return .LastActivation;

    // Restore each register until the current activation's saved register stack height
    // FIXME: Factor this out
    const activation_saved_register_height = activation_stack_snapshot.saved_register_height;
    const saved_register_stack = self.saved_register_stack.allItems();
    const saved_register_count = saved_register_stack.len;
    for (saved_register_stack[activation_saved_register_height..]) |_, i| {
        const saved_register = saved_register_stack[saved_register_count - 1 - i];
        vm.writeRegister(saved_register.register, saved_register.value);
    }

    if (last_activation) |last| {
        if (self.activation_stack.getCurrent() == last) {
            return .LastActivation;
        }
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

pub fn readRegister(self: Self, location: bytecode.RegisterLocation) Value {
    return self.register_file.read(location);
}

pub fn writeRegister(self: *Self, location: bytecode.RegisterLocation, value: Value) void {
    self.register_file.write(location, value);
}

pub fn visitValues(
    self: *Self,
    context: anytype,
    visitor: fn (ctx: @TypeOf(context), value: *Value) Allocator.Error!void,
) !void {
    try visitor(context, &self.actor_object.value);

    // Go through the register file.
    try self.register_file.visitValues(context, visitor);

    if (self.entrypoint_selector) |*value|
        try visitor(context, &value.value);

    if (self.blocked_fd) |*value|
        try visitor(context, &value.value);

    if (self.message_sender) |*value|
        try visitor(context, &value.value);

    // Go through the activation stack.
    for (self.activation_stack.getStack()) |*activation| {
        try visitor(context, &activation.activation_object.value);
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
            try visitor(context, &node.data.sender.value);
            try visitor(context, &node.data.method.value);
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
    self.activation_stack.clear();

    self.argument_stack.restoreTo(0);
    self.slot_stack.restoreTo(0);
    self.saved_register_stack.restoreTo(0);
}

pub fn putMessageInMailbox(
    self: *Self,
    allocator: Allocator,
    sender: ActorObject.Ptr,
    method: MethodObject.Ptr,
    arguments: []Value,
    source_range: SourceRange,
) !void {
    const node = try allocator.create(Mailbox.Node);
    node.* = .{
        .data = .{
            .sender = ActorObject.Value.init(sender),
            .method = MethodObject.Value.init(method),
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

pub fn canWriteTo(self: *Self, value: Value) bool {
    return switch (value.getType()) {
        .ObjectMarker => unreachable,
        .Integer, .FloatingPoint => true,
        .ObjectReference => writable: {
            const object = value.asObject();
            // FIXME: Don't hardcode global actor ID
            break :writable self.id == 0 or object.object_information.reachability != .Global;
        },
    };
}

/// Ensure that the current actor can read the given value.
/// If the actor cannot read this value, then the VM should crash, as we shouldn't
/// be able to reach this object in the first place.
pub fn ensureCanRead(self: *Self, value: Value, source_range: SourceRange) void {
    switch (value.getType()) {
        .ObjectMarker => unreachable,
        .Integer, .FloatingPoint => {},
        .ObjectReference => {
            const object = value.asObject();
            if (object.object_information.reachability != .Global and object.object_information.actor_id != self.id)
                std.debug.panic(
                    "!!! Attempted to read object that is not readable for this actor!\n" ++
                        "  Object {*} owned by actor #{}\n" ++
                        "  Actor #{} is attempting to reach it at {}",
                    .{ object, object.object_information.actor_id, self.id, source_range },
                );
        },
    }
}

// FIXME: This isn't thread safe!
var next_actor_id: u31 = 0;

pub fn newActorID() u31 {
    const this_id = next_actor_id;
    next_actor_id += 1;
    return this_id;
}
