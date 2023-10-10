// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./Heap.zig");
const Slot = slot_import.Slot;
const debug = @import("../debug.zig");
const Actor = @import("./Actor.zig");
const bless = @import("./object_bless.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig").Object;
const bytecode = @import("./bytecode.zig");
const BlockMap = block_object.BlockMap;
const SlotsMap = slots_object.SlotsMap;
const ByteArray = @import("./ByteArray.zig");
const MethodMap = method_object.MethodMap;
const traversal = @import("./object_traversal.zig");
const Activation = @import("./Activation.zig");
const primitives = @import("./primitives.zig");
const SourceRange = @import("./SourceRange.zig");
const BlockObject = block_object.Block;
const SlotsObject = slots_object.Slots;
const slot_import = @import("./slot.zig");
const block_object = @import("objects/block.zig");
const slots_object = @import("objects/slots.zig");
const MethodObject = method_object.Method;
const RuntimeError = @import("RuntimeError.zig");
const method_object = @import("objects/method.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const ExecutionResult = @import("execution_result.zig").ExecutionResult;
const ByteArrayObject = @import("objects/byte_array.zig").ByteArray;
const ActivationObject = @import("objects/activation.zig").Activation;

const EXECUTION_DEBUG = debug.EXECUTION_DEBUG;

pub const InterpreterContext = struct {
    /// The VM that the interpreter is currently running under. Any values
    /// here are global across all actors.
    vm: *VirtualMachine,
    /// The actor that is currently active on this thread.
    actor: *Actor,
    /// The final activation for the execution of the current interpreter
    /// runtime. If this activation is exited with Return or NonlocalReturn,
    /// the interpreter finishes running.
    last_activation_ref: ?Activation.ActivationRef,

    /// The context specific to the current activation. These are expensive to
    /// obtain per-instruction, so are cached.
    activation_context: ActivationContext = undefined,

    /// The result of the currently executed instruction. This is a workaround
    /// for Zig not being able to do tailcalls with return values yet. This is
    /// set when the interpreter needs to exit.
    result: ExecutionResult = undefined,

    const ActivationContext = struct {
        /// The executable the current activation's method/block was defined in.
        executable: bytecode.Executable.Ref,
        /// The bytecode block for the current activation. Each activation
        /// corresponds to exactly one block.
        block: *const bytecode.Block,
    };

    pub fn init(vm: *VirtualMachine, actor: *Actor, last_activation_ref: ?Activation.ActivationRef) InterpreterContext {
        var context = InterpreterContext{
            .vm = vm,
            .actor = actor,
            .last_activation_ref = last_activation_ref,
        };
        context.refreshActivationContext();

        return context;
    }

    pub fn getSourceRange(self: *InterpreterContext) SourceRange {
        return SourceRange.initNoRef(self.getDefinitionExecutable(), self.actor.range);
    }

    pub fn getCurrentActivation(self: InterpreterContext) *Activation {
        return self.actor.activation_stack.getCurrent();
    }

    pub fn getInstructionIndex(self: InterpreterContext) u32 {
        return self.getCurrentActivation().instruction_index;
    }

    pub fn getCurrentActivationObject(self: InterpreterContext) ActivationObject.Ptr {
        return self.getCurrentActivation().activation_object.get();
    }

    pub fn getDefinitionExecutable(self: *InterpreterContext) bytecode.Executable.Ref {
        return self.activation_context.executable;
    }

    pub fn getCurrentBytecodeBlock(self: *InterpreterContext) *const bytecode.Block {
        return self.activation_context.block;
    }

    pub fn refreshActivationContext(self: *InterpreterContext) void {
        const activation_object = self.getCurrentActivationObject();
        self.activation_context = .{
            .executable = activation_object.getDefinitionExecutable(),
            .block = activation_object.getBytecodeBlock(),
        };
    }
};

pub fn execute(context: *InterpreterContext) InterpreterError!Actor.ActorResult {
    try specialized_executors[@intFromEnum(context.getCurrentBytecodeBlock().getOpcode(context.getInstructionIndex()))](context);
    return switch (context.result) {
        .Resolved => |value| Actor.ActorResult{ .Finished = value },
        .ActorYielded => Actor.ActorResult{ .Switched = {} },
        .RuntimeError => |err| Actor.ActorResult{ .RuntimeError = err },

        .Normal, .ActivationChanged, .Restarted => unreachable,
    };
}

pub const InterpreterError = Allocator.Error;
const OpcodeHandler = fn (context: *InterpreterContext) InterpreterError!ExecutionResult;
const Executor = fn (context: *InterpreterContext) InterpreterError!void;
fn opcodeHandler(comptime opcode: bytecode.Instruction.Opcode) OpcodeHandler {
    return switch (opcode) {
        .Send => opcodeSend,
        .SelfSend => opcodeSelfSend,
        .PrimSend => opcodePrimSend,
        .SelfPrimSend => opcodeSelfPrimSend,
        .PushConstantSlot => opcodePushConstantSlot,
        .PushAssignableSlot => opcodePushAssignableSlot,
        .PushArgumentSlot => opcodePushArgumentSlot,
        .CreateInteger => opcodeCreateInteger,
        .CreateFloatingPoint => opcodeCreateFloatingPoint,
        .CreateByteArray => opcodeCreateByteArray,
        .CreateObject => opcodeCreateObject,
        .CreateMethod => opcodeCreateMethod,
        .CreateBlock => opcodeCreateBlock,
        .Return => opcodeReturn,
        .NonlocalReturn => opcodeNonlocalReturn,
        .PushArg => opcodePushArg,
        .PushRegisters => opcodePushRegisters,
        .SetMethodInline => opcodeSetMethodInline,
        .PushArgumentSentinel => opcodePushArgumentSentinel,
        .PushSlotSentinel => opcodePushSlotSentinel,
        .VerifyArgumentSentinel => opcodeVerifyArgumentSentinel,
        .VerifySlotSentinel => opcodeVerifySlotSentinel,
    };
}

const specialized_executors = blk: {
    var executors: []const *const Executor = &[_]*const Executor{};

    for (std.enums.values(bytecode.Instruction.Opcode)) |opcode| {
        executors = executors ++ [_]*const Executor{makeSpecializedExecutor(opcode)};
    }

    break :blk executors;
};

pub fn makeSpecializedExecutor(comptime opcode: bytecode.Instruction.Opcode) Executor {
    return struct {
        fn execute(context: *InterpreterContext) InterpreterError!void {
            if (EXECUTION_DEBUG) {
                const block = context.getCurrentBytecodeBlock();
                const index = context.getInstructionIndex();

                const inst = bytecode.Instruction{
                    .target = block.getTargetLocation(index),
                    .opcode = block.getOpcode(index),
                    .payload = block.getPayload(index),
                    .source_range = block.getSourceRange(index),
                };
                std.debug.print("[#{} {s}] Executing: {} = {}\n", .{ context.actor.id, context.getDefinitionExecutable().value.definition_script.value.file_path, inst.target, inst });
            }

            context.actor.range = context.getCurrentBytecodeBlock().getSourceRange(context.getInstructionIndex());
            const result = try @call(.always_inline, opcodeHandler(opcode), .{context});
            const new_instruction_index = new_instruction_index: {
                switch (result) {
                    .Normal => {
                        break :new_instruction_index context.getCurrentActivation().advanceInstruction();
                    },
                    .ActivationChanged => {
                        context.refreshActivationContext();
                        break :new_instruction_index context.getInstructionIndex();
                    },
                    .Restarted => {
                        break :new_instruction_index context.getCurrentActivation().restart();
                    },
                    else => {
                        context.result = result;
                        return;
                    },
                }
            };

            return @call(.always_tail, specialized_executors[@intFromEnum(context.getCurrentBytecodeBlock().getOpcode(new_instruction_index))], .{context});
        }
    }.execute;
}

// --- Opcode handlers ---

fn opcodeSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .Send);
    const receiver = context.vm.readRegister(payload.receiver_location);
    return try performSend(context.vm, context.actor, receiver, payload.message_name, block.getTargetLocation(index), context.getSourceRange());
}

fn opcodeSelfSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .SelfSend);
    const receiver = context.actor.activation_stack.getCurrent().activation_object.value;
    return try performSend(context.vm, context.actor, receiver, payload.message_name, block.getTargetLocation(index), context.getSourceRange());
}

fn opcodePrimSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .PrimSend);
    const receiver = context.vm.readRegister(payload.receiver_location);
    return try performPrimitiveSend(context.vm, context.actor, receiver, payload.message_name, block.getTargetLocation(index), context.getSourceRange());
}

fn opcodeSelfPrimSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .SelfPrimSend);
    const receiver = context.actor.activation_stack.getCurrent().activation_object.get().findActivationReceiver();
    return try performPrimitiveSend(context.vm, context.actor, receiver, payload.message_name, block.getTargetLocation(index), context.getSourceRange());
}

fn opcodePushConstantSlot(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const payload = context.getCurrentBytecodeBlock().getTypedPayload(context.getInstructionIndex(), .PushConstantSlot);
    const name_value = context.vm.readRegister(payload.name_location);
    const name_byte_array_object = name_value.asObject().mustBeType(.ByteArray);
    const value = context.vm.readRegister(payload.value_location);

    try context.actor.slot_stack.push(context.vm.allocator, Slot.initConstant(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
    return ExecutionResult.normal();
}

fn opcodePushAssignableSlot(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const payload = context.getCurrentBytecodeBlock().getTypedPayload(context.getInstructionIndex(), .PushAssignableSlot);
    const name_value = context.vm.readRegister(payload.name_location);
    const name_byte_array_object = name_value.asObject().mustBeType(.ByteArray);
    const value = context.vm.readRegister(payload.value_location);

    try context.actor.slot_stack.push(context.vm.allocator, Slot.initAssignable(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
    return ExecutionResult.normal();
}

fn opcodePushArgumentSlot(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const payload = context.getCurrentBytecodeBlock().getTypedPayload(context.getInstructionIndex(), .PushArgumentSlot);
    const name_value = context.vm.readRegister(payload.name_location);
    const name_byte_array_object = name_value.asObject().mustBeType(.ByteArray);

    try context.actor.slot_stack.push(context.vm.allocator, Slot.initArgument(name_byte_array_object.getByteArray()));
    return ExecutionResult.normal();
}

fn opcodeCreateInteger(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();
    context.vm.writeRegister(block.getTargetLocation(index), Value.fromInteger(block.getTypedPayload(index, .CreateInteger)));
    return ExecutionResult.normal();
}

fn opcodeCreateFloatingPoint(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();
    context.vm.writeRegister(block.getTargetLocation(index), Value.fromFloatingPoint(block.getTypedPayload(index, .CreateFloatingPoint)));
    return ExecutionResult.normal();
}

fn opcodeCreateByteArray(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .CreateByteArray);
    var token = try context.vm.heap.getAllocation(ByteArrayObject.requiredSizeForAllocation(payload.len));
    defer token.deinit();

    const byte_array = ByteArrayObject.createWithValues(context.vm.getMapMap(), &token, context.actor.id, payload);
    context.vm.writeRegister(block.getTargetLocation(index), byte_array.asValue());
    return ExecutionResult.normal();
}

fn opcodeCreateObject(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    try createObject(
        context.vm,
        context.actor,
        block.getTypedPayload(index, .CreateObject).slot_count,
        block.getTargetLocation(index),
    );
    return ExecutionResult.normal();
}

fn opcodeCreateMethod(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .CreateMethod);
    const method_name_byte_array = context.vm.readRegister(payload.method_name_location).asObject().mustBeType(.ByteArray).getByteArray();

    try createMethod(
        context.vm,
        context.actor,
        context.getDefinitionExecutable(),
        method_name_byte_array,
        payload.slot_count,
        payload.block_index,
        block.getTargetLocation(index),
    );
    return ExecutionResult.normal();
}

fn opcodeCreateBlock(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .CreateBlock);
    try createBlock(
        context.vm,
        context.actor,
        context.getDefinitionExecutable(),
        payload.slot_count,
        payload.block_index,
        block.getTargetLocation(index),
    );
    return ExecutionResult.normal();
}

fn opcodeReturn(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const value = context.vm.readRegister(context.getCurrentBytecodeBlock().getTypedPayload(context.getInstructionIndex(), .Return).value_location);
    context.vm.writeRegister(.ret, value);

    if (context.actor.exitCurrentActivation(context.vm, context.last_activation_ref) == .LastActivation) {
        return ExecutionResult.resolve(context.vm.readRegister(.ret));
    }

    return ExecutionResult.changeActivation();
}

fn opcodeNonlocalReturn(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const value = context.vm.readRegister(context.getCurrentBytecodeBlock().getTypedPayload(context.getInstructionIndex(), .NonlocalReturn).value_location);
    context.vm.writeRegister(.ret, value);

    const current_activation = context.actor.activation_stack.getCurrent();
    // FIXME: Better name
    const target_activation_ref = current_activation.nonlocal_return_target_activation.?;
    const target_activation = target_activation_ref.get(context.actor.activation_stack) orelse {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(context.getSourceRange(), "Attempted to non-local return to non-existent activation"));
    };

    if (context.actor.exitActivation(context.vm, context.last_activation_ref, target_activation) == .LastActivation) {
        return ExecutionResult.resolve(context.vm.readRegister(.ret));
    }

    return ExecutionResult.changeActivation();
}

fn opcodePushArg(context: *InterpreterContext) InterpreterError!ExecutionResult {
    var argument = context.vm.readRegister(context.getCurrentBytecodeBlock().getTypedPayload(context.getInstructionIndex(), .PushArg).argument_location);
    if (argument.isObjectReference()) {
        if (argument.asObject().asType(.Activation)) |activation| {
            argument = activation.findActivationReceiver();
        }
    }
    try context.actor.argument_stack.push(context.vm.allocator, argument);
    return ExecutionResult.normal();
}

fn opcodePushRegisters(context: *InterpreterContext) InterpreterError!ExecutionResult {
    var iterator = context.getCurrentBytecodeBlock().getTypedPayload(context.getInstructionIndex(), .PushRegisters).iterator(.{});
    while (iterator.next()) |clobbered_register| {
        // FIXME: Remove manual register number adjustment!
        const register = bytecode.RegisterLocation.fromInt(@intCast(clobbered_register + 2));
        try context.actor.saved_register_stack.push(context.vm.allocator, .{ .register = register, .value = context.vm.readRegister(register) });
    }

    return ExecutionResult.normal();
}

fn opcodeSetMethodInline(context: *InterpreterContext) InterpreterError!ExecutionResult {
    context.actor.next_method_is_inline = true;
    return ExecutionResult.normal();
}

fn opcodePushArgumentSentinel(context: *InterpreterContext) InterpreterError!ExecutionResult {
    try context.actor.argument_stack.pushSentinel(context.vm.allocator);
    return ExecutionResult.normal();
}

fn opcodePushSlotSentinel(context: *InterpreterContext) InterpreterError!ExecutionResult {
    try context.actor.slot_stack.pushSentinel(context.vm.allocator);
    return ExecutionResult.normal();
}

fn opcodeVerifyArgumentSentinel(context: *InterpreterContext) InterpreterError!ExecutionResult {
    context.actor.argument_stack.verifySentinel();
    return ExecutionResult.normal();
}

fn opcodeVerifySlotSentinel(context: *InterpreterContext) InterpreterError!ExecutionResult {
    context.actor.slot_stack.verifySentinel();
    return ExecutionResult.normal();
}

// --- Utility functions ---

fn performSend(
    vm: *VirtualMachine,
    actor: *Actor,
    receiver: Value,
    message_name: []const u8,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
    const result = try sendMessage(vm, actor, receiver, message_name, target_location, source_range);
    switch (result) {
        .Resolved => |v| {
            var value = v;
            if (value.isObjectReference()) {
                if (value.asObject().asType(.Activation)) |activation| {
                    value = activation.findActivationReceiver();
                }
            }

            vm.writeRegister(target_location, value);
            return ExecutionResult.normal();
        },
        else => return result,
    }
}

fn performPrimitiveSend(
    vm: *VirtualMachine,
    actor: *Actor,
    receiver_: Value,
    message_name: []const u8,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
    if (primitives.getPrimitive(message_name)) |primitive| {
        var receiver = receiver_;
        if (receiver.isObjectReference()) {
            if (receiver.asObject().asType(.Activation)) |activation| {
                receiver = activation.findActivationReceiver();
            }
        }

        const tracked_receiver = try vm.heap.track(receiver);
        defer tracked_receiver.untrack(vm.heap);

        const argument_slice = actor.argument_stack.lastNItems(primitive.arity);

        const primitive_result = try primitive.call(vm, actor, tracked_receiver, argument_slice, target_location, source_range);
        if (!(primitive_result == .ActorYielded and actor.yield_reason == .Blocked)) {
            // NOTE: If the actor got blocked, it will retry the same primitive
            //       call when it gets unblocked, so we shouldn't pop values off
            //       its stack.
            actor.argument_stack.popNItems(primitive.arity);
        }

        switch (primitive_result) {
            .Resolved => |value| {
                vm.writeRegister(target_location, value);
                return ExecutionResult.normal();
            },
            else => return primitive_result,
        }
    }

    return ExecutionResult.runtimeError(
        try RuntimeError.initFormatted(vm, source_range, "Unknown primitive selector '{s}'", .{message_name}),
    );
}

/// Sends a message to the given receiver, returning the result if it can be
/// immediately resolved. If the message send must create a new activation,
/// pushes the activation onto the stack and signals that the activation has
/// changed. If the message send fails, returns the runtime error.
pub fn sendMessage(
    vm: *VirtualMachine,
    actor: *Actor,
    receiver: Value,
    message_name: []const u8,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    // FIXME: Only activate this when the message looks like a block execution.
    {
        var block_receiver = receiver;
        if (block_receiver.isObjectReference()) {
            if (block_receiver.asObject().asType(.Activation)) |activation| {
                block_receiver = activation.findActivationReceiver();
            }
        }

        if (block_receiver.isObjectReference()) {
            if (block_receiver.asObject().asType(.Block)) |receiver_as_block| {
                if (receiver_as_block.isCorrectMessageForBlockExecution(message_name)) {
                    const argument_count = receiver_as_block.getArgumentSlotCount();
                    const argument_slice = actor.argument_stack.lastNItems(argument_count);

                    // Advance the instruction for the activation that will be returned to.
                    _ = actor.activation_stack.getCurrent().advanceInstruction();

                    try executeBlock(vm, actor, receiver_as_block, argument_slice, target_location, source_range);

                    actor.argument_stack.popNItems(argument_count);
                    // Bump the argument stack height of the (now current)
                    // activation since we've now popped this activation's items off
                    // it.
                    actor.activation_stack.getCurrent().stack_snapshot.bumpArgumentHeight(actor);
                    return ExecutionResult.changeActivation();
                }
            }
        }
    }

    actor.ensureCanRead(receiver, source_range);

    return switch (receiver.lookup(vm, message_name)) {
        .Regular => |lookup_result| {
            if (lookup_result.isObjectReference()) {
                if (lookup_result.asObject().asType(.Method)) |method| {
                    const argument_count = method.getArgumentSlotCount();
                    const argument_slice = actor.argument_stack.lastNItems(argument_count);

                    // Advance the instruction for the activation that will be returned to.
                    _ = actor.activation_stack.getCurrent().advanceInstruction();

                    try executeMethod(vm, actor, receiver, method, argument_slice, target_location, source_range);

                    actor.argument_stack.popNItems(argument_count);
                    // Bump the argument stack height of the (now current)
                    // activation since we've now popped this activation's items
                    // off it.
                    actor.activation_stack.getCurrent().stack_snapshot.bumpArgumentHeight(actor);
                    return ExecutionResult.changeActivation();
                }
            }

            return ExecutionResult.resolve(lookup_result);
        },
        .Assignment => |assignment_context| {
            const argument_slice = actor.argument_stack.lastNItems(1);
            var argument = argument_slice[0];
            // NOTE: This is required, for instance, when we are assigning `self` to
            //       a slot (happens more often than you might think!). We need to strip
            //       the activation object to get to the actual value inside.
            if (argument.isObjectReference()) {
                if (argument.asObject().asType(.Activation)) |activation| {
                    argument = activation.findActivationReceiver();
                }
            }

            const object_that_has_the_assignable_slot = assignment_context.object;
            const value_ptr = assignment_context.value_ptr;

            if (!actor.canWriteTo(object_that_has_the_assignable_slot.asValue())) {
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(source_range, "Assignment target is not writable for actor"));
            }

            if (object_that_has_the_assignable_slot.object_information.reachability == .Global) {
                // Mark every object that's not globally reachable in the
                // argument's object graph as globally reachable. This will
                // make the whole object graph part of the global object
                // hierarchy.
                _ = traversal.traverseNonGloballyReachableObjectGraph(argument, {}, struct {
                    fn f(context: void, object: Object.Ptr) error{}!Object.Ptr {
                        _ = context;
                        object.object_information.reachability = .Global;
                        return object;
                    }
                }.f) catch unreachable;
            }

            value_ptr.* = argument;

            // David will remember that.
            try vm.heap.rememberObjectReference(object_that_has_the_assignable_slot.asValue(), argument);

            actor.argument_stack.popNItems(1);
            return ExecutionResult.resolve(receiver);
        },
        .ActorMessage => |actor_message| {
            const method = actor_message.method;
            const argument_count = method.getArgumentSlotCount();
            const argument_slice = actor.argument_stack.lastNItems(argument_count);

            var target_actor = actor_message.target_actor.getActor();

            // FIXME: Figure out a way to avoid creating an owned slice here.
            //        This is required for the time being because we don't have
            //        a better first-in-last-out (which is how messages are
            //        processed) structure yet.
            const new_arguments_slice = try vm.allocator.alloc(Value, argument_slice.len);
            errdefer vm.allocator.free(new_arguments_slice);

            // Blessing each argument is required so that actors don't share memory.
            for (argument_slice, 0..) |argument, i| {
                new_arguments_slice[i] = try bless.bless(vm, vm.heap, target_actor.id, argument);
            }

            try target_actor.putMessageInMailbox(
                vm.allocator,
                actor.actor_object.get(),
                method,
                new_arguments_slice,
                source_range,
            );

            actor.argument_stack.popNItems(argument_count);
            return ExecutionResult.resolve(vm.nil());
        },
        .Nothing => ExecutionResult.runtimeError(try RuntimeError.initFormatted(vm, source_range, "Unknown selector '{s}'", .{message_name})),
    };
}

fn executeBlock(
    vm: *VirtualMachine,
    actor: *Actor,
    block_receiver: BlockObject.Ptr,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    const message_name = try vm.getOrCreateBlockMessageName(@intCast(arguments.len));
    var block = block_receiver;

    var required_memory = ActivationObject.requiredSizeForAllocation(
        block.getArgumentSlotCount(),
        block.getAssignableSlotCount(),
    );
    if (!message_name.exists) required_memory += message_name.requiredSize();

    var token = token: {
        var tracked_block: ?Heap.Tracked = null;
        defer if (tracked_block) |t| t.untrack(vm.heap);

        if (vm.heap.needsToGarbageCollectToProvide(required_memory)) {
            tracked_block = try vm.heap.track(block.asValue());
        }

        // Ensure that we won't GC by creating an activation.
        var token = try vm.heap.getAllocation(required_memory);

        if (tracked_block) |t| {
            // Refresh the pointer to the block.
            block = t.getValue().asObject().mustBeType(.Block);
        }

        break :token token;
    };
    defer token.deinit();

    const parent_activation_object = block.getMap().parent_activation.get(actor.activation_stack).?.activation_object;
    const activation_slot = try actor.activation_stack.getNewActivationSlot(vm.allocator);
    block.activateBlock(
        vm,
        &token,
        parent_activation_object.value,
        arguments,
        target_location,
        try message_name.get(&token),
        source_range,
        activation_slot,
    );
}

fn executeMethod(
    vm: *VirtualMachine,
    actor: *Actor,
    const_receiver: Value,
    const_method: MethodObject.Ptr,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    var receiver_of_method = const_receiver;
    var method = const_method;

    const required_memory = ActivationObject.requiredSizeForAllocation(
        method.getArgumentSlotCount(),
        method.getAssignableSlotCount(),
    );

    var token = token: {
        var tracked_receiver: ?Heap.Tracked = null;
        var tracked_method: ?Heap.Tracked = null;

        defer if (tracked_receiver) |t| {
            tracked_method.?.untrack(vm.heap);
            t.untrack(vm.heap);
        };

        if (vm.heap.needsToGarbageCollectToProvide(required_memory)) {
            tracked_receiver = try vm.heap.track(receiver_of_method);
            tracked_method = try vm.heap.track(method.asValue());
        }

        // Get the allocation token for the method
        var token = try vm.heap.getAllocation(required_memory);

        if (tracked_receiver) |t| {
            // Refresh the pointers to the method and its receiver.
            receiver_of_method = t.getValue();
            method = tracked_method.?.getValue().asObject().mustBeType(.Method);
        }

        break :token token;
    };
    defer token.deinit();

    // NOTE: The receiver of a method activation must never be an activation
    //       object (unless it explicitly wants that), as that would allow
    //       us to access the slots of upper scopes.
    if (!method.expectsActivationObjectAsReceiver() and receiver_of_method.isObjectReference()) {
        if (receiver_of_method.asObject().asType(.Activation)) |activation| {
            receiver_of_method = activation.findActivationReceiver();
        }
    }

    const activation_slot = try actor.activation_stack.getNewActivationSlot(vm.allocator);
    method.activateMethod(vm, &token, actor.id, receiver_of_method, arguments, target_location, source_range, activation_slot);
}

fn createObject(
    vm: *VirtualMachine,
    actor: *Actor,
    slot_count: u32,
    target_location: bytecode.RegisterLocation,
) !void {
    const slots = actor.slot_stack.lastNItems(slot_count);

    var total_slot_count: u32 = 0;
    var total_assignable_slot_count: u8 = 0;
    for (slots, 0..) |slot, i| {
        total_slot_count += slot.requiredSlotSpace(slots[0..i]);
        total_assignable_slot_count += @intCast(slot.requiredAssignableSlotValueSpace(slots[0..i]));
    }

    var token = try vm.heap.getAllocation(
        SlotsMap.requiredSizeForAllocation(total_slot_count) +
            SlotsObject.requiredSizeForAllocation(total_assignable_slot_count),
    );
    defer token.deinit();

    var slots_map = SlotsMap.create(vm.getMapMap(), &token, total_slot_count);
    var map_builder = slots_map.getMapBuilder(&token);

    for (slots) |slot| {
        map_builder.addSlot(slot);
    }

    const the_slots_object = map_builder.createObject(actor.id);
    vm.writeRegister(target_location, the_slots_object.asValue());
    actor.slot_stack.popNItems(slot_count);
}

fn createMethod(
    vm: *VirtualMachine,
    actor: *Actor,
    executable: bytecode.Executable.Ref,
    method_name: ByteArray,
    slot_count: u32,
    block_index: u32,
    target_location: bytecode.RegisterLocation,
) !void {
    defer actor.next_method_is_inline = false;

    const slots = actor.slot_stack.lastNItems(slot_count);
    var total_slot_count: u32 = 0;
    var total_assignable_slot_count: u8 = 0;
    var argument_slot_count: u8 = 0;
    for (slots, 0..) |slot, i| {
        total_slot_count += slot.requiredSlotSpace(slots[0..i]);
        total_assignable_slot_count += @intCast(slot.requiredAssignableSlotValueSpace(slots[0..i]));

        // FIXME: This makes the assumption that argument slots are
        //        never overwritten.
        if (slot.isArgument())
            argument_slot_count += 1;
    }

    var token = try vm.heap.getAllocation(
        MethodMap.requiredSizeForAllocation(total_slot_count) +
            MethodObject.requiredSizeForAllocation(total_assignable_slot_count),
    );
    defer token.deinit();

    const block = executable.value.getBlock(block_index);
    var method_map = try MethodMap.create(
        vm.getMapMap(),
        &token,
        argument_slot_count,
        total_slot_count,
        actor.next_method_is_inline,
        method_name,
        block,
        executable,
    );

    var map_builder = method_map.getMapBuilder(&token);

    for (slots) |slot| {
        map_builder.addSlot(slot);
    }

    const method = map_builder.createObject(actor.id);
    vm.writeRegister(target_location, method.asValue());
    actor.slot_stack.popNItems(slot_count);
}

fn createBlock(
    vm: *VirtualMachine,
    actor: *Actor,
    executable: bytecode.Executable.Ref,
    slot_count: u32,
    block_index: u32,
    target_location: bytecode.RegisterLocation,
) !void {
    const slots = actor.slot_stack.lastNItems(slot_count);
    var total_slot_count: u32 = 0;
    var total_assignable_slot_count: u8 = 0;
    var argument_slot_count: u8 = 0;
    for (slots, 0..) |slot, i| {
        total_slot_count += slot.requiredSlotSpace(slots[0..i]);
        total_assignable_slot_count += @intCast(slot.requiredAssignableSlotValueSpace(slots[0..i]));

        // FIXME: This makes the assumption that argument slots are
        //        never overwritten.
        if (slot.isArgument())
            argument_slot_count += 1;
    }

    const block = executable.value.getBlock(block_index);
    // The latest activation is where the block was created, so it will always
    // be the parent activation (i.e., where we look for parent blocks' and the
    // method's slots).
    const parent_activation = actor.activation_stack.getCurrent();
    // However, we want the _method_ as the non-local return target; because the
    // non-local return can only be returned by the method in which the block
    // making the non-local return was defined, this needs to be separate from
    // parent_activation. If the parent activation is a block, it will also
    // contain a target activation; if it's a method the target activation _is_
    // the parent.
    const nonlocal_return_target_activation = if (parent_activation.nonlocal_return_target_activation) |target|
        target
    else
        parent_activation.takeRef(actor.activation_stack);
    std.debug.assert(nonlocal_return_target_activation.get(actor.activation_stack).?.nonlocal_return_target_activation == null);

    var token = try vm.heap.getAllocation(
        BlockMap.requiredSizeForAllocation(total_slot_count) +
            BlockObject.requiredSizeForAllocation(total_assignable_slot_count),
    );
    defer token.deinit();

    var block_map = try BlockMap.create(
        vm.getMapMap(),
        &token,
        argument_slot_count,
        total_slot_count,
        parent_activation.takeRef(actor.activation_stack),
        nonlocal_return_target_activation,
        block,
        executable,
    );

    var map_builder = block_map.getMapBuilder(&token);

    for (slots) |slot| {
        map_builder.addSlot(slot);
    }

    const the_block_object = map_builder.createObject(actor.id);
    vm.writeRegister(target_location, the_block_object.asValue());
    actor.slot_stack.popNItems(slot_count);
}
