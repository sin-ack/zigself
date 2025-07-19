// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const tracy = @import("tracy");
const Allocator = std.mem.Allocator;

const Heap = VirtualMachine.Heap;
const Slot = @import("./slot.zig").Slot;
const debug = @import("../debug.zig");
const Actor = @import("./Actor.zig");
const bless = @import("./object_bless.zig");
const Value = @import("./value.zig").Value;
const Selector = @import("Selector.zig");
const bytecode = @import("./bytecode.zig");
const BlockMap = objects_block.BlockMap;
const SlotsMap = objects_slots.SlotsMap;
const MethodMap = objects_method.MethodMap;
const ByteArray = @import("objects/byte_array.zig").ByteArray;
const traversal = @import("./object_traversal.zig");
const Activation = @import("./Activation.zig");
const BaseObject = @import("./base_object.zig").BaseObject;
const primitives = @import("./primitives.zig");
const vm_context = @import("context.zig");
const SourceRange = @import("./SourceRange.zig");
const BlockObject = objects_block.Block;
const FloatObject = @import("objects/float.zig").Float;
const SlotsObject = objects_slots.Slots;
const MethodObject = objects_method.Method;
const RuntimeError = @import("RuntimeError.zig");
const ExecutableMap = @import("objects/executable_map.zig").ExecutableMap;
const objects_block = @import("objects/block.zig");
const objects_slots = @import("objects/slots.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const objects_method = @import("objects/method.zig");
const ExecutionResult = @import("execution_result.zig").ExecutionResult;
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

    pub fn init(last_activation_ref: ?Activation.ActivationRef) InterpreterContext {
        var context = InterpreterContext{
            .vm = vm_context.getVM(),
            .actor = vm_context.getActor(),
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
        .VerifyArgumentSentinel => opcodeVerifyArgumentSentinel,
    };
}

const specialized_executors = blk: {
    var executors: []const *const Executor = &[_]*const Executor{};

    for (std.enums.values(bytecode.Instruction.Opcode)) |opcode| {
        executors = executors ++ [_]*const Executor{makeSpecializedExecutor(opcode).execute};
    }

    break :blk executors;
};

// NOTE: We return the struct and not the function because this way Zig gives it
//       a nice name in the symbol table, which is useful for debugging.
//       "runtime.interpreter.makeSpecializedExecutor__struct_12345.execute"
//       -> "runtime.interpreter.makeSpecializedExecutor(.Send).execute"
pub fn makeSpecializedExecutor(comptime opcode: bytecode.Instruction.Opcode) type {
    return struct {
        fn execute(context: *InterpreterContext) InterpreterError!void {
            if (EXECUTION_DEBUG) {
                const block = context.getCurrentBytecodeBlock();
                const index = context.getInstructionIndex();
                const range = block.getSourceRange(index);

                const source_range = SourceRange.initNoRef(context.getDefinitionExecutable(), range);

                const inst = bytecode.Instruction{
                    .target = block.getTargetLocation(index),
                    .opcode = block.getOpcode(index),
                    .payload = block.getPayload(index),
                    .source_range = range,
                };
                std.debug.print("[#{} {} {s}] Executing: {} = {}\n", .{
                    @intFromEnum(context.actor.id),
                    source_range,
                    context.getCurrentActivation().creator_message.getValues(),
                    inst.target,
                    inst,
                });
            }

            const new_instruction_index = new_instruction_index: {
                const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<" ++ @tagName(opcode) ++ ">");
                defer tracy_ctx.end();

                context.actor.range = context.getCurrentBytecodeBlock().getSourceRange(context.getInstructionIndex());
                const result = try @call(.always_inline, opcodeHandler(opcode), .{context});

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
    };
}

// --- Opcode handlers ---

fn opcodeSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .Send);
    const receiver = context.vm.readRegister(payload.receiver_location);
    const executable_map = context.getCurrentActivationObject().getExecutableMap();
    return try performSend(context.vm, receiver, payload.selector, executable_map, payload.send_index, block.getTargetLocation(index), context.getSourceRange());
}

fn opcodeSelfSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .SelfSend);
    const receiver = context.actor.activation_stack.getCurrent().activation_object.value;
    const executable_map = context.getCurrentActivationObject().getExecutableMap();
    return try performSend(context.vm, receiver, payload.selector, executable_map, payload.send_index, block.getTargetLocation(index), context.getSourceRange());
}

fn opcodePrimSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .PrimSend);
    const receiver = context.vm.readRegister(payload.receiver_location);
    return try performPrimitiveSend(receiver, payload.index, block.getTargetLocation(index), context.getSourceRange());
}

fn opcodeSelfPrimSend(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .SelfPrimSend);
    const receiver = context.actor.activation_stack.getCurrent().activation_object.get().findActivationReceiver();
    return try performPrimitiveSend(receiver, payload.index, block.getTargetLocation(index), context.getSourceRange());
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

    var token = try context.vm.heap.allocate(FloatObject.requiredSizeForAllocation());
    defer token.deinit();

    const value = block.getTypedPayload(index, .CreateFloatingPoint);
    const float_object = FloatObject.create(&token, context.actor.id, value);

    context.vm.writeRegister(block.getTargetLocation(index), float_object.asValue());
    return ExecutionResult.normal();
}

fn opcodeCreateByteArray(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .CreateByteArray);
    var token = try context.vm.heap.allocate(ByteArray.requiredSizeForAllocation());
    defer token.deinit();

    const byte_array = try ByteArray.createWithValues(context.vm.allocator, &context.vm.heap, &token, context.actor.id, payload);
    context.vm.writeRegister(block.getTargetLocation(index), byte_array.asValue());
    return ExecutionResult.normal();
}

fn opcodeCreateObject(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const executable = context.getDefinitionExecutable();
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const object_descriptor_index = block.getTypedPayload(index, .CreateObject).descriptor_index;
    try createObject(
        executable.value.getObjectDescriptor(object_descriptor_index),
        block.getTargetLocation(index),
    );
    return ExecutionResult.normal();
}

fn opcodeCreateMethod(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const executable = context.getDefinitionExecutable();
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .CreateMethod);
    const method_name = context.vm.readRegister(payload.method_name_location).unsafeAsObject().unsafeAsType(.ByteArray);

    try createMethod(
        executable,
        method_name,
        executable.value.getObjectDescriptor(payload.descriptor_index),
        payload.block_index,
        block.getTargetLocation(index),
    );
    return ExecutionResult.normal();
}

fn opcodeCreateBlock(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const executable = context.getDefinitionExecutable();
    const block = context.getCurrentBytecodeBlock();
    const index = context.getInstructionIndex();

    const payload = block.getTypedPayload(index, .CreateBlock);
    try createBlock(
        executable,
        executable.value.getObjectDescriptor(payload.descriptor_index),
        payload.block_index,
        block.getTargetLocation(index),
    );
    return ExecutionResult.normal();
}

fn opcodeReturn(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const index = context.getInstructionIndex();
    const payload = context.getCurrentBytecodeBlock().getTypedPayload(index, .Return);

    const value = context.vm.readRegister(payload.value_location);
    context.vm.writeRegister(.ret, value);

    if (context.actor.exitCurrentActivation(context.last_activation_ref) == .LastActivation) {
        return ExecutionResult.resolve(context.vm.readRegister(.ret));
    }

    return ExecutionResult.changeActivation();
}

fn opcodeNonlocalReturn(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const index = context.getInstructionIndex();
    const payload = context.getCurrentBytecodeBlock().getTypedPayload(index, .NonlocalReturn);

    const value = context.vm.readRegister(payload.value_location);
    context.vm.writeRegister(.ret, value);

    const current_activation = context.actor.activation_stack.getCurrent();
    // FIXME: Better name
    const target_activation_ref = current_activation.nonlocal_return_target_activation.?;
    const target_activation = target_activation_ref.get(context.actor.activation_stack) orelse {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(context.getSourceRange(), "Attempted to non-local return to non-existent activation"));
    };

    if (context.actor.exitActivation(context.last_activation_ref, target_activation) == .LastActivation) {
        return ExecutionResult.resolve(context.vm.readRegister(.ret));
    }

    return ExecutionResult.changeActivation();
}

fn opcodePushArg(context: *InterpreterContext) InterpreterError!ExecutionResult {
    const index = context.getInstructionIndex();
    const payload = context.getCurrentBytecodeBlock().getTypedPayload(index, .PushArg);

    var argument = context.vm.readRegister(payload.argument_location);
    if (argument.asObject()) |argument_object| {
        if (argument_object.asType(.Activation)) |activation| {
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
    vm: *const VirtualMachine,
    receiver: Value,
    selector: Selector,
    executable_map: ExecutableMap.Ptr,
    send_index: u32,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
    const result = try sendMessage(vm, receiver, selector, .{ .executable_map = executable_map, .send_index = send_index }, target_location, source_range);
    switch (result) {
        .Resolved => |v| {
            var value = v;
            if (value.asObject()) |object| {
                if (object.asType(.Activation)) |activation| {
                    value = activation.findActivationReceiver();
                }
            }

            vm_context.getVM().writeRegister(target_location, value);
            return ExecutionResult.normal();
        },
        else => return result,
    }
}

fn performPrimitiveSend(
    receiver_: Value,
    index: primitives.PrimitiveIndex,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
    const heap = vm_context.getHeap();

    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(heap);
    defer handles.deinit(heap);

    const primitive = primitives.getPrimitive(index);

    var receiver = receiver_;
    handles.trackValue(&receiver);
    if (receiver.asObject()) |receiver_object| {
        if (receiver_object.asType(.Activation)) |activation| {
            receiver = activation.findActivationReceiver();
        }
    }

    const actor = vm_context.getActor();
    const argument_slice = vm_context.getActor().argument_stack.lastNItems(primitive.arity);

    const primitive_result = try primitive.call(receiver, argument_slice, target_location, source_range);
    if (!(primitive_result == .ActorYielded and actor.yield_reason == .Blocked)) {
        // NOTE: If the actor got blocked, it will retry the same primitive
        //       call when it gets unblocked, so we shouldn't pop values off
        //       its stack.
        actor.argument_stack.popNItems(primitive.arity);
    }

    switch (primitive_result) {
        .Resolved => |value| {
            vm_context.getVM().writeRegister(target_location, value);
            return ExecutionResult.normal();
        },
        else => return primitive_result,
    }
}

/// The location to use for getting and putting the inline cache.
pub const InlineCacheLocation = struct {
    /// The executable map the inline cache is stored in.
    executable_map: ExecutableMap.Ptr,
    /// The index of the send instruction in the executable map.
    send_index: u32,
};

/// Sends a message to the given receiver, returning the result if it can be
/// immediately resolved. If the message send must create a new activation,
/// pushes the activation onto the stack and signals that the activation has
/// changed. If the message send fails, returns the runtime error.
pub fn sendMessage(
    vm: *const VirtualMachine,
    receiver: Value,
    selector: Selector,
    inline_cache_location: ?InlineCacheLocation,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
    const actor = vm_context.getActor();

    var real_receiver = real_receiver: {
        if (receiver.asObject()) |receiver_object| {
            if (receiver_object.asType(.Activation)) |activation| {
                break :real_receiver activation.findActivationReceiver();
            }
        }

        break :real_receiver receiver;
    };

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    // FIXME: Only activate this when the message looks like a block execution.
    {
        if (real_receiver.asObject()) |block_object| {
            if (block_object.asType(.Block)) |receiver_as_block| {
                // FIXME: Check hash here instead of name.
                if (receiver_as_block.isCorrectMessageForBlockExecution(selector.name)) {
                    const argument_count = receiver_as_block.getArgumentSlotCount();
                    const argument_slice = actor.argument_stack.lastNItems(argument_count);

                    // Advance the instruction for the activation that will be returned to.
                    _ = actor.activation_stack.getCurrent().advanceInstruction();

                    try executeBlock(receiver_as_block, argument_slice, target_location, source_range);

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

    const value_slot = value_slot: {
        if (inline_cache_location) |location| {
            const maybe_cached_value_slot = location.executable_map.getCachedValueSlot(vm, location.send_index, real_receiver);
            if (maybe_cached_value_slot) |cached_value_slot| {
                // If the inline cache is hit, we can use the cached value slot directly.
                break :value_slot cached_value_slot;
            }
        }

        // Cache miss: Perform a lookup. :(
        const lookup_result = receiver.lookup(selector);
        break :value_slot switch (lookup_result) {
            .Found => |lookup_target| found_value_slot: {
                @branchHint(.likely);
                if (inline_cache_location) |location| {
                    // Cache this so our future lookups can use it. :)
                    location.executable_map.cacheLookupTarget(vm, location.send_index, real_receiver, lookup_target);
                }
                break :found_value_slot lookup_target.value_slot;
            },
            .FoundUncacheable => |value_slot| value_slot,

            .Nothing => {
                @branchHint(.cold);
                return ExecutionResult.runtimeError(
                    try RuntimeError.initFormatted(source_range, "Unknown selector {}", .{selector}),
                );
            },
            // FIXME: Factor this out into a separate function, it makes this bit
            //        of code quite noisy.
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
                    new_arguments_slice[i] = try bless.bless(target_actor.id, argument);
                }

                try target_actor.putMessageInMailbox(
                    vm.allocator,
                    actor.actor_object.get(),
                    method,
                    new_arguments_slice,
                    source_range,
                );

                actor.argument_stack.popNItems(argument_count);
                return ExecutionResult.resolve(vm.global_nil);
            },
        };
    };

    return resolve: switch (value_slot) {
        .Constant => |target| {
            if (target.asObject()) |target_object| {
                if (target_object.asType(.Method)) |method| {
                    const argument_count = method.getArgumentSlotCount();
                    const argument_slice = actor.argument_stack.lastNItems(argument_count);

                    // Advance the instruction for the activation that will be returned to.
                    _ = actor.activation_stack.getCurrent().advanceInstruction();

                    try executeMethod(receiver, method, argument_slice, target_location, source_range);

                    actor.argument_stack.popNItems(argument_count);
                    // Bump the argument stack height of the (now current)
                    // activation since we've now popped this activation's items
                    // off it.
                    actor.activation_stack.getCurrent().stack_snapshot.bumpArgumentHeight(actor);
                    return ExecutionResult.changeActivation();
                }
            }

            return ExecutionResult.resolve(target);
        },
        .Assignable => |assignment_context| {
            // Just because it's assignable doesn't mean we're actually
            // assigning right now! Check if we are actually performing an
            // assignment.
            if (!selector.canAssignTo(assignment_context.selector)) {
                // No, this was a simple lookup. Treat it like a constant.
                continue :resolve .{ .Constant = assignment_context.value_ptr.* };
            }

            const argument_slice = actor.argument_stack.lastNItems(1);
            var argument = argument_slice[0];
            // NOTE: This is required, for instance, when we are assigning `self` to
            //       a slot (happens more often than you might think!). We need to strip
            //       the activation object to get to the actual value inside.
            if (argument.asObject()) |argument_object| {
                if (argument_object.asType(.Activation)) |activation| {
                    argument = activation.findActivationReceiver();
                }
            }

            const object_that_has_the_assignable_slot = assignment_context.object;
            const value_ptr = assignment_context.value_ptr;

            if (!actor.canWriteTo(object_that_has_the_assignable_slot.asValue())) {
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(source_range, "Assignment target is not writable for actor"));
            }

            if (object_that_has_the_assignable_slot.getMetadata().reachability == .Global) {
                // Mark every object that's not globally reachable in the
                // argument's object graph as globally reachable. This will
                // make the whole object graph part of the global object
                // hierarchy.
                _ = traversal.traverseNonGloballyReachableObjectGraph(argument, struct {
                    pub fn visit(self: @This(), base_object: BaseObject.Ptr) error{}!BaseObject.Ptr {
                        _ = self;
                        base_object.metadata.reachability = .Global;
                        return base_object;
                    }
                }{}) catch unreachable;
            }

            value_ptr.* = argument;

            // David will remember that.
            _ = try vm_context.getHeap().rememberObjectReference(object_that_has_the_assignable_slot.asValue(), argument);

            actor.argument_stack.popNItems(1);
            return ExecutionResult.resolve(receiver);
        },
    };
}

fn executeBlock(
    block_receiver: BlockObject.Ptr,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    const heap = vm_context.getHeap();

    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(heap);
    defer handles.deinit(heap);

    const message_name = try vm_context.getVM().getOrCreateBlockMessageName(@intCast(arguments.len));

    var block = block_receiver;
    handles.trackObject(&block);

    const required_memory = ActivationObject.requiredSizeForAllocation(
        block.getArgumentSlotCount(),
        block.getAssignableSlotCount(),
    );

    var token = try heap.allocate(required_memory);
    defer token.deinit();

    const actor = vm_context.getActor();
    const parent_activation_object = block.getMap().parent_activation.get(actor.activation_stack).?.activation_object;
    const activation_slot = try actor.activation_stack.getNewActivationSlot(vm_context.getVM().allocator);
    block.activateBlock(
        &token,
        parent_activation_object.value,
        arguments,
        target_location,
        try message_name.get(vm_context.getVM().allocator),
        source_range,
        activation_slot,
    );
}

fn executeMethod(
    const_receiver: Value,
    const_method: MethodObject.Ptr,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    const heap = vm_context.getHeap();

    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(heap);
    defer handles.deinit(heap);

    var receiver_of_method = const_receiver;
    handles.trackValue(&receiver_of_method);

    var method = const_method;
    handles.trackObject(&method);

    const required_memory = ActivationObject.requiredSizeForAllocation(
        method.getArgumentSlotCount(),
        method.getAssignableSlotCount(),
    );

    var token = try heap.allocate(required_memory);
    defer token.deinit();

    // NOTE: The receiver of a method activation must never be an activation
    //       object (unless it explicitly wants that), as that would allow
    //       us to access the slots of upper scopes.
    if (!method.expectsActivationObjectAsReceiver()) {
        if (receiver_of_method.asObject()) |receiver_object| {
            if (receiver_object.asType(.Activation)) |activation| {
                receiver_of_method = activation.findActivationReceiver();
            }
        }
    }

    const activation_slot = try vm_context.getActor().activation_stack.getNewActivationSlot(vm_context.getVM().allocator);
    method.activateMethod(&token, vm_context.getActor().id, receiver_of_method, arguments, target_location, source_range, activation_slot);
}

fn createObject(
    object_descriptor: bytecode.ObjectDescriptor,
    target_location: bytecode.RegisterLocation,
) !void {
    const actor = vm_context.getActor();

    // FIXME: We currently have to do this object descriptor -> slot conversion
    //        because of what the `Slot` code expects. That code can be
    //        significantly simplified (most of the complexity is a leftover
    //        from when I had experimented with inherited slots) and this
    //        conversion can be removed.
    var slots: std.ArrayListUnmanaged(Slot) = try .initCapacity(
        // FIXME: Take the VM directly.
        vm_context.getVM().allocator,
        object_descriptor.slots.len,
    );
    slots.items.len = object_descriptor.slots.len;
    defer slots.deinit(vm_context.getVM().allocator);

    // NOTE: We have to go backwards when creating the slots because the initial
    //       values of slots are pushed to the argument stack at this point,
    //       with the last slot's initial value being at the top of the stack.
    var slot_descriptor_i = object_descriptor.slots.len;
    while (slot_descriptor_i > 0) : (slot_descriptor_i -= 1) {
        const index = slot_descriptor_i - 1;
        const slot_descriptor = object_descriptor.slots[index];

        var token = try vm_context.getHeap().allocate(ByteArray.requiredSizeForAllocation());
        defer token.deinit();

        // NOTE: Per ObjectDescriptor documentation, an initial value only
        //       exists for non-argument slots.
        const slot_value = if (slot_descriptor.type != .Argument)
            actor.argument_stack.pop()
        else
            null;
        const slot_name = try ByteArray.createWithValues(vm_context.getVM().allocator, vm_context.getHeap(), &token, actor.id, slot_descriptor.name);

        const slot = switch (slot_descriptor.type) {
            .Regular => Slot.initRegular(slot_name, if (slot_descriptor.assignable) .Assignable else .Constant, slot_value.?),
            .Parent => Slot.initParent(slot_name, if (slot_descriptor.assignable) .Assignable else .Constant, slot_value.?),
            .Argument => Slot.initArgument(slot_name),
        };
        slots.items[index] = slot;
    }

    var total_slot_count: u16 = 0;
    var total_assignable_slot_count: u8 = 0;
    for (slots.items, 0..) |slot, i| {
        total_slot_count += slot.requiredSlotSpace(slots.items[0..i]);
        total_assignable_slot_count += @intCast(slot.requiredAssignableSlotValueSpace(slots.items[0..i]));
    }

    var token = try vm_context.getHeap().allocate(
        SlotsMap.requiredSizeForAllocation(total_slot_count) +
            SlotsObject.requiredSizeForAllocation(total_assignable_slot_count),
    );
    defer token.deinit();

    const slots_map = SlotsMap.create(&token, total_slot_count);
    var map_builder: SlotsMap.MapBuilder = undefined;
    map_builder.initInPlace(&token, slots_map);

    for (slots.items) |slot| {
        map_builder.addSlot(slot);
    }

    const the_slots_object = map_builder.createObject();
    vm_context.getVM().writeRegister(target_location, the_slots_object.asValue());
}

fn createMethod(
    executable: bytecode.Executable.Ref,
    method_name: ByteArray.Ptr,
    object_descriptor: bytecode.ObjectDescriptor,
    block_index: u32,
    target_location: bytecode.RegisterLocation,
) !void {
    const actor = vm_context.getActor();
    defer actor.next_method_is_inline = false;

    // FIXME: We currently have to do this object descriptor -> slot conversion
    //        because of what the `Slot` code expects. That code can be
    //        significantly simplified (most of the complexity is a leftover
    //        from when I had experimented with inherited slots) and this
    //        conversion can be removed.
    var slots: std.ArrayListUnmanaged(Slot) = try .initCapacity(
        // FIXME: Take the VM directly.
        vm_context.getVM().allocator,
        object_descriptor.slots.len,
    );
    slots.items.len = object_descriptor.slots.len;
    defer slots.deinit(vm_context.getVM().allocator);

    // NOTE: We have to go backwards when creating the slots because the initial
    //       values of slots are pushed to the argument stack at this point,
    //       with the last slot's initial value being at the top of the stack.
    var slot_descriptor_i = object_descriptor.slots.len;
    while (slot_descriptor_i > 0) : (slot_descriptor_i -= 1) {
        const index = slot_descriptor_i - 1;
        const slot_descriptor = object_descriptor.slots[index];

        var token = try vm_context.getHeap().allocate(ByteArray.requiredSizeForAllocation());
        defer token.deinit();

        // NOTE: Per ObjectDescriptor documentation, an initial value only
        //       exists for non-argument slots.
        const slot_value = if (slot_descriptor.type != .Argument)
            actor.argument_stack.pop()
        else
            null;
        const slot_name = try ByteArray.createWithValues(vm_context.getVM().allocator, vm_context.getHeap(), &token, actor.id, slot_descriptor.name);

        const slot = switch (slot_descriptor.type) {
            .Regular => Slot.initRegular(slot_name, if (slot_descriptor.assignable) .Assignable else .Constant, slot_value.?),
            .Parent => Slot.initParent(slot_name, if (slot_descriptor.assignable) .Assignable else .Constant, slot_value.?),
            .Argument => Slot.initArgument(slot_name),
        };
        slots.items[index] = slot;
    }

    var total_slot_count: u16 = 0;
    var total_assignable_slot_count: u8 = 0;
    var argument_slot_count: u8 = 0;
    for (slots.items, 0..) |slot, i| {
        total_slot_count += slot.requiredSlotSpace(slots.items[0..i]);
        total_assignable_slot_count += @intCast(slot.requiredAssignableSlotValueSpace(slots.items[0..i]));

        // FIXME: This makes the assumption that argument slots are
        //        never overwritten.
        if (slot.isArgument())
            argument_slot_count += 1;
    }

    var token = try vm_context.getHeap().allocate(
        MethodMap.requiredSizeForAllocation(total_slot_count) +
            MethodObject.requiredSizeForAllocation(total_assignable_slot_count),
    );
    defer token.deinit();

    const block = executable.value.getBlock(block_index);
    const method_map = try MethodMap.create(
        vm_context.getVM().allocator,
        vm_context.getHeap(),
        &token,
        argument_slot_count,
        total_slot_count,
        actor.next_method_is_inline,
        method_name,
        block,
        executable,
    );

    var map_builder: MethodMap.MapBuilder = undefined;
    map_builder.initInPlace(&token, method_map);

    for (slots.items) |slot| {
        map_builder.addSlot(slot);
    }

    const method = map_builder.createObject();
    vm_context.getVM().writeRegister(target_location, method.asValue());
}

fn createBlock(
    executable: bytecode.Executable.Ref,
    object_descriptor: bytecode.ObjectDescriptor,
    block_index: u32,
    target_location: bytecode.RegisterLocation,
) !void {
    const actor = vm_context.getActor();

    // FIXME: We currently have to do this object descriptor -> slot conversion
    //        because of what the `Slot` code expects. That code can be
    //        significantly simplified (most of the complexity is a leftover
    //        from when I had experimented with inherited slots) and this
    //        conversion can be removed.
    var slots: std.ArrayListUnmanaged(Slot) = try .initCapacity(
        // FIXME: Take the VM directly.
        vm_context.getVM().allocator,
        object_descriptor.slots.len,
    );
    slots.items.len = object_descriptor.slots.len;
    defer slots.deinit(vm_context.getVM().allocator);

    // NOTE: We have to go backwards when creating the slots because the initial
    //       values of slots are pushed to the argument stack at this point,
    //       with the last slot's initial value being at the top of the stack.
    var slot_descriptor_i = object_descriptor.slots.len;
    while (slot_descriptor_i > 0) : (slot_descriptor_i -= 1) {
        const index = slot_descriptor_i - 1;
        const slot_descriptor = object_descriptor.slots[index];

        var token = try vm_context.getHeap().allocate(ByteArray.requiredSizeForAllocation());
        defer token.deinit();

        // NOTE: Per ObjectDescriptor documentation, an initial value only
        //       exists for non-argument slots.
        const slot_value = if (slot_descriptor.type != .Argument)
            actor.argument_stack.pop()
        else
            null;
        const slot_name = try ByteArray.createWithValues(vm_context.getVM().allocator, vm_context.getHeap(), &token, actor.id, slot_descriptor.name);

        const slot = switch (slot_descriptor.type) {
            .Regular => Slot.initRegular(slot_name, if (slot_descriptor.assignable) .Assignable else .Constant, slot_value.?),
            .Parent => Slot.initParent(slot_name, if (slot_descriptor.assignable) .Assignable else .Constant, slot_value.?),
            .Argument => Slot.initArgument(slot_name),
        };
        slots.items[index] = slot;
    }

    var total_slot_count: u16 = 0;
    var total_assignable_slot_count: u15 = 0;
    var argument_slot_count: u8 = 0;
    for (slots.items, 0..) |slot, i| {
        total_slot_count += slot.requiredSlotSpace(slots.items[0..i]);
        total_assignable_slot_count += @intCast(slot.requiredAssignableSlotValueSpace(slots.items[0..i]));

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

    var token = try vm_context.getHeap().allocate(
        BlockMap.requiredSizeForAllocation(total_slot_count) +
            BlockObject.requiredSizeForAllocation(total_assignable_slot_count),
    );
    defer token.deinit();

    const block_map = try BlockMap.create(
        vm_context.getVM().allocator,
        vm_context.getHeap(),
        &token,
        argument_slot_count,
        total_slot_count,
        parent_activation.takeRef(actor.activation_stack),
        nonlocal_return_target_activation,
        block,
        executable,
    );

    var map_builder: BlockMap.MapBuilder = undefined;
    map_builder.initInPlace(&token, block_map);

    for (slots.items) |slot| {
        map_builder.addSlot(slot);
    }

    const the_block_object = map_builder.createObject();
    vm_context.getVM().writeRegister(target_location, the_block_object.asValue());
}
