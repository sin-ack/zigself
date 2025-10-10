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
const AllocationToken = @import("Heap.zig").AllocationToken;
const ExecutionResult = @import("execution_result.zig").ExecutionResult;
const ActivationObject = @import("objects/activation.zig").Activation;

const EXECUTION_DEBUG = debug.EXECUTION_DEBUG;

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

const Interpreter = @This();
pub const Error = Allocator.Error;
const ActivationContext = struct {
    /// The executable the current activation's method/block was defined in.
    executable: bytecode.Executable.Ref,
    /// The bytecode block for the current activation. Each activation
    /// corresponds to exactly one block.
    block: *const bytecode.Block,
};

pub fn init(last_activation_ref: ?Activation.ActivationRef) Interpreter {
    var interpreter: Interpreter = .{
        .vm = vm_context.getVM(),
        .actor = vm_context.getActor(),
        .last_activation_ref = last_activation_ref,
    };
    interpreter.refreshActivationContext();

    return interpreter;
}

fn getSourceRange(self: *Interpreter) SourceRange {
    return SourceRange.initNoRef(self.getDefinitionExecutable(), self.actor.range);
}

fn getCurrentActivation(self: Interpreter) *Activation {
    return self.actor.activation_stack.getCurrent();
}

fn getInstructionIndex(self: Interpreter) u32 {
    return self.getCurrentActivation().instruction_index;
}

fn getCurrentActivationObject(self: Interpreter) ActivationObject.Ptr {
    return self.getCurrentActivation().activation_object.get();
}

fn getDefinitionExecutable(self: *Interpreter) bytecode.Executable.Ref {
    return self.activation_context.executable;
}

fn getCurrentBytecodeBlock(self: *Interpreter) *const bytecode.Block {
    return self.activation_context.block;
}

fn refreshActivationContext(self: *Interpreter) void {
    const activation_object = self.getCurrentActivationObject();
    self.activation_context = .{
        .executable = activation_object.getDefinitionExecutable(),
        .block = activation_object.getBytecodeBlock(),
    };
}

fn getCurrentOpcode(self: *Interpreter) bytecode.Instruction.Opcode {
    return self.getCurrentBytecodeBlock().getOpcode(self.getInstructionIndex());
}

fn printCurrentInstruction(self: *Interpreter) void {
    const block = self.getCurrentBytecodeBlock();
    const index = self.getInstructionIndex();
    const range = self.getSourceRange().range;

    const source_range = SourceRange.initNoRef(self.getDefinitionExecutable(), range);

    const inst = bytecode.Instruction{
        .target = block.getTargetLocation(index),
        .opcode = block.getOpcode(index),
        .payload = block.getPayload(index),
        .source_range = range,
    };
    std.debug.print("[#{} {f} {s}] Executing: {f} = {f}\n", .{
        @intFromEnum(self.actor.id),
        source_range,
        self.getCurrentActivation().creator_message.getValues(),
        inst.target,
        inst,
    });
}

/// Run code before each instruction.
fn prelude(self: *Interpreter) void {
    if (EXECUTION_DEBUG) {
        self.printCurrentInstruction();
    }

    self.actor.range = self.getCurrentBytecodeBlock().getSourceRange(self.getInstructionIndex());
}

pub fn execute(self: *Interpreter) Error!Actor.ActorResult {
    next_opcode: switch (self.getCurrentOpcode()) {
        .Send, .SelfSend => |opcode| {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<Send>");
            defer tracy_ctx.end();
            self.prelude();

            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            var selector: Selector = undefined;
            var send_index: u32 = undefined;
            var receiver: Value = undefined;
            if (opcode == .Send) {
                const payload = block.getTypedPayload(index, .Send);
                selector = payload.selector;
                send_index = payload.send_index;
                receiver = self.vm.readRegister(payload.receiver_location);
            } else {
                const payload = block.getTypedPayload(index, .SelfSend);
                selector = payload.selector;
                send_index = payload.send_index;
                receiver = self.actor.activation_stack.getCurrent().activation_object.value;
            }

            const executable_map = self.getCurrentActivationObject().getExecutableMap();
            const target_location = block.getTargetLocation(index);
            const result = try self.sendMessage(receiver, selector, .{ .executable_map = executable_map, .send_index = send_index }, target_location, self.getSourceRange());
            switch (result) {
                .Resolved => |const_value| {
                    var value = const_value;
                    if (value.asObject()) |object| {
                        if (object.asType(.Activation)) |activation| {
                            value = activation.findActivationReceiver();
                        }
                    }

                    self.vm.writeRegister(target_location, value);
                    _ = self.getCurrentActivation().advanceInstruction();
                },
                .ActivationChanged => self.refreshActivationContext(),
                .Restarted => _ = self.getCurrentActivation().restart(),

                .ActorYielded => return .{ .Switched = {} },
                .RuntimeError => |err| return .{ .RuntimeError = err },
                .Normal => unreachable,
            }

            continue :next_opcode self.getCurrentOpcode();
        },
        .PrimSend, .SelfPrimSend => |opcode| {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<PrimSend>");
            defer tracy_ctx.end();
            self.prelude();

            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            var prim_index: primitives.PrimitiveIndex = undefined;
            var receiver: Value = undefined;
            if (opcode == .PrimSend) {
                const payload = block.getTypedPayload(index, .PrimSend);
                prim_index = payload.index;
                receiver = self.vm.readRegister(payload.receiver_location);
            } else {
                const payload = block.getTypedPayload(index, .SelfPrimSend);
                prim_index = payload.index;
                receiver = self.actor.activation_stack.getCurrent().activation_object.value;
            }

            const target_location = block.getTargetLocation(index);
            const result = try self.sendPrimitiveMessage(receiver, prim_index, target_location, self.getSourceRange());
            switch (result) {
                .Resolved => |value| {
                    self.vm.writeRegister(target_location, value);
                    _ = self.getCurrentActivation().advanceInstruction();
                },
                .ActivationChanged => self.refreshActivationContext(),
                .Restarted => _ = self.getCurrentActivation().restart(),

                .ActorYielded => return .{ .Switched = {} },
                .RuntimeError => |err| return .{ .RuntimeError = err },
                .Normal => unreachable,
            }

            continue :next_opcode self.getCurrentOpcode();
        },
        .CreateInteger => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<CreateInteger>");
            defer tracy_ctx.end();
            self.prelude();

            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();
            self.vm.writeRegister(block.getTargetLocation(index), Value.fromInteger(block.getTypedPayload(index, .CreateInteger)));

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .CreateFloatingPoint => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<CreateFloatingPoint>");
            defer tracy_ctx.end();
            self.prelude();

            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            var token = try self.vm.heap.allocate(FloatObject.requiredSizeForAllocation());
            defer token.deinit();

            const value = block.getTypedPayload(index, .CreateFloatingPoint);
            const float_object = FloatObject.create(&token, self.actor.id, value);

            self.vm.writeRegister(block.getTargetLocation(index), float_object.asValue());

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .CreateByteArray => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<CreateByteArray>");
            defer tracy_ctx.end();
            self.prelude();

            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            const payload = block.getTypedPayload(index, .CreateByteArray);
            var token = try self.vm.heap.allocate(ByteArray.requiredSizeForAllocation());
            defer token.deinit();

            const byte_array = try ByteArray.createWithValues(self.vm.allocator, &self.vm.heap, &token, self.actor.id, payload);
            self.vm.writeRegister(block.getTargetLocation(index), byte_array.asValue());

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .CreateObject => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<CreateObject>");
            defer tracy_ctx.end();
            self.prelude();

            const executable = self.getDefinitionExecutable();
            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            const object_descriptor_index = block.getTypedPayload(index, .CreateObject).descriptor_index;
            const object = try self.createObject(executable.value.getObjectDescriptor(object_descriptor_index));
            self.vm.writeRegister(block.getTargetLocation(index), object.asValue());

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .CreateMethod => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<CreateMethod>");
            defer tracy_ctx.end();
            self.prelude();

            const executable = self.getDefinitionExecutable();
            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            const payload = block.getTypedPayload(index, .CreateMethod);
            const method_name = self.vm.readRegister(payload.method_name_location).unsafeAsObject().unsafeAsType(.ByteArray);
            const object = try self.createMethod(
                executable,
                method_name,
                executable.value.getObjectDescriptor(payload.descriptor_index),
                payload.block_index,
            );
            self.vm.writeRegister(block.getTargetLocation(index), object.asValue());

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .CreateBlock => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<CreateBlock>");
            defer tracy_ctx.end();
            self.prelude();

            const executable = self.getDefinitionExecutable();
            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            const payload = block.getTypedPayload(index, .CreateBlock);
            const object = try self.createBlock(
                executable,
                executable.value.getObjectDescriptor(payload.descriptor_index),
                payload.block_index,
            );
            self.vm.writeRegister(block.getTargetLocation(index), object.asValue());

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .Return => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<Return>");
            defer tracy_ctx.end();
            self.prelude();

            const index = self.getInstructionIndex();
            const payload = self.getCurrentBytecodeBlock().getTypedPayload(index, .Return);

            const value = self.vm.readRegister(payload.value_location);
            self.vm.writeRegister(.ret, value);

            if (self.actor.exitCurrentActivation(self.last_activation_ref) == .LastActivation) {
                return .{ .Finished = self.vm.readRegister(.ret) };
            }

            self.refreshActivationContext();
            continue :next_opcode self.getCurrentOpcode();
        },
        .NonlocalReturn => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<NonlocalReturn>");
            defer tracy_ctx.end();
            self.prelude();

            const index = self.getInstructionIndex();
            const payload = self.getCurrentBytecodeBlock().getTypedPayload(index, .NonlocalReturn);

            const value = self.vm.readRegister(payload.value_location);
            self.vm.writeRegister(.ret, value);

            const current_activation = self.actor.activation_stack.getCurrent();
            // FIXME: Better name
            const target_activation_ref = current_activation.nonlocal_return_target_activation.?;
            const target_activation = target_activation_ref.get(self.actor.activation_stack) orelse {
                return .{ .RuntimeError = .initLiteral(self.getSourceRange(), "Attempted to non-local return to non-existent activation") };
            };

            if (self.actor.exitActivation(self.last_activation_ref, target_activation) == .LastActivation) {
                return .{ .Finished = self.vm.readRegister(.ret) };
            }

            self.refreshActivationContext();
            continue :next_opcode self.getCurrentOpcode();
        },
        .PushArg => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<PushArg>");
            defer tracy_ctx.end();
            self.prelude();

            const index = self.getInstructionIndex();
            const payload = self.getCurrentBytecodeBlock().getTypedPayload(index, .PushArg);

            var argument = self.vm.readRegister(payload.argument_location);
            if (argument.asObject()) |argument_object| {
                if (argument_object.asType(.Activation)) |activation| {
                    argument = activation.findActivationReceiver();
                }
            }

            try self.actor.argument_stack.push(self.vm.allocator, argument);

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .PushRegisters => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<PushRegisters>");
            defer tracy_ctx.end();
            self.prelude();

            var iterator = self.getCurrentBytecodeBlock().getTypedPayload(self.getInstructionIndex(), .PushRegisters).iterator(.{});
            while (iterator.next()) |clobbered_register| {
                // FIXME: Remove manual register number adjustment!
                const register = bytecode.RegisterLocation.fromInt(@intCast(clobbered_register + 2));
                try self.actor.saved_register_stack.push(self.vm.allocator, .{ .register = register, .value = self.vm.readRegister(register) });
            }

            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .SetMethodInline => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<SetMethodInline>");
            defer tracy_ctx.end();
            self.prelude();

            self.actor.next_method_is_inline = true;
            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .PushArgumentSentinel => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<PushArgumentSentinel>");
            defer tracy_ctx.end();
            self.prelude();

            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            try self.actor.argument_stack.pushSentinel(self.vm.allocator, block.getTypedPayload(index, .PushArgumentSentinel));
            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
        .VerifyArgumentSentinel => {
            const tracy_ctx = tracy.traceNamed(@src(), "Interpreter.execute<VerifyArgumentSentinel>");
            defer tracy_ctx.end();
            self.prelude();

            const block = self.getCurrentBytecodeBlock();
            const index = self.getInstructionIndex();

            self.actor.argument_stack.verifySentinel(block.getTypedPayload(index, .VerifyArgumentSentinel));
            _ = self.getCurrentActivation().advanceInstruction();
            continue :next_opcode self.getCurrentOpcode();
        },
    }
}

// --- Utility functions ---

fn sendPrimitiveMessage(
    self: *const Interpreter,
    receiver_: Value,
    index: primitives.PrimitiveIndex,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(&self.vm.heap);
    defer handles.deinit(&self.vm.heap);

    const primitive = primitives.getPrimitive(index);

    var receiver = receiver_;
    handles.trackValue(&receiver);
    if (receiver.asObject()) |receiver_object| {
        if (receiver_object.asType(.Activation)) |activation| {
            receiver = activation.findActivationReceiver();
        }
    }

    const argument_slice = self.actor.argument_stack.lastNItems(primitive.arity);

    const primitive_result = try primitive.call(self, receiver, argument_slice, target_location, source_range);
    if (!(primitive_result == .ActorYielded and self.actor.yield_reason == .Blocked)) {
        // NOTE: If the actor got blocked, it will retry the same primitive
        //       call when it gets unblocked, so we shouldn't pop values off
        //       its stack.
        self.actor.argument_stack.popNItems(primitive.arity);
    }

    return primitive_result;
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
    self: *const Interpreter,
    receiver: Value,
    selector: Selector,
    inline_cache_location: ?InlineCacheLocation,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !ExecutionResult {
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
                    const argument_slice = self.actor.argument_stack.lastNItems(argument_count);

                    // Advance the instruction for the activation that will be returned to.
                    _ = self.getCurrentActivation().advanceInstruction();

                    try self.executeBlock(receiver_as_block, argument_slice, target_location, source_range);

                    self.actor.argument_stack.popNItems(argument_count);
                    // Bump the argument stack height of the (now current)
                    // activation since we've now popped this activation's items off
                    // it.
                    self.getCurrentActivation().stack_snapshot.bumpArgumentHeight(self.actor);
                    return ExecutionResult.changeActivation();
                }
            }
        }
    }

    self.actor.ensureCanRead(receiver, source_range);

    const value_slot = value_slot: {
        if (inline_cache_location) |location| {
            const maybe_cached_value_slot = location.executable_map.getCachedValueSlot(self.vm, location.send_index, real_receiver);
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
                    location.executable_map.cacheLookupTarget(self.vm, location.send_index, real_receiver, lookup_target);
                }
                break :found_value_slot lookup_target.value_slot;
            },
            .FoundUncacheable => |value_slot| value_slot,

            .Nothing => {
                @branchHint(.cold);
                return ExecutionResult.runtimeError(
                    try RuntimeError.initFormatted(source_range, "Unknown selector {f}", .{selector}),
                );
            },
            // FIXME: Factor this out into a separate function, it makes this bit
            //        of code quite noisy.
            .ActorMessage => |actor_message| {
                const method = actor_message.method;
                const argument_count = method.getArgumentSlotCount();
                const argument_slice = self.actor.argument_stack.lastNItems(argument_count);

                var target_actor = actor_message.target_actor.getActor();

                // FIXME: Figure out a way to avoid creating an owned slice here.
                //        This is required for the time being because we don't have
                //        a better first-in-last-out (which is how messages are
                //        processed) structure yet.
                const new_arguments_slice = try self.vm.allocator.alloc(Value, argument_slice.len);
                errdefer self.vm.allocator.free(new_arguments_slice);

                // Blessing each argument is required so that actors don't share memory.
                for (argument_slice, 0..) |argument, i| {
                    new_arguments_slice[i] = try bless.bless(target_actor.id, argument);
                }

                try target_actor.putMessageInMailbox(
                    self.vm.allocator,
                    self.actor.actor_object.get(),
                    method,
                    new_arguments_slice,
                    source_range,
                );

                self.actor.argument_stack.popNItems(argument_count);
                return ExecutionResult.resolve(self.vm.global_nil);
            },
        };
    };

    return resolve: switch (value_slot) {
        .Constant => |target| {
            if (target.asObject()) |target_object| {
                if (target_object.asType(.Method)) |method| {
                    const argument_count = method.getArgumentSlotCount();
                    const argument_slice = self.actor.argument_stack.lastNItems(argument_count);

                    // Advance the instruction for the activation that will be returned to.
                    _ = self.getCurrentActivation().advanceInstruction();

                    try self.executeMethod(receiver, method, argument_slice, target_location, source_range);

                    self.actor.argument_stack.popNItems(argument_count);
                    // Bump the argument stack height of the (now current)
                    // activation since we've now popped this activation's items
                    // off it.
                    self.getCurrentActivation().stack_snapshot.bumpArgumentHeight(self.actor);
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

            const argument_slice = self.actor.argument_stack.lastNItems(1);
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

            if (!self.actor.canWriteTo(object_that_has_the_assignable_slot.asValue())) {
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(source_range, "Assignment target is not writable for actor"));
            }

            if (object_that_has_the_assignable_slot.getMetadata().reachability == .Global) {
                // Mark every object that's not globally reachable in the
                // argument's object graph as globally reachable. This will
                // make the whole object graph part of the global object
                // hierarchy.
                _ = traversal.traverseNonGloballyReachableObjectGraph(argument, struct {
                    pub fn visit(ctx: @This(), base_object: BaseObject.Ptr) error{}!BaseObject.Ptr {
                        _ = ctx;
                        base_object.metadata.reachability = .Global;
                        return base_object;
                    }
                }{}) catch unreachable;
            }

            value_ptr.* = argument;

            // David will remember that.
            _ = try self.vm.heap.rememberObjectReference(object_that_has_the_assignable_slot.asValue(), argument);

            self.actor.argument_stack.popNItems(1);
            return ExecutionResult.resolve(receiver);
        },
    };
}

fn executeBlock(
    self: *const Interpreter,
    block_receiver: BlockObject.Ptr,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(&self.vm.heap);
    defer handles.deinit(&self.vm.heap);

    const message_name = try self.vm.getOrCreateBlockMessageName(@intCast(arguments.len));

    var block = block_receiver;
    handles.trackObject(&block);

    const required_memory = ActivationObject.requiredSizeForAllocation(
        block.getArgumentSlotCount(),
        block.getAssignableSlotCount(),
    );

    var token = try self.vm.heap.allocate(required_memory);
    defer token.deinit();

    const parent_activation_object = block.getMap().parent_activation.get(self.actor.activation_stack).?.activation_object;
    const activation_slot = try self.actor.activation_stack.getNewActivationSlot(self.vm.allocator);
    block.activateBlock(
        &token,
        parent_activation_object.value,
        arguments,
        target_location,
        try message_name.get(self.vm.allocator),
        source_range,
        activation_slot,
    );
}

fn executeMethod(
    self: *const Interpreter,
    const_receiver: Value,
    const_method: MethodObject.Ptr,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(&self.vm.heap);
    defer handles.deinit(&self.vm.heap);

    var receiver_of_method = const_receiver;
    handles.trackValue(&receiver_of_method);

    var method = const_method;
    handles.trackObject(&method);

    const required_memory = ActivationObject.requiredSizeForAllocation(
        method.getArgumentSlotCount(),
        method.getAssignableSlotCount(),
    );

    var token = try self.vm.heap.allocate(required_memory);
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

    const activation_slot = try self.actor.activation_stack.getNewActivationSlot(self.vm.allocator);
    method.activateMethod(&token, self.actor.id, receiver_of_method, arguments, target_location, source_range, activation_slot);
}

fn createObject(
    self: *const Interpreter,
    object_descriptor: bytecode.ObjectDescriptor,
) !SlotsObject.Ptr {
    const slot_count: u16 = @intCast(object_descriptor.slots.len);
    const byte_array_required_memory = slot_count * ByteArray.requiredSizeForAllocation();
    var token = try self.vm.heap.allocate(
        SlotsMap.requiredSizeForAllocation(slot_count) +
            SlotsObject.requiredSizeForAllocation(object_descriptor.slots_requiring_assignable_slot_value) +
            byte_array_required_memory,
    );
    defer token.deinit();

    const slots_map = SlotsMap.create2(&token, slot_count, object_descriptor.slots_requiring_assignable_slot_value);
    const slots_object = SlotsObject.create2(&token, self.actor.id, slots_map);
    try self.writeObjectSlots(SlotsMap, &token, object_descriptor, slots_map, slots_object);
    return slots_object;
}

fn createMethod(
    self: *const Interpreter,
    executable: bytecode.Executable.Ref,
    method_name: ByteArray.Ptr,
    object_descriptor: bytecode.ObjectDescriptor,
    block_index: u32,
) !MethodObject.Ptr {
    defer self.actor.next_method_is_inline = false;

    const slot_count: u16 = @intCast(object_descriptor.slots.len);
    const byte_array_required_memory = slot_count * ByteArray.requiredSizeForAllocation();
    var token = try self.vm.heap.allocate(
        MethodMap.requiredSizeForAllocation(slot_count) +
            MethodObject.requiredSizeForAllocation(object_descriptor.slots_requiring_assignable_slot_value) +
            byte_array_required_memory,
    );
    defer token.deinit();

    const block = executable.value.getBlock(block_index);
    const method_map = try MethodMap.create(
        self.vm.allocator,
        &self.vm.heap,
        &token,
        slot_count,
        object_descriptor.slots_requiring_assignable_slot_value,
        object_descriptor.argument_slots,
        self.actor.next_method_is_inline,
        method_name,
        block,
        executable,
    );
    const method_object = MethodObject.create(&token, self.actor.id, method_map);
    try self.writeObjectSlots(MethodMap, &token, object_descriptor, method_map, method_object);
    return method_object;
}

fn createBlock(
    self: *const Interpreter,
    executable: bytecode.Executable.Ref,
    object_descriptor: bytecode.ObjectDescriptor,
    block_index: u32,
) !BlockObject.Ptr {
    const block = executable.value.getBlock(block_index);
    // The latest activation is where the block was created, so it will always
    // be the parent activation (i.e., where we look for parent blocks' and the
    // method's slots).
    const parent_activation = self.actor.activation_stack.getCurrent();
    // However, we want the _method_ as the non-local return target; because the
    // non-local return can only be returned by the method in which the block
    // making the non-local return was defined, this needs to be separate from
    // parent_activation. If the parent activation is a block, it will also
    // contain a target activation; if it's a method the target activation _is_
    // the parent.
    const nonlocal_return_target_activation = if (parent_activation.nonlocal_return_target_activation) |target|
        target
    else
        parent_activation.takeRef(self.actor.activation_stack);
    std.debug.assert(nonlocal_return_target_activation.get(self.actor.activation_stack).?.nonlocal_return_target_activation == null);

    const slot_count: u16 = @intCast(object_descriptor.slots.len);
    const byte_array_required_memory = slot_count * ByteArray.requiredSizeForAllocation();
    var token = try self.vm.heap.allocate(
        BlockMap.requiredSizeForAllocation(slot_count) +
            BlockObject.requiredSizeForAllocation(object_descriptor.slots_requiring_assignable_slot_value) +
            byte_array_required_memory,
    );
    defer token.deinit();

    const block_map = try BlockMap.create(
        self.vm.allocator,
        &self.vm.heap,
        &token,
        slot_count,
        object_descriptor.slots_requiring_assignable_slot_value,
        object_descriptor.argument_slots,
        parent_activation.takeRef(self.actor.activation_stack),
        nonlocal_return_target_activation,
        block,
        executable,
    );
    const block_object = BlockObject.create(&token, self.actor.id, block_map);
    try self.writeObjectSlots(BlockMap, &token, object_descriptor, block_map, block_object);
    return block_object;
}

fn writeObjectSlots(
    self: *const Interpreter,
    comptime MapType: type,
    token: *AllocationToken,
    object_descriptor: bytecode.ObjectDescriptor,
    map: MapType.Ptr,
    object: MapType.ObjectType.Ptr,
) !void {
    const output_slots = map.getSlots();
    const output_assignable_slot_values = object.getAssignableSlots();

    const initial_values = self.actor.argument_stack.lastNItems(object_descriptor.slots_with_initial_value);
    defer self.actor.argument_stack.popNItems(object_descriptor.slots_with_initial_value);

    var initial_values_index: usize = 0;
    // NOTE: This is different from assignable_slot_index which also tracks
    //       assignable slots with no initial value.
    var assignable_slot_value_index: usize = 0;
    for (object_descriptor.slots, 0..) |slot_descriptor, i| {
        const slot_name = try ByteArray.createWithValues(self.vm.allocator, &self.vm.heap, token, self.actor.id, slot_descriptor.name);

        // NOTE: Per ObjectDescriptor documentation, an initial value only
        //       exists for non-argument slots.
        const slot_value = if (slot_descriptor.hasInitialValue()) slot_value: {
            const value = initial_values[initial_values_index];
            initial_values_index += 1;
            break :slot_value value;
        } else null;

        const slot = switch (slot_descriptor.type) {
            .Regular => Slot.initRegular(slot_name, if (slot_descriptor.assignable_index != null) .Assignable else .Constant, slot_value.?),
            .Parent => Slot.initParent(slot_name, if (slot_descriptor.assignable_index != null) .Assignable else .Constant, slot_value.?),
            .Argument => Slot.initArgument(slot_name),
        };

        output_slots[i] = slot;
        if (slot_descriptor.assignable_index) |assignable_index| {
            output_slots[i].value = Value.fromUnsignedInteger(@intCast(assignable_index));

            if (slot_descriptor.requiresAssignableSlotValue()) {
                std.debug.assert(assignable_slot_value_index == assignable_index);

                output_assignable_slot_values[assignable_slot_value_index] = slot_value.?;
                assignable_slot_value_index += 1;
            }
        }
    }
}
