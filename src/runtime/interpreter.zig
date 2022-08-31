// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Slot = slot_import.Slot;
const debug = @import("../debug.zig");
const Actor = @import("./Actor.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./Object.zig");
const bytecode = @import("./bytecode.zig");
const ByteArray = @import("./ByteArray.zig");
const traversal = @import("./object/traversal.zig");
const Activation = @import("./Activation.zig");
const Completion = @import("./Completion.zig");
const primitives = @import("./primitives.zig");
const SourceRange = @import("./SourceRange.zig");
const slot_import = @import("./slot.zig");
const VirtualMachine = @import("./VirtualMachine.zig");

const EXECUTION_DEBUG = debug.EXECUTION_DEBUG;

/// The result of executing a single instruction.
pub const ExecutionResult = union(enum) {
    /// The current actor has been switched.
    ActorSwitch,
    /// An activation has been entered or exited (but not the last activation).
    ActivationChange,
    /// The instruction successfully executed.
    Success,
    /// A completion happened. Either the last activation was exited (in which
    /// case the a Normal completion is returned), or a runtime error was raised.
    Completion: Completion,

    pub fn success() ExecutionResult {
        return .{ .Success = {} };
    }

    pub fn activationChange() ExecutionResult {
        return .{ .ActivationChange = {} };
    }

    pub fn actorSwitch() ExecutionResult {
        return .{ .ActorSwitch = {} };
    }

    pub fn completion(c: Completion) ExecutionResult {
        return .{ .Completion = c };
    }
};

const activation_change = ExecutionResult.activationChange();
const success = ExecutionResult.success();
pub fn execute(
    vm: *VirtualMachine,
    actor: *Actor,
    last_activation_ref: ?Activation.ActivationRef,
    executable: bytecode.Executable.Ref,
    inst: bytecode.Instruction,
) !ExecutionResult {
    const source_range = SourceRange.initNoRef(executable, actor.range);

    if (EXECUTION_DEBUG) std.debug.print("[#{} {s}] Executing: {} = {}\n", .{ actor.id, executable.value.definition_script.value.file_path, inst.target, inst });

    switch (inst.value) {
        .Send => |payload| {
            const receiver = vm.readRegister(payload.receiver_location);

            const completion = (try sendMessage(vm, actor, receiver, payload.message_name, inst.target, source_range)) orelse
                return activation_change;

            if (completion.isNormal()) {
                var result = completion.data.Normal;
                if (result.isObjectReference() and result.asObject().isActivationObject()) {
                    result = result.asObject().asActivationObject().findActivationReceiver();
                }

                vm.writeRegister(inst.target, result);
                return success;
            }

            return ExecutionResult.completion(completion);
        },
        .PrimSend => |payload| {
            if (primitives.getPrimitive(payload.message_name)) |primitive| {
                var receiver = vm.readRegister(payload.receiver_location);
                if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
                    receiver = receiver.asObject().asActivationObject().findActivationReceiver();
                }

                const tracked_receiver = try vm.heap.track(receiver);
                defer tracked_receiver.untrack(vm.heap);

                const argument_slice = actor.argument_stack.lastNItems(primitive.arity);

                const primitive_result = try primitive.call(vm, actor, tracked_receiver, argument_slice, inst.target, source_range);
                if (!(primitive_result == .ActorSwitch and actor.yield_reason == .Blocked)) {
                    // NOTE: If the actor got blocked, it will retry the same
                    //       primitive call when it gets unblocked, so we
                    //       shouldn't pop values off its stack.
                    actor.argument_stack.popNItems(primitive.arity);
                }

                switch (primitive_result) {
                    .ActorSwitch, .ActivationChange, .Success => return primitive_result,
                    .Completion => |completion| {
                        if (completion.isNormal()) {
                            vm.writeRegister(inst.target, completion.data.Normal);
                            return success;
                        }

                        return ExecutionResult.completion(completion);
                    },
                }
            }

            return ExecutionResult.completion(
                try Completion.initRuntimeError(vm, source_range, "Unknown primitive selector '{s}'", .{payload.message_name}),
            );
        },
        .SelfSend => |payload| {
            const receiver = actor.activation_stack.getCurrent().activation_object.value;

            const completion = (try sendMessage(vm, actor, receiver, payload.message_name, inst.target, source_range)) orelse
                return activation_change;

            if (completion.isNormal()) {
                var result = completion.data.Normal;
                if (result.isObjectReference() and result.asObject().isActivationObject()) {
                    result = result.asObject().asActivationObject().findActivationReceiver();
                }

                vm.writeRegister(inst.target, result);
                return success;
            }

            return ExecutionResult.completion(completion);
        },
        .SelfPrimSend => |payload| {
            if (primitives.getPrimitive(payload.message_name)) |primitive| {
                var receiver = actor.activation_stack.getCurrent().activation_object.get().findActivationReceiver();
                const tracked_receiver = try vm.heap.track(receiver);
                defer tracked_receiver.untrack(vm.heap);

                const argument_slice = actor.argument_stack.lastNItems(primitive.arity);

                const primitive_result = try primitive.call(vm, actor, tracked_receiver, argument_slice, inst.target, source_range);
                if (!(primitive_result == .ActorSwitch and actor.yield_reason == .Blocked)) {
                    // NOTE: If the actor got blocked, it will retry the same
                    //       primitive call when it gets unblocked, so we
                    //       shouldn't pop values off its stack.
                    actor.argument_stack.popNItems(primitive.arity);
                }

                switch (primitive_result) {
                    .ActorSwitch, .ActivationChange, .Success => return primitive_result,
                    .Completion => |completion| {
                        if (completion.isNormal()) {
                            vm.writeRegister(inst.target, completion.data.Normal);
                            return success;
                        }

                        return ExecutionResult.completion(completion);
                    },
                }
            }

            return ExecutionResult.completion(try Completion.initRuntimeError(vm, source_range, "Unknown primitive selector '{s}'", .{payload.message_name}));
        },
        .PushConstantSlot => |payload| {
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try actor.slot_stack.push(vm.allocator, Slot.initConstant(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
            return success;
        },
        .PushAssignableSlot => |payload| {
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try actor.slot_stack.push(vm.allocator, Slot.initAssignable(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
            return success;
        },
        .PushArgumentSlot => |payload| {
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();

            try actor.slot_stack.push(vm.allocator, Slot.initArgument(name_byte_array_object.getByteArray()));
            return success;
        },
        .PushInheritedSlot => |payload| {
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try actor.slot_stack.push(vm.allocator, Slot.initInherited(name_byte_array_object.getByteArray(), value));
            return success;
        },
        .CreateInteger => |payload| {
            vm.writeRegister(inst.target, Value.fromInteger(payload));
            return success;
        },
        .CreateFloatingPoint => |payload| {
            vm.writeRegister(inst.target, Value.fromFloatingPoint(payload));
            return success;
        },
        .CreateObject => |payload| {
            const slots = actor.slot_stack.lastNItems(payload.slot_count);
            var total_slot_count: u32 = 0;
            var total_assignable_slot_count: u8 = 0;
            for (slots) |slot, i| {
                total_slot_count += slot.requiredSlotSpace(slots[0..i]);
                total_assignable_slot_count += @intCast(u8, slot.requiredAssignableSlotValueSpace(slots[0..i]));
            }

            var token = try vm.heap.getAllocation(
                Object.Map.Slots.requiredSizeForAllocation(total_slot_count) +
                    Object.Slots.requiredSizeForAllocation(total_assignable_slot_count),
            );
            defer token.deinit();

            var slots_map = Object.Map.Slots.create(&token, total_slot_count);

            var map_builder = slots_map.getMapBuilder(&token);

            for (slots) |slot| {
                map_builder.addSlot(slot);
            }

            const slots_object = map_builder.createObject(actor.id);
            vm.writeRegister(inst.target, slots_object.asValue());
            actor.slot_stack.popNItems(payload.slot_count);
            return success;
        },
        .CreateMethod => |payload| {
            defer actor.next_method_is_inline = false;

            const slots = actor.slot_stack.lastNItems(payload.slot_count);
            var total_slot_count: u32 = 0;
            var total_assignable_slot_count: u8 = 0;
            var argument_slot_count: u8 = 0;
            for (slots) |slot, i| {
                total_slot_count += slot.requiredSlotSpace(slots[0..i]);
                total_assignable_slot_count += @intCast(u8, slot.requiredAssignableSlotValueSpace(slots[0..i]));

                // FIXME: This makes the assumption that argument slots are
                //        never overwritten.
                if (slot.isArgument())
                    argument_slot_count += 1;
            }

            var token = try vm.heap.getAllocation(
                Object.Map.Method.requiredSizeForAllocation(total_slot_count) +
                    Object.Method.requiredSizeForAllocation(total_assignable_slot_count),
            );
            defer token.deinit();

            const method_name_as_object = vm.readRegister(payload.method_name_location).asObject().asByteArrayObject();
            const block = executable.value.getBlock(payload.block_index);
            var method_map = try Object.Map.Method.create(
                &token,
                argument_slot_count,
                total_slot_count,
                actor.next_method_is_inline,
                method_name_as_object.getByteArray(),
                block,
                executable,
            );

            var map_builder = method_map.getMapBuilder(&token);

            for (slots) |slot| {
                map_builder.addSlot(slot);
            }

            const method_object = map_builder.createObject(actor.id);
            vm.writeRegister(inst.target, method_object.asValue());
            actor.slot_stack.popNItems(payload.slot_count);
            return success;
        },
        .CreateBlock => |payload| {
            const slots = actor.slot_stack.lastNItems(payload.slot_count);
            var total_slot_count: u32 = 0;
            var total_assignable_slot_count: u8 = 0;
            var argument_slot_count: u8 = 0;
            for (slots) |slot, i| {
                total_slot_count += slot.requiredSlotSpace(slots[0..i]);
                total_assignable_slot_count += @intCast(u8, slot.requiredAssignableSlotValueSpace(slots[0..i]));

                // FIXME: This makes the assumption that argument slots are
                //        never overwritten.
                if (slot.isArgument())
                    argument_slot_count += 1;
            }

            const block = executable.value.getBlock(payload.block_index);
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
                Object.Map.Block.requiredSizeForAllocation(total_slot_count) +
                    Object.Block.requiredSizeForAllocation(total_assignable_slot_count),
            );
            defer token.deinit();

            var block_map = try Object.Map.Block.create(
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

            const block_object = map_builder.createObject(actor.id);
            vm.writeRegister(inst.target, block_object.asValue());
            actor.slot_stack.popNItems(payload.slot_count);
            return success;
        },
        .CreateByteArray => |payload| {
            var token = try vm.heap.getAllocation(Object.ByteArray.requiredSizeForAllocation(payload.len));
            defer token.deinit();

            const byte_array = try Object.ByteArray.createWithValues(&token, actor.id, payload);
            vm.writeRegister(inst.target, byte_array.asValue());
            return success;
        },
        .SetMethodInline => {
            actor.next_method_is_inline = true;
            return success;
        },
        .Return => |payload| {
            const value = vm.readRegister(payload.value_location);
            vm.writeRegister(.ret, value);

            if (actor.exitCurrentActivation(vm, last_activation_ref) == .LastActivation)
                return ExecutionResult.completion(Completion.initNormal(vm.readRegister(.ret)));
            return activation_change;
        },
        .NonlocalReturn => |payload| {
            const value = vm.readRegister(payload.value_location);
            vm.writeRegister(.ret, value);

            const current_activation = actor.activation_stack.getCurrent();
            // FIXME: Better name
            const target_activation_ref = current_activation.nonlocal_return_target_activation.?;
            const target_activation = target_activation_ref.get(actor.activation_stack) orelse
                return ExecutionResult.completion(try Completion.initRuntimeError(vm, source_range, "Attempted to non-local return to non-existent activation", .{}));

            if (actor.exitActivation(vm, last_activation_ref, target_activation) == .LastActivation)
                return ExecutionResult.completion(Completion.initNormal(vm.readRegister(.ret)));
            return activation_change;
        },
        .PushArg => |payload| {
            var argument = vm.readRegister(payload.argument_location);
            if (argument.isObjectReference() and argument.asObject().isActivationObject()) {
                argument = argument.asObject().asActivationObject().findActivationReceiver();
            }
            try actor.argument_stack.push(vm.allocator, argument);

            return success;
        },
        .PushRegisters => |clobbered_registers| {
            var iterator = clobbered_registers.iterator(.{});
            while (iterator.next()) |clobbered_register| {
                // FIXME: Remove manual register number adjustment!
                const register = bytecode.RegisterLocation.fromInt(@intCast(u32, clobbered_register + 2));
                try actor.saved_register_stack.push(vm.allocator, .{ .register = register, .value = vm.readRegister(register) });
            }

            return success;
        },
        .SourceRange => |range| {
            actor.range = range;
            return success;
        },
        .PushArgumentSentinel => {
            try actor.argument_stack.pushSentinel(vm.allocator);
            return success;
        },
        .PushSlotSentinel => {
            try actor.slot_stack.pushSentinel(vm.allocator);
            return success;
        },
        .VerifyArgumentSentinel => {
            actor.argument_stack.verifySentinel();
            return success;
        },
        .VerifySlotSentinel => {
            actor.slot_stack.verifySentinel();
            return success;
        },
    }
}

/// Sends a message to the given receiver, returning the result as a normal
/// completion if it can be immediately resolved; if the message send must
/// create a new activation, pushes the activation onto the stack and returns
/// null. If the message send fails, then returns an error completion.
pub fn sendMessage(
    vm: *VirtualMachine,
    actor: *Actor,
    receiver: Value,
    message_name: []const u8,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !?Completion {
    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    // FIXME: Only activate this when the message looks like a block execution.
    {
        var block_receiver = receiver;
        if (block_receiver.isObjectReference() and block_receiver.asObject().isActivationObject()) {
            block_receiver = block_receiver.asObject().asActivationObject().findActivationReceiver();
        }

        if (block_receiver.isObjectReference() and block_receiver.asObject().isBlockObject()) {
            const receiver_as_block = block_receiver.asObject().asBlockObject();
            if (receiver_as_block.isCorrectMessageForBlockExecution(message_name)) {
                const argument_count = receiver_as_block.getArgumentSlotCount();
                const argument_slice = actor.argument_stack.lastNItems(argument_count);

                // Advance the instruction for the activation that will be returned to.
                actor.activation_stack.getCurrent().advanceInstruction();

                try executeBlock(vm, actor, receiver_as_block, argument_slice, target_location, source_range);

                actor.argument_stack.popNItems(argument_count);
                // Bump the argument stack height of the (now current)
                // activation since we've now popped this activation's items off
                // it.
                actor.activation_stack.getCurrent().stack_snapshot.bumpArgumentHeight(actor);
                return null;
            }
        }
    }

    actor.ensureCanRead(receiver, source_range);

    return switch (receiver.lookup(vm, message_name)) {
        .Regular => |lookup_result| {
            if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
                const method_object = lookup_result.asObject().asMethodObject();
                const argument_count = method_object.getArgumentSlotCount();
                const argument_slice = actor.argument_stack.lastNItems(argument_count);

                // Advance the instruction for the activation that will be returned to.
                actor.activation_stack.getCurrent().advanceInstruction();

                try executeMethod(vm, actor, receiver, method_object, argument_slice, target_location, source_range);

                actor.argument_stack.popNItems(argument_count);
                // Bump the argument stack height of the (now current)
                // activation since we've now popped this activation's items off
                // it.
                actor.activation_stack.getCurrent().stack_snapshot.bumpArgumentHeight(actor);
                return null;
            } else {
                return Completion.initNormal(lookup_result);
            }
        },
        .Assignment => |assignment_context| {
            const argument_slice = actor.argument_stack.lastNItems(1);
            var argument = argument_slice[0];
            // NOTE: This is required, for instance, when we are assigning `self` to
            //       a slot (happens more often than you might think!). We need to strip
            //       the activation object to get to the actual value inside.
            if (argument.isObjectReference() and argument.asObject().isActivationObject()) {
                argument = argument.asObject().asActivationObject().findActivationReceiver();
            }

            const object_that_has_the_assignable_slot = assignment_context.object;
            const value_ptr = assignment_context.value_ptr;

            if (!actor.canWriteTo(object_that_has_the_assignable_slot.asValue())) {
                return try Completion.initRuntimeError(vm, source_range, "Assignment target is not writable for actor", .{});
            }

            if (object_that_has_the_assignable_slot.header.isGloballyReachable()) {
                // Mark every object that's not globally reachable in the
                // argument's object graph as globally reachable. This will
                // make the whole object graph part of the global object
                // hierarchy.
                _ = traversal.traverseNonGloballyReachableObjectGraph(argument, {}, struct {
                    fn f(context: void, object: Object) error{}!Object {
                        _ = context;
                        object.header.setGloballyReachable(true);
                        return object;
                    }
                }.f) catch unreachable;
            }

            value_ptr.* = argument;

            // David will remember that.
            try vm.heap.rememberObjectReference(object_that_has_the_assignable_slot.asValue(), argument);

            actor.argument_stack.popNItems(1);
            return Completion.initNormal(receiver);
        },
        .ActorMessage => |actor_message| {
            const method_object = actor_message.method;
            const argument_count = method_object.getArgumentSlotCount();
            const argument_slice = actor.argument_stack.lastNItems(argument_count);

            // FIXME: Figure out a way to avoid copying to an owned slice here.
            //        This is required for the time being because we don't have
            //        a better first-in-last-out (which is how messages are
            //        processed) structure yet.
            const copied_arguments_slice = try vm.allocator.dupe(Value, argument_slice);
            errdefer vm.allocator.free(copied_arguments_slice);

            try actor_message.target_actor.getActor().putMessageInMailbox(
                vm.allocator,
                actor.actor_object.get(),
                method_object,
                copied_arguments_slice,
                source_range,
            );

            actor.argument_stack.popNItems(argument_count);
            return Completion.initNormal(vm.nil());
        },
        .Nothing => try Completion.initRuntimeError(vm, source_range, "Unknown selector '{s}'", .{message_name}),
    };
}

fn executeBlock(
    vm: *VirtualMachine,
    actor: *Actor,
    block_receiver: *Object.Block,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    const tracked_block = try vm.heap.track(block_receiver.asValue());
    var block = block_receiver;

    const message_name = try vm.getOrCreateBlockMessageName(@intCast(u8, arguments.len));
    var token = token: {
        defer tracked_block.untrack(vm.heap);

        var required_memory = Object.Activation.requiredSizeForAllocation(
            block.getArgumentSlotCount(),
            block.getAssignableSlotCount(),
        );
        if (!message_name.exists) required_memory += message_name.requiredSize();

        // Ensure that we won't GC by creating an activation.
        var token = try vm.heap.getAllocation(required_memory);

        // Refresh the pointer to the block.
        block = tracked_block.getValue().asObject().asBlockObject();

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
    method_object: *Object.Method,
    arguments: []const Value,
    target_location: bytecode.RegisterLocation,
    source_range: SourceRange,
) !void {
    const tracked_receiver = try vm.heap.track(const_receiver);
    const tracked_method = try vm.heap.track(method_object.asValue());
    var receiver_of_method = const_receiver;
    var method = method_object;

    var token = token: {
        defer tracked_receiver.untrack(vm.heap);
        defer tracked_method.untrack(vm.heap);

        // Get the allocation token for the method
        var token = try vm.heap.getAllocation(
            Object.Activation.requiredSizeForAllocation(
                method.getArgumentSlotCount(),
                method.getAssignableSlotCount(),
            ),
        );

        // Refresh the pointers to the method and its receiver.
        receiver_of_method = tracked_receiver.getValue();
        method = tracked_method.getValue().asObject().asMethodObject();

        break :token token;
    };
    defer token.deinit();

    // NOTE: The receiver of a method activation must never be an activation
    //       object (unless it explicitly wants that), as that would allow
    //       us to access the slots of upper scopes.
    if (!method.expectsActivationObjectAsReceiver() and
        receiver_of_method.isObjectReference() and
        receiver_of_method.asObject().isActivationObject())
    {
        receiver_of_method = receiver_of_method.asObject().asActivationObject().findActivationReceiver();
    }

    const activation_slot = try actor.activation_stack.getNewActivationSlot(vm.allocator);
    method.activateMethod(vm, &token, actor.id, receiver_of_method, arguments, target_location, source_range, activation_slot);
}
