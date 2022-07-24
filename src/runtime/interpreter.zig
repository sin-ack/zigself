// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Slot = slot_import.Slot;
const debug = @import("../debug.zig");
const Actor = @import("./Actor.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./Object.zig");
const ByteArray = @import("./ByteArray.zig");
const Activation = @import("./Activation.zig");
const Completion = @import("./Completion.zig");
const Executable = @import("./lowcode/Executable.zig");
const primitives = @import("./primitives.zig");
const Instruction = @import("./lowcode/Instruction.zig");
const SourceRange = @import("./SourceRange.zig");
const slot_import = @import("./slot.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const RegisterLocation = @import("./lowcode/register_location.zig").RegisterLocation;

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
pub fn execute(vm: *VirtualMachine, actor: *Actor, last_activation_ref: ?Activation.ActivationRef, executable: Executable.Ref, inst: Instruction) !ExecutionResult {
    const source_range = SourceRange.initNoRef(executable, actor.range);

    if (EXECUTION_DEBUG) std.debug.print("[{*} {s}] Executing: {} = {}\n", .{ actor, executable.value.definition_script.value.file_path, inst.target, inst });

    switch (inst.tag) {
        .Send => {
            const payload = inst.payload(.Send);
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
        .PrimSend => {
            const payload = inst.payload(.PrimSend);

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
        .SelfSend => {
            const payload = inst.payload(.SelfSend);
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
        .SelfPrimSend => {
            const payload = inst.payload(.SelfPrimSend);

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
        .PushConstantSlot => {
            const payload = inst.payload(.PushConstantSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try actor.slot_stack.push(vm.allocator, Slot.initConstant(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
            return success;
        },
        .PushAssignableSlot => {
            const payload = inst.payload(.PushAssignableSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try actor.slot_stack.push(vm.allocator, Slot.initAssignable(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
            return success;
        },
        .PushArgumentSlot => {
            const payload = inst.payload(.PushArgumentSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();

            try actor.slot_stack.push(vm.allocator, Slot.initArgument(name_byte_array_object.getByteArray()));
            return success;
        },
        .PushInheritedSlot => {
            const payload = inst.payload(.PushInheritedSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try actor.slot_stack.push(vm.allocator, Slot.initInherited(name_byte_array_object.getByteArray(), value));
            return success;
        },
        .CreateInteger => {
            const payload = inst.payload(.CreateInteger);
            vm.writeRegister(inst.target, Value.fromInteger(payload.value));
            return success;
        },
        .CreateFloatingPoint => {
            const payload = inst.payload(.CreateFloatingPoint);
            vm.writeRegister(inst.target, Value.fromFloatingPoint(payload.value));
            return success;
        },
        .CreateObject => {
            const payload = inst.payload(.CreateObject);

            const slots = actor.slot_stack.lastNItems(payload.slot_count);
            var total_slot_count: u32 = 0;
            for (slots) |slot, i| {
                total_slot_count += slot.requiredSlotSpace(slots[0..i]);
            }

            var slots_map = try Object.Map.Slots.create(vm.heap, total_slot_count);

            var map_builder = try slots_map.getMapBuilder(vm.heap);
            defer map_builder.deinit();

            for (slots) |slot| {
                try map_builder.addSlot(slot);
            }

            const slots_object = try map_builder.createObject(actor.id);
            vm.writeRegister(inst.target, slots_object.asValue());
            actor.slot_stack.popNItems(payload.slot_count);
            return success;
        },
        .CreateMethod => {
            const payload = inst.payload(.CreateMethod);

            defer actor.next_method_is_inline = false;

            const slots = actor.slot_stack.lastNItems(payload.slot_count);
            var total_slot_count: u32 = 0;
            var argument_slot_count: u8 = 0;
            for (slots) |slot, i| {
                total_slot_count += slot.requiredSlotSpace(slots[0..i]);

                // FIXME: This makes the assumption that argument slots are
                //        never overwritten.
                if (slot.isArgument())
                    argument_slot_count += 1;
            }

            const method_name_as_object = vm.readRegister(payload.method_name_location).asObject().asByteArrayObject();
            const block = executable.value.getBlock(payload.block_index);
            var method_map = try Object.Map.Method.create(
                vm.heap,
                argument_slot_count,
                total_slot_count,
                actor.next_method_is_inline,
                method_name_as_object.getByteArray(),
                block,
                executable,
            );

            var map_builder = try method_map.getMapBuilder(vm.heap);
            defer map_builder.deinit();

            for (slots) |slot| {
                try map_builder.addSlot(slot);
            }

            const method_object = try map_builder.createObject(actor.id);
            vm.writeRegister(inst.target, method_object.asValue());
            actor.slot_stack.popNItems(payload.slot_count);
            return success;
        },
        .CreateBlock => {
            const payload = inst.payload(.CreateBlock);

            const slots = actor.slot_stack.lastNItems(payload.slot_count);
            var total_slot_count: u32 = 0;
            var argument_slot_count: u8 = 0;
            for (slots) |slot, i| {
                total_slot_count += slot.requiredSlotSpace(slots[0..i]);

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

            var block_map = try Object.Map.Block.create(
                vm.heap,
                argument_slot_count,
                total_slot_count,
                parent_activation.takeRef(actor.activation_stack),
                nonlocal_return_target_activation,
                block,
                executable,
            );

            var map_builder = try block_map.getMapBuilder(vm.heap);
            defer map_builder.deinit();

            for (slots) |slot| {
                try map_builder.addSlot(slot);
            }

            const block_object = try map_builder.createObject(actor.id);
            vm.writeRegister(inst.target, block_object.asValue());
            actor.slot_stack.popNItems(payload.slot_count);
            return success;
        },
        .CreateByteArray => {
            const payload = inst.payload(.CreateByteArray);
            const string = payload.string;

            try vm.heap.ensureSpaceInEden(Object.ByteArray.requiredSizeForAllocation(string.len));

            const byte_array = try Object.ByteArray.createWithValues(vm.heap, actor.id, string);
            vm.writeRegister(inst.target, byte_array.asValue());
            return success;
        },
        .SetMethodInline => {
            actor.next_method_is_inline = true;
            return success;
        },
        .Return => {
            if (actor.exitCurrentActivation(vm, last_activation_ref) == .LastActivation)
                return ExecutionResult.completion(Completion.initNormal(vm.readRegister(.ret)));
            return activation_change;
        },
        .NonlocalReturn => {
            const current_activation = actor.activation_stack.getCurrent();
            // FIXME: Better name
            const target_activation_ref = current_activation.nonlocal_return_target_activation.?;
            const target_activation = target_activation_ref.get(actor.activation_stack) orelse
                return ExecutionResult.completion(try Completion.initRuntimeError(vm, source_range, "Attempted to non-local return to non-existent activation", .{}));

            if (actor.exitActivation(vm, last_activation_ref, target_activation) == .LastActivation)
                return ExecutionResult.completion(Completion.initNormal(vm.readRegister(.ret)));
            return activation_change;
        },
        .WriteReturnValue => {
            const payload = inst.payload(.WriteReturnValue);
            const value = vm.readRegister(payload.value_location);
            vm.writeRegister(.ret, value);
            return success;
        },
        .PushArg => {
            const payload = inst.payload(.PushArg);

            var argument = vm.readRegister(payload.argument_location);
            if (argument.isObjectReference() and argument.asObject().isActivationObject()) {
                argument = argument.asObject().asActivationObject().findActivationReceiver();
            }
            try actor.argument_stack.push(vm.allocator, argument);

            return success;
        },
        .PushRegisters => {
            const payload = inst.payload(.PushRegisters);

            var iterator = payload.clobbered_registers.iterator(.{});
            while (iterator.next()) |clobbered_register| {
                // FIXME: Remove manual register number adjustment!
                const register = RegisterLocation.fromInt(@intCast(u32, clobbered_register + 2));
                try actor.saved_register_stack.push(vm.allocator, .{ .register = register, .value = vm.readRegister(register) });
            }

            return success;
        },
        .SourceRange => {
            const range = inst.payload(.SourceRange);
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
    target_location: RegisterLocation,
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
    target_location: RegisterLocation,
    source_range: SourceRange,
) !void {
    const tracked_block = try vm.heap.track(block_receiver.asValue());
    var block = block_receiver;

    {
        defer tracked_block.untrack(vm.heap);

        // Ensure that we won't GC by creating an activation.
        try vm.heap.ensureSpaceInEden(
            Object.Activation.requiredSizeForAllocation(
                block.getArgumentSlotCount(),
                block.getAssignableSlotCount(),
            ),
        );

        // Refresh the pointer to the block.
        block = tracked_block.getValue().asObject().asBlockObject();
    }

    const parent_activation_object = block.getMap().parent_activation.get(actor.activation_stack).?.activation_object;
    const activation_slot = try actor.activation_stack.getNewActivationSlot(vm.allocator);
    const tracked_message_name = try vm.getOrCreateBlockMessageName(@intCast(u8, arguments.len));
    try block.activateBlock(
        vm,
        parent_activation_object.value,
        arguments,
        target_location,
        tracked_message_name.getValue(),
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
    target_location: RegisterLocation,
    source_range: SourceRange,
) !void {
    const tracked_receiver = try vm.heap.track(const_receiver);
    const tracked_method = try vm.heap.track(method_object.asValue());
    var method = method_object;

    {
        defer tracked_receiver.untrack(vm.heap);
        defer tracked_method.untrack(vm.heap);

        // Ensure that we won't GC by creating an activation.
        try vm.heap.ensureSpaceInEden(
            Object.Activation.requiredSizeForAllocation(
                method.getArgumentSlotCount(),
                method.getAssignableSlotCount(),
            ),
        );

        // Refresh the pointers to the method and its receiver.
        method = tracked_method.getValue().asObject().asMethodObject();
    }

    // NOTE: The receiver of a method activation must never be an activation
    //       object (unless it explicitly wants that), as that would allow
    //       us to access the slots of upper scopes.
    var receiver_of_method = tracked_receiver.getValue();
    if (!method.expectsActivationObjectAsReceiver() and
        receiver_of_method.isObjectReference() and
        receiver_of_method.asObject().isActivationObject())
    {
        receiver_of_method = receiver_of_method.asObject().asActivationObject().findActivationReceiver();
    }

    const activation_slot = try actor.activation_stack.getNewActivationSlot(vm.allocator);
    try method.activateMethod(vm, actor.id, receiver_of_method, arguments, target_location, source_range, activation_slot);
}
