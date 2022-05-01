// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const debug = @import("../debug.zig");
const Actor = @import("./Actor.zig");
const Value = @import("./value.zig").Value;
const Opcode = @import("./bytecode/Opcode.zig");
const Object = @import("./Object.zig");
const ByteArray = @import("./ByteArray.zig");
const Activation = @import("./Activation.zig");
const Completion = @import("./Completion.zig");
const Executable = @import("./bytecode/Executable.zig");
const primitives = @import("./primitives.zig");
const SourceRange = @import("./SourceRange.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const RegisterLocation = @import("./bytecode/register_location.zig").RegisterLocation;
const slot_import = @import("./slot.zig");
const Slot = slot_import.Slot;

const EXECUTION_DEBUG = debug.EXECUTION_DEBUG;

pub fn execute(vm: *VirtualMachine, actor: *Actor, last_activation: ?*Activation, executable: Executable.Ref, opcode: Opcode) !?Completion {
    var dummy_source_range = SourceRange.init(executable, .{
        .start = .{ .line_start = 0, .line_end = 2, .line = 1, .column = 1 },
        .end = .{ .line_start = 0, .line_end = 2, .line = 1, .column = 2 },
    });
    defer dummy_source_range.deinit();

    if (EXECUTION_DEBUG) std.debug.print("[{s}] Executing: {} = {}\n", .{ executable.value.definition_script.value.file_path, opcode.target, opcode });

    switch (opcode.tag) {
        .Send => {
            const payload = opcode.payload(.Send);
            const receiver = vm.readRegister(payload.receiver_location);

            const completion = (try sendMessage(vm, actor, receiver, payload.message_name, opcode.target, dummy_source_range)) orelse return null;

            if (completion.isNormal()) {
                var result = completion.data.Normal;
                if (result.isObjectReference() and result.asObject().isActivationObject()) {
                    result = result.asObject().asActivationObject().findActivationReceiver();
                }

                vm.writeRegister(opcode.target, result);
                return null;
            }

            return completion;
        },
        .PrimSend => {
            const payload = opcode.payload(.PrimSend);

            if (primitives.getPrimitive(payload.message_name)) |primitive| {
                var receiver = vm.readRegister(payload.receiver_location);
                if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
                    receiver = receiver.asObject().asActivationObject().findActivationReceiver();
                }

                const tracked_receiver = try vm.heap.track(receiver);
                defer tracked_receiver.untrack(vm.heap);

                const argument_slice = vm.lastNArguments(primitive.arity);
                const completion = (try primitive.call(vm, actor, tracked_receiver, argument_slice, opcode.target, dummy_source_range)) orelse return null;
                vm.popNArguments(primitive.arity);

                if (completion.isNormal()) {
                    vm.writeRegister(opcode.target, completion.data.Normal);
                    return null;
                }

                return completion;
            }

            return try Completion.initRuntimeError(vm, dummy_source_range, "Unknown primitive selector '{s}'", .{payload.message_name});
        },
        .SelfSend => {
            const payload = opcode.payload(.SelfSend);
            const receiver = actor.activation_stack.getCurrent().activation_object;

            const completion = (try sendMessage(vm, actor, receiver, payload.message_name, opcode.target, dummy_source_range)) orelse return null;

            if (completion.isNormal()) {
                var result = completion.data.Normal;
                if (result.isObjectReference() and result.asObject().isActivationObject()) {
                    result = result.asObject().asActivationObject().findActivationReceiver();
                }

                vm.writeRegister(opcode.target, result);
                return null;
            }

            return completion;
        },
        .SelfPrimSend => {
            const payload = opcode.payload(.SelfPrimSend);

            if (primitives.getPrimitive(payload.message_name)) |primitive| {
                var receiver = actor.activation_stack.getCurrent().activation_object;
                if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
                    receiver = receiver.asObject().asActivationObject().findActivationReceiver();
                }

                const tracked_receiver = try vm.heap.track(receiver);
                defer tracked_receiver.untrack(vm.heap);

                const argument_slice = vm.lastNArguments(primitive.arity);
                const completion = (try primitive.call(vm, actor, tracked_receiver, argument_slice, opcode.target, dummy_source_range)) orelse return null;
                vm.popNArguments(primitive.arity);

                if (completion.isNormal()) {
                    vm.writeRegister(opcode.target, completion.data.Normal);
                    return null;
                }

                return completion;
            }

            return try Completion.initRuntimeError(vm, dummy_source_range, "Unknown primitive selector '{s}'", .{payload.message_name});
        },
        .PushConstantSlot => {
            const payload = opcode.payload(.PushConstantSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try vm.pushSlot(Slot.initConstant(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
            return null;
        },
        .PushAssignableSlot => {
            const payload = opcode.payload(.PushAssignableSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try vm.pushSlot(Slot.initAssignable(name_byte_array_object.getByteArray(), if (payload.is_parent) .Parent else .NotParent, value));
            return null;
        },
        .PushArgumentSlot => {
            const payload = opcode.payload(.PushArgumentSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();

            try vm.pushSlot(Slot.initArgument(name_byte_array_object.getByteArray()));
            return null;
        },
        .PushInheritedSlot => {
            const payload = opcode.payload(.PushInheritedSlot);
            const name_value = vm.readRegister(payload.name_location);
            const name_byte_array_object = name_value.asObject().asByteArrayObject();
            const value = vm.readRegister(payload.value_location);

            try vm.pushSlot(Slot.initInherited(name_byte_array_object.getByteArray(), value));
            return null;
        },
        .CreateInteger => {
            const payload = opcode.payload(.CreateInteger);
            vm.writeRegister(opcode.target, Value.fromInteger(payload.value));
            return null;
        },
        .CreateFloatingPoint => {
            const payload = opcode.payload(.CreateFloatingPoint);
            vm.writeRegister(opcode.target, Value.fromFloatingPoint(payload.value));
            return null;
        },
        .CreateObject => {
            const payload = opcode.payload(.CreateObject);

            const slots = vm.lastNSlots(payload.slot_count);
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

            const slots_object = try map_builder.createObject();
            vm.writeRegister(opcode.target, slots_object.asValue());
            vm.popNSlots(payload.slot_count);
            return null;
        },
        .CreateMethod => {
            const payload = opcode.payload(.CreateMethod);

            defer vm.next_method_is_inline = false;

            const slots = vm.lastNSlots(payload.slot_count);
            var total_slot_count: u32 = 0;
            var argument_slot_count: u8 = 0;
            for (slots) |slot, i| {
                total_slot_count += slot.requiredSlotSpace(slots[0..i]);

                // FIXME: This makes the assumption that argument slots are
                //        never overwritten.
                if (slot.isArgument())
                    argument_slot_count += 1;
            }

            const method_name_as_object = vm.readRegister(payload.method_name).asObject().asByteArrayObject();
            const block = executable.value.getBlock(payload.block_index);
            // FIXME: Support inline methods
            var method_map = try Object.Map.Method.create(
                vm.heap,
                argument_slot_count,
                total_slot_count,
                vm.next_method_is_inline,
                method_name_as_object.getByteArray(),
                block,
                executable,
            );

            var map_builder = try method_map.getMapBuilder(vm.heap);
            defer map_builder.deinit();

            for (slots) |slot| {
                try map_builder.addSlot(slot);
            }

            const method_object = try map_builder.createObject();
            vm.writeRegister(opcode.target, method_object.asValue());
            vm.popNSlots(payload.slot_count);
            return null;
        },
        .CreateBlock => {
            const payload = opcode.payload(.CreateBlock);

            const slots = vm.lastNSlots(payload.slot_count);
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
            const nonlocal_return_target_activation = if (parent_activation.nonlocal_return_target_activation) |target| target else parent_activation;
            std.debug.assert(nonlocal_return_target_activation.nonlocal_return_target_activation == null);

            var block_map = try Object.Map.Block.create(
                vm.heap,
                argument_slot_count,
                total_slot_count,
                parent_activation,
                nonlocal_return_target_activation,
                block,
                executable,
            );

            var map_builder = try block_map.getMapBuilder(vm.heap);
            defer map_builder.deinit();

            for (slots) |slot| {
                try map_builder.addSlot(slot);
            }

            const block_object = try map_builder.createObject();
            vm.writeRegister(opcode.target, block_object.asValue());
            vm.popNSlots(payload.slot_count);
            return null;
        },
        .CreateByteArray => {
            const payload = opcode.payload(.CreateByteArray);
            const string = payload.string;

            try vm.heap.ensureSpaceInEden(
                ByteArray.requiredSizeForAllocation(string.len) +
                    Object.Map.ByteArray.requiredSizeForAllocation() +
                    Object.ByteArray.requiredSizeForAllocation(),
            );

            const byte_array = try ByteArray.createFromString(vm.heap, string);
            const byte_array_map = try Object.Map.ByteArray.create(vm.heap, byte_array);
            const byte_array_object = try Object.ByteArray.create(vm.heap, byte_array_map);

            vm.writeRegister(opcode.target, byte_array_object.asValue());
            return null;
        },
        .SetMethodInline => {
            vm.next_method_is_inline = true;
            return null;
        },
        .ExitActivation => {
            const payload = opcode.payload(.ExitActivation);
            const value = vm.readRegister(payload.value_location);

            if (actor.exitCurrentActivation(vm, last_activation, value) == .LastActivation)
                return Completion.initNormal(value);
            return null;
        },
        .NonlocalReturn => {
            const payload = opcode.payload(.NonlocalReturn);
            const value = vm.readRegister(payload.value_location);

            const current_activation = actor.activation_stack.getCurrent();
            // FIXME: Better name
            const target_activation = current_activation.nonlocal_return_target_activation.?;

            if (actor.exitActivation(vm, last_activation, target_activation, value) == .LastActivation)
                return Completion.initNormal(value);
            return null;
        },
        .PushArg => {
            const payload = opcode.payload(.PushArg);

            var argument = vm.readRegister(payload.argument_location);
            if (argument.isObjectReference() and argument.asObject().isActivationObject()) {
                argument = argument.asObject().asActivationObject().findActivationReceiver();
            }
            try vm.pushArgument(argument);

            return null;
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
                const argument_slice = vm.lastNArguments(argument_count);

                try executeBlock(vm, actor, receiver_as_block, argument_slice, target_location, source_range);

                vm.popNArguments(argument_count);
                return null;
            }
        }
    }

    return switch (try receiver.lookup(vm, message_name)) {
        .Regular => |lookup_result| {
            if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
                const method_object = lookup_result.asObject().asMethodObject();
                const argument_count = method_object.getArgumentSlotCount();
                const argument_slice = vm.lastNArguments(argument_count);

                try executeMethod(vm, actor, receiver, method_object, argument_slice, target_location, source_range);

                vm.popNArguments(argument_count);
                return null;
            } else {
                return Completion.initNormal(lookup_result);
            }
        },
        .Assignment => |assignment_context| {
            const argument_slice = vm.lastNArguments(1);
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

            vm.popNArguments(1);
            return Completion.initNormal(receiver);
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

    const parent_activation_object = block.getMap().parent_activation.get(&actor.activation_stack).?.activation_object;
    const activation_slot = actor.activation_stack.getNewActivationSlot();
    const tracked_message_name = try vm.getOrCreateBlockMessageName(@intCast(u8, arguments.len));
    try block.activateBlock(
        vm,
        actor,
        parent_activation_object,
        arguments,
        target_location,
        tracked_message_name.getValue(),
        source_range,
        activation_slot,
    );
    actor.activation_stack.setRegisters(vm);
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

    const activation_slot = actor.activation_stack.getNewActivationSlot();
    try method.activateMethod(vm, receiver_of_method, arguments, target_location, source_range, activation_slot);
    actor.activation_stack.setRegisters(vm);
}
