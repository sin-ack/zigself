// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");
const Completion = @import("../completion.zig");
const environment = @import("../environment.zig");
const value_inspector = @import("../value_inspector.zig");
const message_interpreter = @import("../interpreter/message.zig");

const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Adds the slots in the argument object to the receiver object. The slots
/// are copied. The objects at each slot are not cloned, however.
pub fn AddSlots(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const argument = context.arguments[0].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isSlotsObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected Slots as the receiver to _AddSlots:", .{});
    }

    if (!(argument.isObjectReference() and argument.asObject().isSlotsObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected Slots as the argument to _AddSlots:", .{});
    }

    var receiver_object = receiver.asObject().asSlotsObject();
    var argument_object = argument.asObject().asSlotsObject();

    // Avoid any further GCs by reserving the space beforehand
    try context.interpreter_context.heap.ensureSpaceInEden(Object.Slots.requiredSizeForMerging(receiver_object, argument_object));

    // Refresh the pointers in case that caused a GC
    receiver_object = context.receiver.getValue().asObject().asSlotsObject();
    argument_object = context.arguments[0].getValue().asObject().asSlotsObject();

    const new_object = try receiver_object.addSlotsFrom(argument_object, context.interpreter_context.heap, context.interpreter_context.allocator);
    return Completion.initNormal(new_object.asValue());
}

/// Removes the given slot. If the slot isn't found or otherwise cannot be
/// removed, the second argument is evaluated as a block.
pub fn RemoveSlot_IfFail(context: PrimitiveContext) !Completion {
    var receiver = context.receiver.getValue();
    var slot_name = context.arguments[0].getValue();
    var fail_block = context.arguments[1].getValue();

    if (!slot_name.value.is(.ByteVector)) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected ByteVector for the slot name argument of _RemoveSlot:IfFail:, got {s}",
            .{@tagName(slot_name.value.content)},
        );
    }

    if (!fail_block.value.is(.Block)) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected Block for the failure block argument of _RemoveSlot:IfFail:, got {s}",
            .{@tagName(fail_block.value.content)},
        );
    }

    const did_remove_slot = receiver.value.removeSlot(context.interpreter_context.allocator, slot_name.value.content.ByteVector.values) catch |err| switch (err) {
        error.ObjectDoesNotAcceptSlots => {
            return Completion.initRuntimeError(
                context.interpreter_context.allocator,
                context.source_range,
                "Attempted to remove a slot from an object which does not accept slots",
                .{},
            );
        },
        else => return @errSetCast(Allocator.Error, err),
    };

    if (!did_remove_slot) {
        const returned_value = try message_interpreter.executeBlockMessage(fail_block, &.{}, context.source_range, context.interpreter_context);
        returned_value.unrefWithAllocator(context.interpreter_context.allocator);
    }

    return Completion.initNormal(environment.globalNil());
}

/// Inspect the receiver and print it to stderr. Return the receiver.
pub fn Inspect(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    try value_inspector.inspectValue(.Multiline, receiver);
    return Completion.initNormal(receiver);
}

/// Make an identical shallow copy of the receiver and return it.
pub fn Clone(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    return Completion.initNormal(try receiver.clone(context.interpreter_context.heap));
}

/// Return whether the receiver and argument are identical. Returns either
/// the global "true" or "false" object.
pub fn Eq(context: PrimitiveContext) error{}!Completion {
    return Completion.initNormal(
        if (context.receiver.getValue().data == context.arguments[0].getValue().data)
            environment.globalTrue()
        else
            environment.globalFalse(),
    );
}
