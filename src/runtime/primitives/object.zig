// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const tracy = @import("tracy");
const Allocator = std.mem.Allocator;

const SlotsObject = @import("../objects/slots.zig").Slots;
const RuntimeError = @import("../RuntimeError.zig");
const value_inspector = @import("../value_inspector.zig");
const ExecutionResult = @import("../execution_result.zig").ExecutionResult;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Adds the slots in the argument object to the receiver object. The slots
/// are copied. The objects at each slot are not cloned, however.
pub fn AddSlots(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const arguments = context.getArguments("_AddSlots:");
    var receiver = try arguments.getObject(PrimitiveContext.Receiver, .Slots);
    var argument = try arguments.getObject(0, .Slots);

    if (!context.actor.canWriteTo(context.receiver.get())) {
        return ExecutionResult.runtimeError(
            RuntimeError.initLiteral(
                context.source_range,
                "_AddSlots: receiver is not writable for actor",
            ),
        );
    }

    var token = try context.vm.heap.allocate(
        try SlotsObject.requiredSizeForMerging(receiver, argument, context.vm.allocator),
    );
    defer token.deinit();

    // Refresh the pointers in case that caused a GC
    receiver = context.receiver.get().asObject().?.asType(.Slots).?;
    argument = context.arguments[0].asObject().?.asType(.Slots).?;

    const new_object = try receiver.addSlotsFrom(context.vm.allocator, &context.vm.heap, &token, argument);
    return ExecutionResult.resolve(new_object.asValue());
}

// FIXME: Re-enable this.
/// Removes the given slot. If the slot isn't found or otherwise cannot be
/// removed, the second argument is evaluated as a block.
pub fn RemoveSlot_IfFail(context: *PrimitiveContext) !ExecutionResult {
    var receiver = context.receiver.get();
    var slot_name = context.arguments[0].get();
    var fail_block = context.arguments[1].get();

    if (!slot_name.value.is(.ByteVector)) {
        return ExecutionResult.runtimeError(try RuntimeError.initFormatted(
            context.vm.allocator,
            context.source_range,
            "Expected ByteVector for the slot name argument of _RemoveSlot:IfFail:, got {s}",
            .{@tagName(slot_name.value.content)},
        ));
    }

    if (!fail_block.value.is(.Block)) {
        return ExecutionResult.runtimeError(try RuntimeError.initFormatted(
            context.vm.allocator,
            context.source_range,
            "Expected Block for the failure block argument of _RemoveSlot:IfFail:, got {s}",
            .{@tagName(fail_block.value.content)},
        ));
    }

    const did_remove_slot = receiver.value.removeSlot(context.vm.allocator, slot_name.value.content.ByteVector.values) catch |err| switch (err) {
        error.ObjectDoesNotAcceptSlots => {
            return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                context.source_range,
                "Attempted to remove a slot from an object which does not accept slots",
            ));
        },
        else => return @as(Allocator.Error, @errorCast(err)),
    };

    if (!did_remove_slot) {
        // const returned_value = try message_interpreter.executeBlockMessage(fail_block, &.{}, context.source_range, context.interpreter_context);
        // returned_value.unrefWithAllocator(context.vm.allocator);
    }

    return ExecutionResult.resolve(context.vm.nil());
}

/// Inspect the receiver and print it to stderr. Return the receiver.
pub fn Inspect(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const receiver = context.receiver.get();
    try value_inspector.inspectValue(.Multiline, context.vm, receiver);
    return ExecutionResult.resolve(receiver);
}

/// Make an identical shallow copy of the receiver and return it.
pub fn Clone(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var receiver = context.receiver.get();

    const required_memory = if (receiver.asObject()) |object|
        object.getSizeForCloning()
    else
        0;

    var token = try context.vm.heap.allocate(required_memory);
    defer token.deinit();

    receiver = context.receiver.get();

    return ExecutionResult.resolve(receiver.clone(context.vm.allocator, &context.vm.heap, &token, context.actor.id));
}

/// Return whether the receiver and argument are identical. Returns either
/// the global "true" or "false" object.
pub fn Eq(context: *PrimitiveContext) error{}!ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    return ExecutionResult.resolve(
        if (context.receiver.get().data == context.arguments[0].data)
            context.vm.getTrue()
        else
            context.vm.getFalse(),
    );
}
