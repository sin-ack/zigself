// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("../slot.zig");
const Heap = @import("../heap.zig");
const Range = @import("../../language/location_range.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Completion = @import("../completion.zig");
const environment = @import("../environment.zig");
const object_inspector = @import("../object_inspector.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;
const message_interpreter = @import("../interpreter/message.zig");

/// Adds the slots in the argument object to the receiver object. The slots
/// are copied. The objects at each slot are not cloned, however.
pub fn AddSlots(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Completion {
    _ = message_range;
    _ = context;

    const receiver = tracked_receiver.getValue();
    const argument = arguments[0].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isSlotsObject())) {
        return Completion.initRuntimeError(allocator, "Expected Slots as the receiver to _AddSlots:", .{});
    }

    if (!(argument.isObjectReference() and argument.asObject().isSlotsObject())) {
        return Completion.initRuntimeError(allocator, "Expected Slots as the argument to _AddSlots:", .{});
    }

    var receiver_object = receiver.asObject().asSlotsObject();
    var argument_object = receiver.asObject().asSlotsObject();

    // Avoid any further GCs by reserving the space beforehand
    try heap.ensureSpaceInEden(Object.Slots.requiredSizeForMerging(receiver_object, argument_object));

    // Refresh the pointers in case that caused a GC
    receiver_object = tracked_receiver.getValue().asObject().asSlotsObject();
    argument_object = arguments[0].getValue().asObject().asSlotsObject();

    const new_object = try receiver_object.addSlotsFrom(argument_object, heap, allocator);
    return Completion.initNormal(new_object.asValue());
}

/// Removes the given slot. If the slot isn't found or otherwise cannot be
/// removed, the second argument is evaluated as a block.
pub fn RemoveSlot_IfFail(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Completion {
    var receiver = tracked_receiver.getValue();
    var slot_name = arguments[0].getValue();
    var fail_block = arguments[1].getValue();

    if (!slot_name.value.is(.ByteVector)) {
        return Completion.initRuntimeError(allocator, "Expected ByteVector for the slot name argument of _RemoveSlot:IfFail:, got {s}", .{@tagName(slot_name.value.content)});
    }

    if (!fail_block.value.is(.Block)) {
        return Completion.initRuntimeError(allocator, "Expected Block for the failure block argument of _RemoveSlot:IfFail:, got {s}", .{@tagName(fail_block.value.content)});
    }

    const did_remove_slot = receiver.value.removeSlot(allocator, slot_name.value.content.ByteVector.values) catch |err| switch (err) {
        error.ObjectDoesNotAcceptSlots => {
            return Completion.initRuntimeError(allocator, "Attempted to remove a slot from an object which does not accept slots", .{});
        },
        else => return @errSetCast(Allocator.Error, err),
    };

    if (!did_remove_slot) {
        const returned_value = try message_interpreter.executeBlockMessage(allocator, heap, message_range, fail_block, &[_]Value{}, context);
        returned_value.unrefWithAllocator(allocator);
    }

    return Completion.initNormal(environment.globalNil());
}

/// Inspect the receiver and print it to stderr. Return the receiver.
pub fn Inspect(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Completion {
    _ = heap;
    _ = context;
    _ = arguments;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    try object_inspector.inspectObject(allocator, receiver, .Multiline);

    return Completion.initNormal(receiver);
}

/// Make an identical shallow copy of the receiver and return it.
pub fn Clone(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Completion {
    _ = allocator;
    _ = message_range;
    _ = arguments;
    _ = context;

    const receiver = tracked_receiver.getValue();
    return Completion.initNormal(try receiver.clone(heap));
}

/// Return whether the receiver and argument are identical. Returns either
/// the global "true" or "false" object.
pub fn Eq(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) error{}!Completion {
    _ = allocator;
    _ = heap;
    _ = context;
    _ = message_range;

    return Completion.initNormal(
        if (tracked_receiver.getValue().data == arguments[0].getValue().data)
            environment.globalTrue()
        else
            environment.globalFalse(),
    );
}
