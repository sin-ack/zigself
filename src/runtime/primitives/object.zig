// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("../slot.zig");
const Range = @import("../../language/location_range.zig");
const Object = @import("../object.zig");
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");
const object_inspector = @import("../object_inspector.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;
const message_interpreter = @import("../interpreter/message.zig");

/// Adds the slots in the argument object to the receiver object. The slots
/// are copied. The objects at each slot are not cloned, however.
pub fn AddSlots(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    const argument = arguments[0];
    defer argument.unref();

    if (argument.value.is(.Empty)) {
        return receiver;
    } else if (!argument.value.is(.Slots)) {
        return runtime_error.raiseError(allocator, context, "Expected Empty or Slots object as argument of _AddSlots:, got {s}", .{@tagName(argument.value.content)});
    }

    const argument_slots = argument.value.content.Slots.slots;
    var slots_copy = try std.ArrayList(Slot).initCapacity(allocator, argument_slots.len);
    defer slots_copy.deinit();
    errdefer {
        for (slots_copy.items) |*slot| {
            slot.deinit();
        }
    }

    for (argument_slots) |slot| {
        var slot_copy = try slot.copy();
        errdefer slot_copy.deinit();

        try slots_copy.append(slot_copy);
    }

    receiver.value.addSlots(slots_copy.items) catch |err| switch (err) {
        error.ObjectDoesNotAcceptSlots => {
            return runtime_error.raiseError(allocator, context, "Attempted to add slots to an object which doesn't accept slots", .{});
        },
        else => return @errSetCast(Allocator.Error, err),
    };

    return receiver;
}

/// Removes the given slot. If the slot isn't found or otherwise cannot be
/// removed, the second argument is evaluated as a block.
pub fn RemoveSlot_IfFail(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    defer receiver.unref();

    var slot_name = arguments[0];
    if (!slot_name.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector for the slot name argument of _RemoveSlot:IfFail:, got {s}", .{@tagName(slot_name.value.content)});
    }
    defer slot_name.unref();

    var fail_block = arguments[1];
    if (!fail_block.value.is(.Block)) {
        return runtime_error.raiseError(allocator, context, "Expected Block for the failure block argument of _RemoveSlot:IfFail:, got {s}", .{@tagName(fail_block.value.content)});
    }
    defer fail_block.unref();

    const did_remove_slot = receiver.value.removeSlot(slot_name.value.content.ByteVector.values) catch |err| switch (err) {
        error.ObjectDoesNotAcceptSlots => {
            return runtime_error.raiseError(allocator, context, "Attempted to remove a slot from an object which does not accept slots", .{});
        },
        else => return @errSetCast(Allocator.Error, err),
    };

    if (!did_remove_slot) {
        const returned_value = try message_interpreter.executeBlockMessage(allocator, message_range, fail_block, &[_]Object.Ref{}, context);
        returned_value.unref();
    }

    return environment.globalNil();
}

/// Inspect the receiver and print it to stderr. Return the receiver.
pub fn Inspect(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = context;
    _ = arguments;
    _ = message_range;

    try object_inspector.inspectObject(allocator, receiver, .Multiline);

    return receiver;
}

/// Make an identical shallow copy of the receiver and return it.
pub fn Clone(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = context;
    _ = allocator;
    _ = arguments;
    _ = message_range;

    defer receiver.unref();
    return try receiver.value.copy();
}
