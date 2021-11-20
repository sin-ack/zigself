// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("../slot.zig");
const Object = @import("../object.zig");
const environment = @import("../environment.zig");
const object_inspector = @import("../object_inspector.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Adds the slots in the argument object to the receiver object. The slots
/// are copied. The objects at each slot are not cloned, however.
pub fn AddSlots(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = context;

    const argument = arguments[0];
    defer argument.unref();

    if (argument.value.is(.Empty)) {
        return receiver;
    } else if (!argument.value.is(.Slots)) {
        std.debug.panic("Expected Empty or Slots object as argument of _AddSlots:, got {s}", .{@tagName(argument.value.content)});
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
            @panic("Attempted to add slots to an object which doesn't accept slots");
        },
        else => return @errSetCast(Allocator.Error, err),
    };

    return receiver;
}

/// Inspect the receiver and print it to stderr. Return the receiver.
pub fn Inspect(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = arguments;
    _ = context;

    try object_inspector.inspectObject(allocator, receiver, .Multiline);

    return receiver;
}
