// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("../slot.zig");
const Range = @import("../../language/location_range.zig");
const Object = @import("../object.zig");
const Activation = @import("../activation.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Returns whether the passed message name is the correct one for this block
/// to be executed. The logic is:
///
/// - For blocks with no arguments, `value`
/// - For blocks with a single argument, `value:`
/// - For blocks with more than one argument, `value:With:`, with as many
///   `With:`s as needed (number of colons should match number of arguments)
pub fn isCorrectMessageForBlockExecution(self: Object, message: []const u8) bool {
    std.debug.assert(self.is(.Block));

    if (self.content.Block.arguments.len == 0 and std.mem.eql(u8, message, "value")) {
        return true;
    }

    if (message.len < 6 or !std.mem.eql(u8, message[0..6], "value:")) {
        return false;
    }

    var remaining_message = message[6..];
    var remaining_arguments = self.content.Block.arguments.len - 1;
    while (remaining_arguments > 0) : (remaining_arguments -= 1) {
        if (remaining_message.len == 0)
            return false;

        var with_slice = remaining_message[0..5];
        if (!std.mem.eql(u8, with_slice, "With:"))
            return false;

        remaining_message = remaining_message[5..];
    }

    return remaining_message.len == 0;
}

fn createMessageNameForBlock(self: Object) ![]const u8 {
    std.debug.assert(self.is(.Block));

    var needed_space: usize = 5; // value
    if (self.content.Block.arguments.len > 0) {
        needed_space += 1; // :
        needed_space += 5 * (self.content.Block.arguments.len - 1); // Any other With:s needed
    }

    var message_name = try self.allocator.alloc(u8, needed_space);
    std.mem.copy(u8, message_name, "value");

    if (self.content.Block.arguments.len > 0) {
        message_name[5] = ':';

        var remaining_buffer = message_name[6..];
        while (remaining_buffer.len > 0) {
            std.mem.copy(u8, remaining_buffer, "With:");
            remaining_buffer = remaining_buffer[5..];
        }
    }

    return message_name;
}

fn createActivationObject(
    allocator: *Allocator,
    arguments: []Object.Ref,
    argument_names: [][]const u8,
    slots: []Slot,
    receiver: Object.Ref,
    check_activation_receiver: bool,
) !Object.Ref {
    std.debug.assert(arguments.len == argument_names.len);

    var slots_copy = try std.ArrayList(Slot).initCapacity(allocator, slots.len + arguments.len);
    errdefer {
        for (slots_copy.items) |*slot| {
            slot.deinit();
        }
        slots_copy.deinit();
    }

    // Add argument slots first, as those are more likely to be used
    for (arguments) |argument, i| {
        var new_slot = try Slot.init(allocator, false, false, argument_names[i], argument);
        errdefer new_slot.deinit();

        try slots_copy.append(new_slot);
    }

    for (slots) |slot| {
        var slot_copy = try slot.copy();
        errdefer slot_copy.deinit();

        try slots_copy.append(slot_copy);
    }

    // NOTE: This is very important! When we're performing a method activation,
    //       we must NOT select previous method or block activation objects as
    //       receiver, that would make their slots visible to us which we
    //       absolutely do not want.
    //
    //       Not only that, but that would also cause the actual object to be
    //       wrapped in multiple layers of activation objects like ogres.
    var the_receiver = receiver;
    if (check_activation_receiver) {
        if (try receiver.value.findActivationReceiver()) |actual_receiver| {
            the_receiver = actual_receiver;
        }
    }
    the_receiver.ref();

    return try Object.create(allocator, .{ .Activation = .{ .slots = slots_copy.toOwnedSlice(), .receiver = the_receiver } });
}

/// Activate this block and return a slots object for it.
/// Borrows 1 ref from each object in `arguments`.
/// `parent_activation_object` is ref'd internally.
/// `context.script` is ref'd once.
pub fn activateBlock(self: Object, context: *InterpreterContext, message_range: Range, arguments: []Object.Ref, parent_activation_object: Object.Ref) !*Activation {
    std.debug.assert(self.is(.Block));

    const activation_object = try createActivationObject(
        self.allocator,
        arguments,
        self.content.Block.arguments,
        self.content.Block.slots,
        parent_activation_object,
        false,
    );
    errdefer activation_object.unref();

    var message_name = try createMessageNameForBlock(self);
    errdefer self.allocator.free(message_name);

    context.script.ref();
    errdefer context.script.unref();

    var activation = try Activation.create(self.allocator, activation_object, message_name, context.script, message_range);
    // If we got here then the parent and non-local return target activations
    // must exist.
    activation.parent_activation = self.content.Block.parent_activation.getPointer().?;
    activation.nonlocal_return_target_activation = self.content.Block.nonlocal_return_target_activation.getPointer().?;
    return activation;
}

/// Activate this method and return a slots object for it.
/// Borrows 1 ref from each object in `arguments`.
/// `parent` is ref'd internally.
/// `context.script` is ref'd once.
pub fn activateMethod(self: Object, context: *InterpreterContext, message_range: Range, arguments: []Object.Ref, parent: Object.Ref) !*Activation {
    std.debug.assert(self.is(.Method));

    const activation_object = try createActivationObject(
        self.allocator,
        arguments,
        self.content.Method.arguments,
        self.content.Method.slots,
        parent,
        true,
    );
    errdefer activation_object.unref();

    var message_name_copy = try self.allocator.dupe(u8, self.content.Method.message_name);
    errdefer self.allocator.free(message_name_copy);

    context.script.ref();
    errdefer context.script.unref();

    return try Activation.create(self.allocator, activation_object, message_name_copy, context.script, message_range);
}
