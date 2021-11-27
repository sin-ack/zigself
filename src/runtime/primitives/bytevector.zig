// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Range = @import("../../language/location_range.zig");
const Object = @import("../object.zig");
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Print the given ByteVector to stdout, followed by a newline.
pub fn StringPrint(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = arguments;
    _ = message_range;

    defer receiver.unrefWithAllocator(allocator);

    const writer = std.io.getStdOut().writer();

    // FIXME: Don't ignore errors.
    switch (receiver.value.content) {
        .ByteVector => |byte_vector| {
            writer.print("{s}", .{byte_vector.values}) catch unreachable;
        },

        .Integer => |integer| {
            writer.print("{d}", .{integer.value}) catch unreachable;
        },

        .FloatingPoint => |floating_point| {
            writer.print("{d}", .{floating_point.value}) catch unreachable;
        },

        else => {
            return runtime_error.raiseError(allocator, context, "Unexpected object type {s} passed as the receiver of _StringPrint", .{@tagName(receiver.value.content)});
        },
    }

    return environment.globalNil();
}

/// Return the size of the byte vector in bytes.
pub fn ByteVectorSize(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = arguments;
    _ = message_range;

    defer receiver.unrefWithAllocator(allocator);

    if (!receiver.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector as _ByteVectorSize receiver, got {s}", .{@tagName(receiver.value.content)});
    }

    return Object.createFromIntegerLiteral(allocator, @intCast(i64, receiver.value.content.ByteVector.values.len));
}

/// Return a byte at the given (integer) position of the receiver, which is a
/// byte vector. Fails if the index is out of bounds or if the receiver is not a
/// byte vector.
pub fn ByteAt(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    defer receiver.unrefWithAllocator(allocator);

    var argument = arguments[0];
    defer argument.unrefWithAllocator(allocator);

    if (!receiver.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector as _ByteAt: receiver, got {s}", .{@tagName(receiver.value.content)});
    }

    if (!argument.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _ByteAt: argument, got {s}", .{@tagName(argument.value.content)});
    }

    const values = receiver.value.content.ByteVector.values;
    const position = @intCast(usize, argument.value.content.Integer.value);
    if (position < 0 or position >= values.len) {
        return runtime_error.raiseError(
            allocator,
            context,
            "Argument passed to _ByteAt: is out of bounds for this receiver (passed {d}, size {d})",
            .{ position, values.len },
        );
    }

    return try Object.createFromIntegerLiteral(allocator, values[position]);
}

/// Place the second argument at the position given by the first argument on the
/// byte vector receiver. Fails if the index is out of bounds or if the receiver
/// is not a byte vector.
pub fn ByteAt_Put(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    errdefer receiver.unrefWithAllocator(allocator);

    var first_argument = arguments[0];
    defer first_argument.unrefWithAllocator(allocator);

    var second_argument = arguments[1];
    defer second_argument.unrefWithAllocator(allocator);

    if (!receiver.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector as _ByteAt:Put: receiver, got {s}", .{@tagName(receiver.value.content)});
    }

    if (!first_argument.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as first _ByteAt:Put: argument, got {s}", .{@tagName(first_argument.value.content)});
    }
    if (!second_argument.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as second _ByteAt:Put: argument, got {s}", .{@tagName(second_argument.value.content)});
    }

    var values = receiver.value.content.ByteVector.values;
    const position = @intCast(usize, first_argument.value.content.Integer.value);
    const new_value = second_argument.value.content.Integer.value;

    if (position < 0 or position >= values.len) {
        return runtime_error.raiseError(
            allocator,
            context,
            "First argument passed to _ByteAt:Put: is out of bounds for this receiver (passed {d}, size {d})",
            .{ position, values.len },
        );
    }

    if (new_value < 0 or new_value > 255) {
        return runtime_error.raiseError(allocator, context, "New value passed to _ByteAt:Put: cannot be cast to a byte", .{});
    }

    values[position] = @intCast(u8, new_value);

    return receiver;
}

/// Copy the byte vector receiver with a new size. Size cannot exceed the
/// receiver's size.
pub fn ByteVectorCopySize(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    defer receiver.unrefWithAllocator(allocator);

    var argument = arguments[0];
    defer argument.unrefWithAllocator(allocator);

    if (!receiver.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector as _ByteVectorCopySize: receiver, got {s}", .{@tagName(receiver.value.content)});
    }
    if (!argument.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _ByteVectorCopySize: argument, got {s}", .{@tagName(argument.value.content)});
    }

    const values = receiver.value.content.ByteVector.values;
    const size = argument.value.content.Integer.value;
    if (size >= values.len) {
        return runtime_error.raiseError(allocator, context, "_ByteVectorCopySize: argument exceeds receiver's size", .{});
    }

    if (size < 0) {
        return runtime_error.raiseError(allocator, context, "_ByteVectorCopySize: argument must be positive", .{});
    }

    return try Object.createCopyFromStringLiteral(allocator, values[0..@intCast(usize, size)]);
}
