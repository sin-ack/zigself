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

fn makeInteger(allocator: *Allocator, value: u64) !Object.Ref {
    return try Object.createFromIntegerLiteral(allocator, value);
}

/// Add two integer numbers. The returned value is an integer.
pub fn IntAdd(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    if (!receiver.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntAdd: receiver, got {s}", .{@tagName(receiver.value.content)});
    }
    defer receiver.unref();

    var addend = arguments[0];
    if (!addend.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntAdd: argument, got {s}", .{@tagName(addend.value.content)});
    }
    defer addend.unref();

    const result = try makeInteger(
        allocator,
        receiver.value.content.Integer.value + addend.value.content.Integer.value,
    );

    return result;
}

/// Subtract the argument from the receiver. The returned value is an integer.
pub fn IntSub(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    if (!receiver.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntSub: receiver, got {s}", .{@tagName(receiver.value.content)});
    }
    defer receiver.unref();

    var term = arguments[0];
    if (!term.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntSub: argument, got {s}", .{@tagName(term.value.content)});
    }
    defer term.unref();

    const result = try makeInteger(
        allocator,
        receiver.value.content.Integer.value - term.value.content.Integer.value,
    );

    return result;
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntLT(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    if (!receiver.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntLT: receiver, got {s}", .{@tagName(receiver.value.content)});
    }
    defer receiver.unref();

    var argument = arguments[0];
    if (!argument.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntLT: argument, got {s}", .{@tagName(argument.value.content)});
    }
    defer argument.unref();

    const result = if (receiver.value.content.Integer.value < argument.value.content.Integer.value)
        environment.globalTrue()
    else
        environment.globalFalse();

    return result;
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntEq(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    defer receiver.unref();
    if (!receiver.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntEq: receiver, got {s}", .{@tagName(receiver.value.content)});
    }

    var argument = arguments[0];
    defer argument.unref();
    if (!argument.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as _IntEq: argument, got {s}", .{@tagName(argument.value.content)});
    }

    const result = if (receiver.value.content.Integer.value == argument.value.content.Integer.value)
        environment.globalTrue()
    else
        environment.globalFalse();

    return result;
}
