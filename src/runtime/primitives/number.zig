// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

fn getNumberTraits(lobby: Object.Ref) !Object.Ref {
    // FIXME: Convert these panics into runtime errors
    if (try lobby.value.lookup("traits", .Value)) |traits| {
        if (try traits.value.lookup("number", .Value)) |traits_number| {
            return traits_number;
        } else {
            @panic("Could not find number in traits");
        }
    } else {
        @panic("Could not find traits in lobby");
    }
}

fn makeInteger(allocator: *Allocator, value: u64, lobby: Object.Ref) !Object.Ref {
    var number_traits = try getNumberTraits(lobby);
    number_traits.ref();
    return try Object.createFromIntegerLiteral(allocator, value, number_traits);
}

/// Add two integer numbers. The returned value is an integer.
pub fn IntAdd(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
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
        context.lobby,
    );

    return result;
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntLT(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
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
