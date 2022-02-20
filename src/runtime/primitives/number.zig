// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const Completion = @import("../completion.zig");
const environment = @import("../environment.zig");

const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

// FIXME: Add overflow checks here

/// Add two integer numbers. The returned value is an integer.
pub fn IntAdd(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntAdd: receiver", .{});
    }

    if (!term.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntAdd: argument", .{});
    }

    return Completion.initNormal(Value.fromInteger(receiver.asInteger() + term.asInteger()));
}

/// Subtract the argument from the receiver. The returned value is an integer.
pub fn IntSub(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntSub: receiver", .{});
    }

    if (!term.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntSub: argument", .{});
    }

    return Completion.initNormal(Value.fromInteger(receiver.asInteger() - term.asInteger()));
}

/// Multiply the argument with the receiver. The returned value is an integer.
pub fn IntMul(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntMul: receiver", .{});
    }

    if (!term.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntMul: argument", .{});
    }

    return Completion.initNormal(Value.fromInteger(receiver.asInteger() * term.asInteger()));
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntLT(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntLT: receiver", .{});
    }

    if (!term.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntLT: argument", .{});
    }

    return Completion.initNormal(
        if (receiver.asInteger() < term.asInteger())
            environment.globalTrue()
        else
            environment.globalFalse(),
    );
}

/// Return whether the receiver is equal to its argument. The return value is
/// either "true" or "false". Note that passing a non-integer term is not an
/// error and simply returns false.
pub fn IntEq(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntEq: receiver", .{});
    }

    if (!term.isInteger()) {
        return Completion.initNormal(environment.globalFalse());
    }

    return Completion.initNormal(
        if (receiver.asInteger() == term.asInteger())
            environment.globalTrue()
        else
            environment.globalFalse(),
    );
}

/// Return whether the receiver is greater than its argument. The return value
/// is either "true" or "false".
pub fn IntGT(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntGT: receiver", .{});
    }

    if (!term.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _IntGT: argument", .{});
    }

    return Completion.initNormal(
        if (receiver.asInteger() > term.asInteger())
            environment.globalTrue()
        else
            environment.globalFalse(),
    );
}
