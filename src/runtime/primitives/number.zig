// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const Completion = @import("../completion.zig");

const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;
const InterpreterError = @import("../interpreter.zig").InterpreterError;

// FIXME: Add overflow checks here

fn integerOpCommon(
    comptime primitive_name: [*:0]const u8,
    context: PrimitiveContext,
    operation: fn (context: PrimitiveContext, receiver: i64, term: i64) InterpreterError!Completion,
) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected integer as _" ++ primitive_name ++ ": receiver",
            .{},
        );
    }

    if (!term.isInteger()) {
        return Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected integer as _" ++ primitive_name ++ ": argument",
            .{},
        );
    }

    return operation(context, receiver.asInteger(), term.asInteger());
}

/// Add two integer numbers. The returned value is an integer.
pub fn IntAdd(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntAdd", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            _ = ctx;
            return Completion.initNormal(Value.fromInteger(receiver + term));
        }
    }.op);
}

/// Subtract the argument from the receiver. The returned value is an integer.
pub fn IntSub(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntSub", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            _ = ctx;
            return Completion.initNormal(Value.fromInteger(receiver - term));
        }
    }.op);
}

/// Multiply the argument with the receiver. The returned value is an integer.
pub fn IntMul(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntMul", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            _ = ctx;
            return Completion.initNormal(Value.fromInteger(receiver * term));
        }
    }.op);
}

/// Perform integer division on the receiver with the argument. Fraction is
/// discarded. The returned value is an integer.
pub fn IntDiv(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntDiv", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            _ = ctx;
            return Completion.initNormal(Value.fromInteger(@divFloor(receiver, term)));
        }
    }.op);
}

/// Perform modulo on the receiver with the argument. The returned value is an
/// integer.
pub fn IntMod(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntMod", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            _ = ctx;
            return Completion.initNormal(Value.fromInteger(@mod(receiver, term)));
        }
    }.op);
}

/// Perform a left shift on the receiver with the argument. The returned value
/// is an integer.
pub fn IntShl(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntShl", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            if (term < 0 or term > 62)
                return Completion.initRuntimeError(ctx.vm, ctx.source_range, "Argument to _IntShl: must be between 0 and 62", .{});
            // FIXME: These functions should be passed i62s in the first place,
            // but doing that requires making Value.fromInteger return 62-bit
            // integers.
            const result: i64 = (receiver << @intCast(u6, term)) & ((@as(i64, 1) << 62) - 1);
            return Completion.initNormal(Value.fromInteger(result));
        }
    }.op);
}

/// Perform a right shift on the receiver with the argument. The returned value
/// is an integer.
pub fn IntShr(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntShr", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            if (term < 0 or term > 62)
                return Completion.initRuntimeError(ctx.vm, ctx.source_range, "Argument to _IntShr: must be between 0 and 62", .{});
            // FIXME: These functions should be passed i62s in the first place,
            // but doing that requires making Value.fromInteger return 62-bit
            // integers.
            const result: i64 = (receiver >> @intCast(u6, term)) & ((@as(i64, 1) << 62) - 1);
            return Completion.initNormal(Value.fromInteger(result));
        }
    }.op);
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntLT(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntLT", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            return Completion.initNormal(
                if (receiver < term)
                    ctx.vm.getTrue()
                else
                    ctx.vm.getFalse(),
            );
        }
    }.op);
}

/// Return whether the receiver is greater than its argument. The return value
/// is either "true" or "false".
pub fn IntGT(context: PrimitiveContext) !Completion {
    return integerOpCommon("IntGT", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: i64, term: i64) !Completion {
            return Completion.initNormal(
                if (receiver > term)
                    ctx.vm.getTrue()
                else
                    ctx.vm.getFalse(),
            );
        }
    }.op);
}

/// Return whether the receiver is equal to its argument. The return value is
/// either "true" or "false". Note that passing a non-integer term is not an
/// error and simply returns false.
pub fn IntEq(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const term = context.arguments[0].getValue();

    if (!receiver.isInteger()) {
        return Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected integer as _IntEq: receiver",
            .{},
        );
    }

    if (!term.isInteger()) {
        return Completion.initNormal(context.vm.getFalse());
    }

    return Completion.initNormal(
        if (receiver.asInteger() == term.asInteger())
            context.vm.getTrue()
        else
            context.vm.getFalse(),
    );
}
