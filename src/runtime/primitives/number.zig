// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const RuntimeError = @import("../RuntimeError.zig");
const ExecutionResult = @import("../execution_result.zig").ExecutionResult;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

// FIXME: Add overflow checks here

fn integerOpCommon(
    comptime primitive_name: []const u8,
    context: *PrimitiveContext,
    comptime operation: fn (context: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) Allocator.Error!ExecutionResult,
) !ExecutionResult {
    const arguments = context.getArguments("_" ++ primitive_name ++ ":");
    const receiver = try arguments.getInteger(PrimitiveContext.Receiver, .Signed);
    const term = try arguments.getInteger(0, .Signed);

    return @call(.always_inline, operation, .{ context.*, receiver, term });
}

/// Add two integer numbers. The returned value is an integer.
pub fn IntAdd(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntAdd", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            const result, const overflow = @addWithOverflow(receiver, term);
            if (overflow == 1) {
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                    ctx.source_range,
                    "Integer overflow",
                ));
            }
            return ExecutionResult.resolve(Value.fromInteger(result));
        }
    }.op);
}

/// Subtract the argument from the receiver. The returned value is an integer.
pub fn IntSub(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntSub", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            const result, const overflow = @subWithOverflow(receiver, term);
            if (overflow == 1) {
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                    ctx.source_range,
                    "Integer underflow",
                ));
            }
            return ExecutionResult.resolve(Value.fromInteger(result));
        }
    }.op);
}

/// Multiply the argument with the receiver. The returned value is an integer.
pub fn IntMul(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntMul", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            const result, const overflow = @mulWithOverflow(receiver, term);
            if (overflow == 1) {
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                    ctx.source_range,
                    "Integer overflow",
                ));
            }
            return ExecutionResult.resolve(Value.fromInteger(result));
        }
    }.op);
}

/// Perform integer division on the receiver with the argument. Fraction is
/// discarded. The returned value is an integer.
pub fn IntDiv(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntDiv", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            if (term == 0)
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                    ctx.source_range,
                    "Division by zero",
                ));
            return ExecutionResult.resolve(Value.fromInteger(@divFloor(receiver, term)));
        }
    }.op);
}

/// Perform modulo on the receiver with the argument. The returned value is an
/// integer.
pub fn IntMod(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntMod", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            if (term == 0)
                return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                    ctx.source_range,
                    "Modulo by zero",
                ));
            return ExecutionResult.resolve(Value.fromInteger(@mod(receiver, term)));
        }
    }.op);
}

/// Perform a left shift on the receiver with the argument. The returned value
/// is an integer.
pub fn IntShl(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntShl", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            if (term < 0 or term > Value.DataBits)
                return ExecutionResult.runtimeError(RuntimeError.initFormattedComptime(
                    ctx.source_range,
                    "Argument to _IntShl: must be between 0 and {}",
                    .{Value.DataBits},
                ));
            const result: Value.SignedData = (receiver << @as(u6, @intCast(term)));
            return ExecutionResult.resolve(Value.fromInteger(result));
        }
    }.op);
}

/// Perform a right shift on the receiver with the argument. The returned value
/// is an integer.
pub fn IntShr(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntShr", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            if (term < 0 or term > Value.DataBits)
                return ExecutionResult.runtimeError(
                    RuntimeError.initFormattedComptime(
                        ctx.source_range,
                        "Argument to _IntShr: must be between 0 and {}",
                        .{Value.DataBits},
                    ),
                );
            const result: Value.SignedData = (receiver >> @as(u6, @intCast(term)));
            return ExecutionResult.resolve(Value.fromInteger(result));
        }
    }.op);
}

/// Perform a bitwise XOR on the receiver with the argument. The returned value
/// is an integer.
pub fn IntXor(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntXor", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            _ = ctx;
            return ExecutionResult.resolve(Value.fromInteger(receiver ^ term));
        }
    }.op);
}

/// Perform a bitwise AND on the receiver with the argument. The returned value
/// is an integer.
pub fn IntAnd(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntAnd", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            _ = ctx;
            return ExecutionResult.resolve(Value.fromInteger(receiver & term));
        }
    }.op);
}

/// Perform a bitwise OR on the receiver with the argument. The returned value
/// is an integer.
pub fn IntOr(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntOr", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            _ = ctx;
            return ExecutionResult.resolve(Value.fromInteger(receiver | term));
        }
    }.op);
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntLT(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntLT", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            return ExecutionResult.resolve(
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
pub fn IntGT(context: *PrimitiveContext) !ExecutionResult {
    return try integerOpCommon("IntGT", context, struct {
        pub fn op(ctx: PrimitiveContext, receiver: Value.SignedData, term: Value.SignedData) !ExecutionResult {
            return ExecutionResult.resolve(
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
pub fn IntEq(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_IntEq:");
    const receiver = try arguments.getInteger(PrimitiveContext.Receiver, .Signed);
    const term = arguments.getValue(0);

    if (term.type != .Integer) {
        return ExecutionResult.resolve(context.vm.getFalse());
    }

    return ExecutionResult.resolve(
        if (receiver == term.asInteger())
            context.vm.getTrue()
        else
            context.vm.getFalse(),
    );
}
