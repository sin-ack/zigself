// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../heap.zig");
const Range = @import("../../language/location_range.zig");
const Value = @import("../value.zig").Value;
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

// FIXME: Add overflow checks here

/// Add two integer numbers. The returned value is an integer.
pub fn IntAdd(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const term = arguments[0].getValue();

    if (!receiver.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntAdd: receiver", .{});
    }

    if (!term.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntAdd: argument", .{});
    }

    return Value.fromInteger(receiver.asInteger() + term.asInteger());
}

/// Subtract the argument from the receiver. The returned value is an integer.
pub fn IntSub(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const term = arguments[0].getValue();

    if (!receiver.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntSub: receiver", .{});
    }

    if (!term.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntSub: argument", .{});
    }

    return Value.fromInteger(receiver.asInteger() - term.asInteger());
}

/// Multiply the argument with the receiver. The returned value is an integer.
pub fn IntMul(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const term = arguments[0].getValue();

    if (!receiver.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntMul: receiver", .{});
    }

    if (!term.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntMul: argument", .{});
    }

    return Value.fromInteger(receiver.asInteger() * term.asInteger());
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntLT(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const term = arguments[0].getValue();

    if (!receiver.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntLT: receiver", .{});
    }

    if (!term.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntLT: argument", .{});
    }

    return if (receiver.asInteger() < term.asInteger())
        environment.globalTrue()
    else
        environment.globalFalse();
}

/// Return whether the receiver is less than its argument. The return value is
/// either "true" or "false".
pub fn IntEq(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const term = arguments[0].getValue();

    if (!receiver.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntEq: receiver", .{});
    }

    if (!term.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntEq: argument", .{});
    }

    return if (receiver.asInteger() == term.asInteger())
        environment.globalTrue()
    else
        environment.globalFalse();
}

/// Return whether the receiver is greater than its argument. The return value
/// is either "true" or "false".
pub fn IntGT(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const term = arguments[0].getValue();

    if (!receiver.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntGT: receiver", .{});
    }

    if (!term.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer as _IntGT: argument", .{});
    }

    return if (receiver.asInteger() > term.asInteger())
        environment.globalTrue()
    else
        environment.globalFalse();
}
