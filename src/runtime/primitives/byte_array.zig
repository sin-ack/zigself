// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Completion = @import("../completion.zig");
const ByteArray = @import("../byte_array.zig");
const environment = @import("../environment.zig");

const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Print the given ByteArray to stdout, followed by a newline.
pub fn StringPrint(context: PrimitiveContext) !Completion {
    const writer = std.io.getStdOut().writer();

    // FIXME: Don't ignore errors.
    const receiver = context.receiver.getValue();
    switch (receiver.getType()) {
        .Integer => {
            writer.print("{d}", .{receiver.asInteger()}) catch unreachable;
        },
        .FloatingPoint => {
            writer.print("{d}", .{receiver.asFloatingPoint()}) catch unreachable;
        },
        .ObjectReference => {
            if (!(receiver.asObject().isByteArrayObject())) {
                return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected ByteArray as the receiver of _StringPrint", .{});
            }

            writer.print("{s}", .{receiver.asObject().asByteArrayObject().getValues()}) catch unreachable;
        },
        else => unreachable,
    }

    return Completion.initNormal(environment.globalNil());
}

/// Return the size of the byte vector in bytes.
pub fn ByteArraySize(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected ByteArray as _ByteArraySize receiver", .{});
    }

    return Completion.initNormal(Value.fromInteger(@intCast(i64, receiver.asObject().asByteArrayObject().getValues().len)));
}

/// Return a byte at the given (integer) position of the receiver, which is a
/// byte vector. Fails if the index is out of bounds or if the receiver is not a
/// byte vector.
pub fn ByteAt(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const argument = context.arguments[0].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected ByteArray as _ByteAt: receiver", .{});
    }

    if (!argument.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as _ByteAt: argument", .{});
    }

    const values = receiver.asObject().asByteArrayObject().getValues();
    const position = @intCast(usize, argument.asInteger());
    if (position < 0 or position >= values.len) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Argument passed to _ByteAt: is out of bounds for this receiver (passed {d}, size {d})",
            .{ position, values.len },
        );
    }

    return Completion.initNormal(Value.fromInteger(values[position]));
}

/// Place the second argument at the position given by the first argument on the
/// byte vector receiver. Fails if the index is out of bounds or if the receiver
/// is not a byte vector.
pub fn ByteAt_Put(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const first_argument = context.arguments[0].getValue();
    const second_argument = context.arguments[1].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected ByteArray as _ByteAt:Put: receiver", .{});
    }

    if (!first_argument.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as first _ByteAt:Put: argument", .{});
    }
    if (!second_argument.isInteger()) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected integer as second _ByteAt:Put: argument", .{});
    }

    var values = receiver.asObject().asByteArrayObject().getValues();
    const position = @intCast(usize, first_argument.asInteger());
    const new_value = second_argument.asInteger();

    if (position < 0 or position >= values.len) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "First argument passed to _ByteAt:Put: is out of bounds for this receiver (passed {d}, size {d})",
            .{ position, values.len },
        );
    }

    if (new_value < 0 or new_value > 255) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "New value passed to _ByteAt:Put: cannot be cast to a byte", .{});
    }

    values[position] = @intCast(u8, new_value);

    return Completion.initNormal(receiver);
}

/// Copy the byte vector receiver with a new size. Extra space is filled
/// with the second argument (must be a byte array of length 1).
pub fn ByteArrayCopySize_FillingExtrasWith(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    const size_value = context.arguments[0].getValue();
    const filler_value = context.arguments[1].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected byte array as receiver of _ByteArrayCopySize:FillingExtrasWith:",
            .{},
        );
    }

    if (!size_value.isInteger()) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected integer as the first argument to _ByteArrayCopySize:FillingExtrasWith:",
            .{},
        );
    }

    if (!(filler_value.isObjectReference() and filler_value.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected byte array as the second argument to _ByteArrayCopySize:FillingExtrasWith:",
            .{},
        );
    }

    const size = size_value.asInteger();
    if (size < 0) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Size argument to _ByteArrayCopySize:FillingExtrasWith: must be positive",
            .{},
        );
    }

    const filler_contents = filler_value.asObject().asByteArrayObject().getValues();
    if (filler_contents.len != 1) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Filler argument to _ByteArrayCopySize:FillingExtrasWith: must have a length of 1",
            .{},
        );
    }

    const filler = filler_contents[0];

    const values = receiver.asObject().asByteArrayObject().getValues();

    try context.interpreter_context.heap.ensureSpaceInEden(
        ByteArray.requiredSizeForAllocation(@intCast(u64, size)) +
            Object.Map.ByteArray.requiredSizeForAllocation() +
            Object.ByteArray.requiredSizeForAllocation(),
    );

    const new_byte_array = try ByteArray.createUninitialized(context.interpreter_context.heap, @intCast(usize, size));
    const bytes_to_copy = @intCast(usize, std.math.min(size, values.len));
    std.mem.copy(u8, new_byte_array.getValues(), values[0..bytes_to_copy]);

    if (size > values.len) {
        std.mem.set(u8, new_byte_array.getValues()[bytes_to_copy..], filler);
    }

    const byte_array_map = try Object.Map.ByteArray.create(context.interpreter_context.heap, new_byte_array);
    return Completion.initNormal((try Object.ByteArray.create(context.interpreter_context.heap, byte_array_map)).asValue());
}

/// Return whether the receiver byte array is equal to the argument.
/// Note that the argument not being a byte array is not an error
/// and this primitive simply returns false in that case.
pub fn ByteArrayEq(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    var argument = context.arguments[0].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected ByteArray as _ByteArrayEq: receiver", .{});
    }

    if (!(argument.isObjectReference() and argument.asObject().isByteArrayObject())) {
        return Completion.initNormal(environment.globalFalse());
    }

    return Completion.initNormal(
        if (std.mem.eql(u8, receiver.asObject().asByteArrayObject().getValues(), argument.asObject().asByteArrayObject().getValues()))
            environment.globalTrue()
        else
            environment.globalFalse(),
    );
}

pub fn ByteArrayConcatenate(context: PrimitiveContext) !Completion {
    var receiver_value = context.receiver.getValue();
    var argument_value = context.arguments[0].getValue();

    if (!(receiver_value.isObjectReference() and receiver_value.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected ByteArray as _ByteArrayConcatenate: receiver", .{});
    }

    if (!(argument_value.isObjectReference() and argument_value.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.interpreter_context.allocator, context.source_range, "Expected ByteArray as _ByteArrayConcatenate: argument", .{});
    }

    // FIXME: A byte array can have free capacity in it if its length is not a
    //        multiple of a machine word. Use this to optimize small
    //        concatenations.

    var receiver = receiver_value.asObject().asByteArrayObject();
    var argument = argument_value.asObject().asByteArrayObject();

    const receiver_size = receiver.getValues().len;
    const argument_size = argument.getValues().len;

    try context.interpreter_context.heap.ensureSpaceInEden(
        ByteArray.requiredSizeForAllocation(receiver_size + argument_size) +
            Object.Map.ByteArray.requiredSizeForAllocation() +
            Object.ByteArray.requiredSizeForAllocation(),
    );

    // Refresh pointers
    receiver = context.receiver.getValue().asObject().asByteArrayObject();
    argument = context.arguments[0].getValue().asObject().asByteArrayObject();

    var new_byte_array = try ByteArray.createUninitialized(context.interpreter_context.heap, receiver_size + argument_size);
    std.mem.copy(u8, new_byte_array.getValues()[0..receiver_size], receiver.getValues());
    std.mem.copy(u8, new_byte_array.getValues()[receiver_size..], argument.getValues());

    const byte_array_map = try Object.Map.ByteArray.create(context.interpreter_context.heap, new_byte_array);
    return Completion.initNormal((try Object.ByteArray.create(context.interpreter_context.heap, byte_array_map)).asValue());
}
