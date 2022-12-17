// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const ByteArray = @import("../ByteArray.zig");
const Completion = @import("../Completion.zig");
const ByteArrayObject = @import("../objects/byte_array.zig").ByteArray;
const ExecutionResult = @import("../interpreter.zig").ExecutionResult;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Return the size of the byte vector in bytes.
pub fn ByteArraySize(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ByteArraySize");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    return ExecutionResult.completion(Completion.initNormal(Value.fromInteger(@intCast(i64, receiver.getValues().len))));
}

/// Return a byte at the given (integer) position of the receiver, which is a
/// byte vector. Fails if the index is out of bounds or if the receiver is not a
/// byte vector.
pub fn ByteAt(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ByteAt:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    var position = try arguments.getInteger(0, .Unsigned);
    if (try context.wouldOverflow(usize, position, "position")) |result| return result;

    const values = receiver.getValues();
    if (position >= values.len) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Argument passed to _ByteAt: is out of bounds for this receiver (passed {d}, size {d})",
            .{ position, values.len },
        ));
    }

    return ExecutionResult.completion(Completion.initNormal(Value.fromInteger(values[@intCast(usize, position)])));
}

/// Place the second argument at the position given by the first argument on the
/// byte vector receiver. Fails if the index is out of bounds or if the receiver
/// is not a byte vector.
pub fn ByteAt_Put(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ByteAt:Put:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    const position = try arguments.getInteger(0, .Unsigned);
    const new_value = try arguments.getInteger(1, .Unsigned);
    if (try context.wouldOverflow(usize, position, "position")) |result| return result;

    var values = receiver.getValues();

    if (position >= values.len) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "First argument passed to _ByteAt:Put: is out of bounds for this receiver (passed {d}, size {d})",
            .{ position, values.len },
        ));
    }

    if (new_value > 255) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "New value passed to _ByteAt:Put: cannot be cast to a byte",
            .{},
        ));
    }

    values[@intCast(usize, position)] = @intCast(u8, new_value);
    return ExecutionResult.completion(Completion.initNormal(receiver.asValue()));
}

/// Copy the byte vector receiver with a new size. Extra space is filled
/// with the second argument (must be a byte array of length 1).
pub fn ByteArrayCopySize_FillingExtrasWith(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ByteArrayCopySize:FillingExtrasWith:");
    var receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    const size = try arguments.getInteger(0, .Unsigned);
    const filler_byte_array = try arguments.getObject(1, .ByteArray);
    if (try context.wouldOverflow(usize, size, "size")) |result| return result;

    const filler_contents = filler_byte_array.getValues();
    if (filler_contents.len != 1) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Filler argument to _ByteArrayCopySize:FillingExtrasWith: must have a length of 1",
            .{},
        ));
    }

    const filler = filler_contents[0];

    var token = try context.vm.heap.getAllocation(
        ByteArrayObject.requiredSizeForAllocation(@intCast(usize, size)),
    );
    defer token.deinit();

    // Refresh pointers
    receiver = context.receiver.getValue().asObject().asType(.ByteArray).?;

    var values = receiver.getValues();

    const new_byte_array = ByteArray.createUninitialized(&token, @intCast(usize, size));
    const bytes_to_copy = @intCast(usize, std.math.min(size, values.len));
    std.mem.copy(u8, new_byte_array.getValues(), values[0..bytes_to_copy]);

    if (size > values.len) {
        std.mem.set(u8, new_byte_array.getValues()[bytes_to_copy..], filler);
    }

    const byte_array_object = ByteArrayObject.create(context.vm.getMapMap(), &token, context.actor.id, new_byte_array);
    return ExecutionResult.completion(Completion.initNormal(byte_array_object.asValue()));
}

/// Return whether the receiver byte array is equal to the argument.
/// Note that the argument not being a byte array is not an error
/// and this primitive simply returns false in that case.
pub fn ByteArrayEq(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ByteArrayEq:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    const argument = arguments.getValue(0);

    if (argument.isObjectReference()) {
        if (argument.asObject().asType(.ByteArray)) |byte_array| {
            return ExecutionResult.completion(Completion.initNormal(
                if (std.mem.eql(u8, receiver.getValues(), byte_array.getValues()))
                    context.vm.getTrue()
                else
                    context.vm.getFalse(),
            ));
        }
    }

    return ExecutionResult.completion(Completion.initNormal(context.vm.getFalse()));
}

pub fn ByteArrayConcatenate(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ByteArrayConcatenate:");
    var receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    var argument = try arguments.getObject(0, .ByteArray);

    // FIXME: A byte array can have free capacity in it if its length is not a
    //        multiple of a machine word. Use this to optimize small
    //        concatenations.

    const receiver_size = receiver.getValues().len;
    const argument_size = argument.getValues().len;

    var token = try context.vm.heap.getAllocation(
        ByteArrayObject.requiredSizeForAllocation(receiver_size + argument_size),
    );
    defer token.deinit();

    // Refresh pointers
    receiver = context.receiver.getValue().asObject().asType(.ByteArray).?;
    argument = context.arguments[0].asObject().asType(.ByteArray).?;

    var new_byte_array = ByteArray.createUninitialized(&token, receiver_size + argument_size);
    std.mem.copy(u8, new_byte_array.getValues()[0..receiver_size], receiver.getValues());
    std.mem.copy(u8, new_byte_array.getValues()[receiver_size..], argument.getValues());

    const byte_array_object = ByteArrayObject.create(context.vm.getMapMap(), &token, context.actor.id, new_byte_array);
    return ExecutionResult.completion(Completion.initNormal(byte_array_object.asValue()));
}
