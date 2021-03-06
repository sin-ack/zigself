// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const Completion = @import("../Completion.zig");
const ExecutionResult = @import("../interpreter.zig").ExecutionResult;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Copy the receiver array and create a new one with the size given as the
/// first argument, and if the newly created array has more items than the
/// receiver, fill the extra spaces with the second argument.
///
/// As a special case, if the first argument is 0, then a new empty array is
/// created without looking at the receiver.
pub fn ArrayCopySize_FillingExtrasWith(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ArrayCopySize:FillingExtrasWith:");
    const size = try arguments.getInteger(0, .Unsigned);

    const required_memory = Object.Map.Array.requiredSizeForAllocation() + Object.Array.requiredSizeForAllocation(@intCast(u64, size));
    try context.vm.heap.ensureSpaceInEden(required_memory);

    if (size == 0) {
        const array_map = try Object.Map.Array.create(context.vm.heap, 0);
        const array = try Object.Array.createWithValues(context.vm.heap, array_map, &[_]Value{}, null);
        return ExecutionResult.completion(Completion.initNormal(array.asValue()));
    } else {
        const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
        const filler = arguments.getValue(1);

        const new_array_map = try Object.Map.Array.create(context.vm.heap, @intCast(u64, size));
        const new_array = try Object.Array.createWithValues(context.vm.heap, new_array_map, receiver.getValues(), filler);
        return ExecutionResult.completion(Completion.initNormal(new_array.asValue()));
    }
}

/// Return the size of the receiver array.
pub fn ArraySize(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ArraySize");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
    return ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(receiver.getSize())));
}

/// Return the value at the given position of the receiver array. If the given
/// position is out of bounds, an error is raised.
pub fn ArrayAt(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ArrayAt:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
    const position = try arguments.getInteger(0, .Unsigned);

    const array_values = receiver.getValues();
    if (position >= array_values.len) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(
                context.vm,
                context.source_range,
                "Position passed to _ArrayAt: is out of bounds (position: {d}, size: {d})",
                .{ position, array_values.len },
            ),
        );
    }

    return ExecutionResult.completion(Completion.initNormal(array_values[position]));
}

/// Place the object in the second argument to the integer position in the first
/// argument. If the given position is out of bounds, an error is raised.
/// Returns the receiver.
pub fn ArrayAt_Put(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ArrayAt:Put:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
    const position = try arguments.getInteger(0, .Unsigned);
    const new_value = arguments.getValue(1);

    const array_values = receiver.getValues();
    if (position >= array_values.len) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(
                context.vm,
                context.source_range,
                "Position passed to _ArrayAt:Put: is out of bounds (position: {d}, size: {d})",
                .{ position, array_values.len },
            ),
        );
    }

    array_values[position] = new_value;
    // Since the array object could potentially be in an older space than the
    // value stored in it, let's add the array object to the remembered set.
    try context.vm.heap.rememberObjectReference(receiver.asValue(), new_value);

    return ExecutionResult.completion(Completion.initNormal(receiver.asValue()));
}
