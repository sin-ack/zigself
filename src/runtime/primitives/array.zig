// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const traversal = @import("../object_traversal.zig");
const BaseObject = @import("../base_object.zig").BaseObject;
const array_object = @import("../objects/array.zig");
const RuntimeError = @import("../RuntimeError.zig");
const ExecutionResult = @import("../execution_result.zig").ExecutionResult;
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
    if (try context.wouldOverflow(usize, size, "size")) |result| return result;

    const required_memory = array_object.ArrayMap.requiredSizeForAllocation() + array_object.Array.requiredSizeForAllocation(@intCast(size));
    var token = try context.vm.heap.getAllocation(required_memory);
    defer token.deinit();

    if (size == 0) {
        const array_map = array_object.ArrayMap.create(&token, 0);
        const array = array_object.Array.createWithValues(&token, context.actor.id, array_map, &[_]Value{}, null);
        return ExecutionResult.resolve(array.asValue());
    } else {
        const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
        const filler = arguments.getValue(1);

        const new_array_map = array_object.ArrayMap.create(&token, @intCast(size));
        const new_array = array_object.Array.createWithValues(&token, context.actor.id, new_array_map, receiver.getValues(), filler);
        return ExecutionResult.resolve(new_array.asValue());
    }
}

/// Return the size of the receiver array.
pub fn ArraySize(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ArraySize");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
    return ExecutionResult.resolve(Value.fromUnsignedInteger(receiver.getSize()));
}

/// Return the value at the given position of the receiver array. If the given
/// position is out of bounds, an error is raised.
pub fn ArrayAt(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ArrayAt:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
    const position = try arguments.getInteger(0, .Unsigned);
    if (try context.wouldOverflow(usize, position, "position")) |result| return result;

    const array_values = receiver.getValues();
    if (position >= array_values.len) {
        return ExecutionResult.runtimeError(
            try RuntimeError.initFormatted(
                context.source_range,
                "Position passed to _ArrayAt: is out of bounds (position: {d}, size: {d})",
                .{ position, array_values.len },
            ),
        );
    }

    return ExecutionResult.resolve(array_values[@intCast(position)]);
}

/// Place the object in the second argument to the integer position in the first
/// argument. If the given position is out of bounds, an error is raised.
/// Returns the receiver.
pub fn ArrayAt_Put(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ArrayAt:Put:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Array);
    const position = try arguments.getInteger(0, .Unsigned);
    const new_value = arguments.getValue(1);

    if (try context.wouldOverflow(usize, position, "position")) |result| return result;

    if (!context.actor.canWriteTo(context.receiver.getValue())) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ArrayAt:Put: receiver is not writable for actor",
        ));
    }

    const array_values = receiver.getValues();
    if (position >= array_values.len) {
        return ExecutionResult.runtimeError(
            try RuntimeError.initFormatted(
                context.source_range,
                "Position passed to _ArrayAt:Put: is out of bounds (position: {d}, size: {d})",
                .{ position, array_values.len },
            ),
        );
    }

    array_values[@intCast(position)] = new_value;

    if (receiver.object.getMetadata().reachability == .Global) {
        // Mark the object graph of new_value as globally reachable
        _ = traversal.traverseNonGloballyReachableObjectGraph(new_value, struct {
            pub fn visit(self: @This(), base_object: BaseObject.Ptr) error{}!BaseObject.Ptr {
                _ = self;
                base_object.metadata.reachability = .Global;
                return base_object;
            }
        }{}) catch unreachable;
    }

    // Since the array object could potentially be in an older space than the
    // value stored in it, let's add the array object to the remembered set.
    try context.vm.heap.rememberObjectReference(receiver.asValue(), new_value);

    return ExecutionResult.resolve(receiver.asValue());
}
