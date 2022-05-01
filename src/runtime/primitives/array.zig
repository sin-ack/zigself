// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const Completion = @import("../Completion.zig");

const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Copy the receiver array and create a new one with the size given as the
/// first argument, and if the newly created array has more items than the
/// receiver, fill the extra spaces with the second argument.
///
/// As a special case, if the first argument is 0, then a new empty array is
/// created without looking at the receiver.
pub fn ArrayCopySize_FillingExtrasWith(context: PrimitiveContext) !?Completion {
    const size_value = context.arguments[0];
    if (!size_value.isInteger()) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected Integer as the first argument of _ArrayCopySize:FillingExtrasWith:", .{});
    }

    const size = size_value.asInteger();
    if (size < 0) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "First argument of _ArrayCopySize:FillingExtrasWith: must be positive", .{});
    }

    const required_memory = Object.Map.Array.requiredSizeForAllocation() + Object.Array.requiredSizeForAllocation(@intCast(u64, size));
    try context.vm.heap.ensureSpaceInEden(required_memory);

    if (size == 0) {
        const array_map = try Object.Map.Array.create(context.vm.heap, 0);
        const array = try Object.Array.createWithValues(context.vm.heap, array_map, &[_]Value{}, null);
        return Completion.initNormal(array.asValue());
    } else {
        var receiver = context.receiver.getValue();
        if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
            return try Completion.initRuntimeError(context.vm, context.source_range, "Expected Array as the receiver of _ArrayCopySize:FillingExtrasWith:", .{});
        }

        const filler = context.arguments[1];

        const new_array_map = try Object.Map.Array.create(context.vm.heap, @intCast(u64, size));
        const new_array = try Object.Array.createWithValues(context.vm.heap, new_array_map, receiver.asObject().asArrayObject().getValues(), filler);
        return Completion.initNormal(new_array.asValue());
    }
}

/// Return the size of the receiver array.
pub fn ArraySize(context: PrimitiveContext) !?Completion {
    const receiver = context.receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected Array as the receiver of _ArraySize", .{});
    }

    return Completion.initNormal(Value.fromUnsignedInteger(receiver.asObject().asArrayObject().getSize()));
}

/// Return the value at the given position of the receiver array. If the given
/// position is out of bounds, an error is raised.
pub fn ArrayAt(context: PrimitiveContext) !?Completion {
    const receiver = context.receiver.getValue();
    const position_value = context.arguments[0];

    if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected Array as the receiver of _ArrayAt:", .{});
    }

    if (!position_value.isInteger()) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected Integer as the first argument of _ArrayAt:", .{});
    }

    const position = position_value.asInteger();
    const array_values = receiver.asObject().asArrayObject().getValues();
    if (position < 0 or position >= array_values.len) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Position passed to _ArrayAt: is out of bounds (position: {d}, size: {d})", .{ position, array_values.len });
    }

    return Completion.initNormal(array_values[@intCast(u64, position)]);
}

/// Place the object in the second argument to the integer position in the first
/// argument. If the given position is out of bounds, an error is raised.
/// Returns the receiver.
pub fn ArrayAt_Put(context: PrimitiveContext) !?Completion {
    const receiver = context.receiver.getValue();
    const position_value = context.arguments[0];
    const new_value = context.arguments[1];

    if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected Array as the receiver of _ArrayAt:Put:", .{});
    }

    if (!position_value.isInteger()) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected Integer as the first argument of _ArrayAt:Put:", .{});
    }

    const position = position_value.asInteger();
    const array_values = receiver.asObject().asArrayObject().getValues();
    if (position < 0 or position >= array_values.len) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Position passed to _ArrayAt:Put: is out of bounds (position: {d}, size: {d})", .{ position, array_values.len });
    }

    array_values[@intCast(u64, position)] = new_value;
    // Since the array object could potentially be in an older space than the
    // value stored in it, let's add the array object to the remembered set.
    try context.vm.heap.rememberObjectReference(receiver, new_value);

    return Completion.initNormal(receiver);
}
