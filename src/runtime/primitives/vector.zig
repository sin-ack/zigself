// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Range = @import("../../language/location_range.zig");
const Object = @import("../object.zig");
const ByteVector = @import("../byte_vector.zig");
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Copy the receiver vector and create a new one with the size given as the
/// first argument, and if the newly created vector has more items than the
/// receiver, fill the extra spaces with the second argument.
///
/// As a special case, if the first argument is 0, then a new empty vector is
/// created without looking at the receiver.
pub fn VectorCopySize_FillingExtrasWith(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) !Value {
    _ = tracked_receiver;
    _ = message_range;

    const size_value = arguments[0].getValue();
    if (!size_value.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as the first argument of _VectorCopySize:FillingExtrasWith:", .{});
    }

    const size = size_value.asInteger();
    if (size < 0) {
        return runtime_error.raiseError(allocator, context, "First argument of _VectorCopySize:FillingExtrasWith: must be positive", .{});
    }

    const required_memory = Object.Map.Vector.requiredSizeForAllocation() + Object.Vector.requiredSizeForAllocation(@intCast(u64, size));
    try heap.ensureSpaceInEden(required_memory);

    if (size == 0) {
        const vector_map = try Object.Map.Vector.create(heap, 0);
        const vector = try Object.Vector.createWithValues(heap, vector_map, &[_]Value{}, null);
        return vector.asValue();
    } else {
        var receiver = tracked_receiver.getValue();
        if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
            return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorCopySize:FillingExtrasWith:", .{});
        }

        const filler = arguments[1].getValue();

        // FIXME: This first fills up the new map with the filler and then overwrites the
        //        values which we do have. This is too much work, let's just fill the space
        //        that's actually extra.
        const new_vector_map = try Object.Map.Vector.create(heap, @intCast(u64, size));
        const new_vector = try Object.Vector.createWithValues(heap, new_vector_map, receiver.asObject().asVectorObject().getValues(), filler);
        return new_vector.asValue();
    }
}

/// Return the size of the receiver vector.
pub fn VectorSize(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) !Value {
    _ = allocator;
    _ = heap;
    _ = message_range;
    _ = arguments;
    _ = context;

    const receiver = tracked_receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
        return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorSize", .{});
    }

    return Value.fromUnsignedInteger(receiver.asObject().asVectorObject().getSize());
}

/// Return the value at the given position of the receiver vector. If the given
/// position is out of bounds, an error is raised.
pub fn VectorAt(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const position_value = arguments[0].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
        return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorAt:", .{});
    }

    if (!position_value.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as the first argument of _VectorAt:", .{});
    }

    const position = position_value.asInteger();
    const vector_values = receiver.asObject().asVectorObject().getValues();
    if (position < 0 or position >= vector_values.len) {
        return runtime_error.raiseError(allocator, context, "Position passed to _VectorAt: is out of bounds (position: {d}, size: {d})", .{ position, vector_values.len });
    }

    return vector_values[@intCast(u64, position)];
}

/// Place the object in the second argument to the integer position in the first
/// argument. If the given position is out of bounds, an error is raised.
/// Returns the receiver.
pub fn VectorAt_Put(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) !Value {
    _ = heap;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    const position_value = arguments[0].getValue();
    const new_value = arguments[1].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
        return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorAt:Put:", .{});
    }

    if (!position_value.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as the first argument of _VectorAt:Put:", .{});
    }

    const position = position_value.asInteger();
    const vector_values = receiver.asObject().asVectorObject().getValues();
    if (position < 0 or position >= vector_values.len) {
        return runtime_error.raiseError(allocator, context, "Position passed to _VectorAt:Put: is out of bounds (position: {d}, size: {d})", .{ position, vector_values.len });
    }

    vector_values[@intCast(u64, position)] = new_value;
    // Since the vector object could potentially be in an older space than the
    // value stored in it, let's add the vector object to the remembered set.
    try heap.rememberObjectReference(receiver, new_value);

    return tracked_receiver.getValue();
}
