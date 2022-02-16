// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Completion = @import("../completion.zig");
const ByteVector = @import("../byte_vector.zig");
const SourceRange = @import("../../language/source_range.zig");
const environment = @import("../environment.zig");
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
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    source_range: SourceRange,
    context: *InterpreterContext,
) !Completion {
    _ = tracked_receiver;
    _ = context;

    const size_value = arguments[0].getValue();
    if (!size_value.isInteger()) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Integer as the first argument of _VectorCopySize:FillingExtrasWith:", .{});
    }

    const size = size_value.asInteger();
    if (size < 0) {
        return Completion.initRuntimeError(allocator, source_range, "First argument of _VectorCopySize:FillingExtrasWith: must be positive", .{});
    }

    const required_memory = Object.Map.Vector.requiredSizeForAllocation() + Object.Vector.requiredSizeForAllocation(@intCast(u64, size));
    try heap.ensureSpaceInEden(required_memory);

    if (size == 0) {
        const vector_map = try Object.Map.Vector.create(heap, 0);
        const vector = try Object.Vector.createWithValues(heap, vector_map, &[_]Value{}, null);
        return Completion.initNormal(vector.asValue());
    } else {
        var receiver = tracked_receiver.getValue();
        if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
            return Completion.initRuntimeError(allocator, source_range, "Expected Vector as the receiver of _VectorCopySize:FillingExtrasWith:", .{});
        }

        const filler = arguments[1].getValue();

        // FIXME: This first fills up the new map with the filler and then overwrites the
        //        values which we do have. This is too much work, let's just fill the space
        //        that's actually extra.
        const new_vector_map = try Object.Map.Vector.create(heap, @intCast(u64, size));
        const new_vector = try Object.Vector.createWithValues(heap, new_vector_map, receiver.asObject().asVectorObject().getValues(), filler);
        return Completion.initNormal(new_vector.asValue());
    }
}

/// Return the size of the receiver vector.
pub fn VectorSize(
    allocator: Allocator,
    heap: *Heap,
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    source_range: SourceRange,
    context: *InterpreterContext,
) !Completion {
    _ = allocator;
    _ = heap;
    _ = arguments;
    _ = context;

    const receiver = tracked_receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Vector as the receiver of _VectorSize", .{});
    }

    return Completion.initNormal(Value.fromUnsignedInteger(receiver.asObject().asVectorObject().getSize()));
}

/// Return the value at the given position of the receiver vector. If the given
/// position is out of bounds, an error is raised.
pub fn VectorAt(
    allocator: Allocator,
    heap: *Heap,
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    source_range: SourceRange,
    context: *InterpreterContext,
) !Completion {
    _ = heap;
    _ = context;

    const receiver = tracked_receiver.getValue();
    const position_value = arguments[0].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Vector as the receiver of _VectorAt:", .{});
    }

    if (!position_value.isInteger()) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Integer as the first argument of _VectorAt:", .{});
    }

    const position = position_value.asInteger();
    const vector_values = receiver.asObject().asVectorObject().getValues();
    if (position < 0 or position >= vector_values.len) {
        return Completion.initRuntimeError(allocator, source_range, "Position passed to _VectorAt: is out of bounds (position: {d}, size: {d})", .{ position, vector_values.len });
    }

    return Completion.initNormal(vector_values[@intCast(u64, position)]);
}

/// Place the object in the second argument to the integer position in the first
/// argument. If the given position is out of bounds, an error is raised.
/// Returns the receiver.
pub fn VectorAt_Put(
    allocator: Allocator,
    heap: *Heap,
    tracked_receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    source_range: SourceRange,
    context: *InterpreterContext,
) !Completion {
    _ = heap;
    _ = context;

    const receiver = tracked_receiver.getValue();
    const position_value = arguments[0].getValue();
    const new_value = arguments[1].getValue();

    if (!(receiver.isObjectReference() and receiver.asObject().isVectorObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Vector as the receiver of _VectorAt:Put:", .{});
    }

    if (!position_value.isInteger()) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Integer as the first argument of _VectorAt:Put:", .{});
    }

    const position = position_value.asInteger();
    const vector_values = receiver.asObject().asVectorObject().getValues();
    if (position < 0 or position >= vector_values.len) {
        return Completion.initRuntimeError(allocator, source_range, "Position passed to _VectorAt:Put: is out of bounds (position: {d}, size: {d})", .{ position, vector_values.len });
    }

    vector_values[@intCast(u64, position)] = new_value;
    // Since the vector object could potentially be in an older space than the
    // value stored in it, let's add the vector object to the remembered set.
    try heap.rememberObjectReference(receiver, new_value);

    return Completion.initNormal(tracked_receiver.getValue());
}
