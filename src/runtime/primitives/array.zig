// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Completion = @import("../completion.zig");
const ByteArray = @import("../byte_array.zig");
const SourceRange = @import("../../language/source_range.zig");
const environment = @import("../environment.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Copy the receiver array and create a new one with the size given as the
/// first argument, and if the newly created array has more items than the
/// receiver, fill the extra spaces with the second argument.
///
/// As a special case, if the first argument is 0, then a new empty array is
/// created without looking at the receiver.
pub fn ArrayCopySize_FillingExtrasWith(
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
        return Completion.initRuntimeError(allocator, source_range, "Expected Integer as the first argument of _ArrayCopySize:FillingExtrasWith:", .{});
    }

    const size = size_value.asInteger();
    if (size < 0) {
        return Completion.initRuntimeError(allocator, source_range, "First argument of _ArrayCopySize:FillingExtrasWith: must be positive", .{});
    }

    const required_memory = Object.Map.Array.requiredSizeForAllocation() + Object.Array.requiredSizeForAllocation(@intCast(u64, size));
    try heap.ensureSpaceInEden(required_memory);

    if (size == 0) {
        const array_map = try Object.Map.Array.create(heap, 0);
        const array = try Object.Array.createWithValues(heap, array_map, &[_]Value{}, null);
        return Completion.initNormal(array.asValue());
    } else {
        var receiver = tracked_receiver.getValue();
        if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
            return Completion.initRuntimeError(allocator, source_range, "Expected Array as the receiver of _ArrayCopySize:FillingExtrasWith:", .{});
        }

        const filler = arguments[1].getValue();

        // FIXME: This first fills up the new map with the filler and then overwrites the
        //        values which we do have. This is too much work, let's just fill the space
        //        that's actually extra.
        const new_array_map = try Object.Map.Array.create(heap, @intCast(u64, size));
        const new_array = try Object.Array.createWithValues(heap, new_array_map, receiver.asObject().asArrayObject().getValues(), filler);
        return Completion.initNormal(new_array.asValue());
    }
}

/// Return the size of the receiver array.
pub fn ArraySize(
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
    if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Array as the receiver of _ArraySize", .{});
    }

    return Completion.initNormal(Value.fromUnsignedInteger(receiver.asObject().asArrayObject().getSize()));
}

/// Return the value at the given position of the receiver array. If the given
/// position is out of bounds, an error is raised.
pub fn ArrayAt(
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

    if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Array as the receiver of _ArrayAt:", .{});
    }

    if (!position_value.isInteger()) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Integer as the first argument of _ArrayAt:", .{});
    }

    const position = position_value.asInteger();
    const array_values = receiver.asObject().asArrayObject().getValues();
    if (position < 0 or position >= array_values.len) {
        return Completion.initRuntimeError(allocator, source_range, "Position passed to _ArrayAt: is out of bounds (position: {d}, size: {d})", .{ position, array_values.len });
    }

    return Completion.initNormal(array_values[@intCast(u64, position)]);
}

/// Place the object in the second argument to the integer position in the first
/// argument. If the given position is out of bounds, an error is raised.
/// Returns the receiver.
pub fn ArrayAt_Put(
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

    if (!(receiver.isObjectReference() and receiver.asObject().isArrayObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Array as the receiver of _ArrayAt:Put:", .{});
    }

    if (!position_value.isInteger()) {
        return Completion.initRuntimeError(allocator, source_range, "Expected Integer as the first argument of _ArrayAt:Put:", .{});
    }

    const position = position_value.asInteger();
    const array_values = receiver.asObject().asArrayObject().getValues();
    if (position < 0 or position >= array_values.len) {
        return Completion.initRuntimeError(allocator, source_range, "Position passed to _ArrayAt:Put: is out of bounds (position: {d}, size: {d})", .{ position, array_values.len });
    }

    array_values[@intCast(u64, position)] = new_value;
    // Since the array object could potentially be in an older space than the
    // value stored in it, let's add the array object to the remembered set.
    try heap.rememberObjectReference(receiver, new_value);

    return Completion.initNormal(tracked_receiver.getValue());
}
