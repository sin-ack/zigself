// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Range = @import("../../language/location_range.zig");
const Object = @import("../object.zig");
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
    allocator: *Allocator,
    message_range: Range,
    receiver: Object.Ref,
    arguments: []Object.Ref,
    context: *InterpreterContext,
) !Object.Ref {
    _ = message_range;

    defer receiver.unrefWithAllocator(allocator);

    const size_object = arguments[0];
    defer size_object.unrefWithAllocator(allocator);

    const filler = arguments[1];
    defer filler.unrefWithAllocator(allocator);

    if (!size_object.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as the first argument of _VectorCopySize:FillingExtrasWith:, got {s}", .{@tagName(size_object.value.content)});
    }

    const size = size_object.value.content.Integer.value;
    if (size == 0) {
        return Object.createEmptyVector(allocator);
    } else if (size < 0) {
        return runtime_error.raiseError(allocator, context, "First argument of _VectorCopySize:FillingExtrasWith: must be positive", .{});
    } else {
        if (!receiver.value.is(.Vector)) {
            return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorCopySize:FillingExtrasWith:, got {s}", .{@tagName(receiver.value.content)});
        }

        var values = try allocator.alloc(Object.Ref, @intCast(usize, size));
        errdefer allocator.free(values);

        const receiver_values = receiver.value.content.Vector.values;
        for (values) |*v, i| {
            if (i >= receiver_values.len) {
                filler.ref();
                v.* = filler;
            } else {
                const receiver_value = receiver_values[i];
                receiver_value.ref();
                v.* = receiver_value;
            }
        }
        errdefer for (values) |v| v.unrefWithAllocator(allocator);

        return Object.createVectorFromValues(allocator, values);
    }
}

/// Return the size of the receiver vector.
pub fn VectorSize(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;
    _ = arguments;

    defer receiver.unrefWithAllocator(allocator);

    if (!receiver.value.is(.Vector)) {
        return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorSize, got {s}", .{@tagName(receiver.value.content)});
    }

    const values = receiver.value.content.Vector.values;
    return Object.createFromIntegerLiteral(allocator, @intCast(i64, values.len));
}

/// Return the value at the given position of the receiver vector. If the given
/// position is out of bounds, an error is raised.
pub fn VectorAt(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    defer receiver.unrefWithAllocator(allocator);

    const position_object = arguments[0];
    defer position_object.unrefWithAllocator(allocator);

    if (!receiver.value.is(.Vector)) {
        return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorAt:, got {s}", .{@tagName(receiver.value.content)});
    }

    if (!position_object.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as the first argument of _VectorAt:, got {s}", .{@tagName(position_object.value.content)});
    }

    const position = position_object.value.content.Integer.value;
    const values = receiver.value.content.Vector.values;
    if (position < 0 or position >= values.len) {
        return runtime_error.raiseError(allocator, context, "Position passed to _VectorAt: is out of bounds (position: {d}, size: {d})", .{ position, values.len });
    }

    const value = values[@intCast(usize, position)];
    value.ref();
    return value;
}

/// Place the object in the second argument to the integer position in the first
/// argument. If the given position is out of bounds, an error is raised.
/// Returns the receiver.
pub fn VectorAt_Put(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    errdefer receiver.unrefWithAllocator(allocator);

    const position_object = arguments[0];
    defer position_object.unrefWithAllocator(allocator);

    const value = arguments[1];
    errdefer value.unrefWithAllocator(allocator);

    if (!receiver.value.is(.Vector)) {
        return runtime_error.raiseError(allocator, context, "Expected Vector as the receiver of _VectorAt:Put:, got {s}", .{@tagName(receiver.value.content)});
    }

    if (!position_object.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer as the first argument of _VectorAt:Put:, got {s}", .{@tagName(position_object.value.content)});
    }

    const position = position_object.value.content.Integer.value;
    const values = receiver.value.content.Vector.values;
    if (position < 0 or position >= values.len) {
        return runtime_error.raiseError(allocator, context, "Position passed to _VectorAt:Put: is out of bounds (position: {d}, size: {d})", .{ position, values.len });
    }

    values[@intCast(usize, position)] = value;

    return receiver;
}
