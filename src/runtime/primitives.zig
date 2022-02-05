// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Range = @import("../language/location_range.zig");
const Object = @import("./object.zig");
const runtime_error = @import("./error.zig");
const interpreter = @import("./interpreter.zig");
const InterpreterContext = interpreter.InterpreterContext;

const basic_primitives = @import("./primitives/basic.zig");
const bytevector_primitives = @import("./primitives/bytevector.zig");
const vector_primitives = @import("./primitives/vector.zig");
const number_primitives = @import("./primitives/number.zig");
const object_primitives = @import("./primitives/object.zig");

/// A primitive specification. The `name` field specifies the exact selector the
/// primitive uses (i.e. `_DoFoo:WithBar:`, or `_StringPrint`), and the
/// `function` is the function which is called to execute the primitive.
const PrimitiveSpec = struct {
    name: []const u8,
    function: fn (
        allocator: Allocator,
        heap: *Heap,
        message_range: Range,
        receiver: Heap.Tracked,
        arguments: []Heap.Tracked,
        context: *InterpreterContext,
    ) interpreter.InterpreterError!Value,
};

const PrimitiveRegistry = &[_]PrimitiveSpec{
    // basic primitives
    .{ .name = "_Nil", .function = basic_primitives.Nil },
    .{ .name = "_Exit:", .function = basic_primitives.Exit },
    .{ .name = "_RunScript", .function = basic_primitives.RunScript },
    .{ .name = "_Error:", .function = basic_primitives.Error },
    .{ .name = "_Restart", .function = basic_primitives.Restart },
    // byte vector primitives
    .{ .name = "_StringPrint", .function = bytevector_primitives.StringPrint },
    .{ .name = "_ByteVectorSize", .function = bytevector_primitives.ByteVectorSize },
    .{ .name = "_ByteAt:", .function = bytevector_primitives.ByteAt },
    .{ .name = "_ByteAt:Put:", .function = bytevector_primitives.ByteAt_Put },
    .{ .name = "_ByteVectorCopySize:", .function = bytevector_primitives.ByteVectorCopySize },
    // vector primitives
    .{ .name = "_VectorCopySize:FillingExtrasWith:", .function = vector_primitives.VectorCopySize_FillingExtrasWith },
    .{ .name = "_VectorSize", .function = vector_primitives.VectorSize },
    .{ .name = "_VectorAt:", .function = vector_primitives.VectorAt },
    .{ .name = "_VectorAt:Put:", .function = vector_primitives.VectorAt_Put },
    // number primitives
    .{ .name = "_IntAdd:", .function = number_primitives.IntAdd },
    .{ .name = "_IntSub:", .function = number_primitives.IntSub },
    .{ .name = "_IntMul:", .function = number_primitives.IntMul },
    .{ .name = "_IntLT:", .function = number_primitives.IntLT },
    .{ .name = "_IntEq:", .function = number_primitives.IntEq },
    .{ .name = "_IntGT:", .function = number_primitives.IntGT },
    // object primitives
    .{ .name = "_AddSlots:", .function = object_primitives.AddSlots },
    // .{ .name = "_RemoveSlot:IfFail:", .function = object_primitives.RemoveSlot_IfFail },
    // .{ .name = "_Inspect", .function = object_primitives.Inspect },
    .{ .name = "_Clone", .function = object_primitives.Clone },
    .{ .name = "_Eq:", .function = object_primitives.Eq },
};

// FIXME: This is very naive! We shouldn't need to linear search every single
//        time.
pub fn hasPrimitive(selector: []const u8) bool {
    for (PrimitiveRegistry) |primitive| {
        if (std.mem.eql(u8, primitive.name, selector)) {
            return true;
        }
    }

    return false;
}

pub fn callPrimitive(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    selector: []const u8,
    receiver: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) !Value {
    for (PrimitiveRegistry) |primitive| {
        if (std.mem.eql(u8, primitive.name, selector)) {
            return try primitive.function(allocator, heap, message_range, receiver, arguments, context);
        }
    }

    return runtime_error.raiseError(allocator, context, "Unknown primitive \"{s}\" called", .{selector});
}
