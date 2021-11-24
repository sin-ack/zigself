// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Range = @import("../language/location_range.zig");
const Object = @import("./object.zig");
const runtime_error = @import("./error.zig");
const interpreter = @import("./interpreter.zig");
const InterpreterContext = interpreter.InterpreterContext;

const basic_primitives = @import("./primitives/basic.zig");
const bytevector_primitives = @import("./primitives/bytevector.zig");
const number_primitives = @import("./primitives/number.zig");
const object_primitives = @import("./primitives/object.zig");

/// A primitive specification. The `name` field specifies the exact selector the
/// primitive uses (i.e. `_DoFoo:WithBar:`, or `_StringPrint`), and the
/// `function` is the function which is called to execute the primitive.
const PrimitiveSpec = struct {
    name: []const u8,
    function: fn (
        allocator: *Allocator,
        message_range: Range,
        receiver: Object.Ref,
        arguments: []Object.Ref,
        context: *InterpreterContext,
    ) interpreter.InterpreterError!Object.Ref,
};

const PrimitiveRegistry = &[_]PrimitiveSpec{
    // basic primitives
    .{ .name = "_Nil", .function = basic_primitives.Nil },
    .{ .name = "_Exit:", .function = basic_primitives.Exit },
    .{ .name = "_RunScript", .function = basic_primitives.RunScript },
    .{ .name = "_ID", .function = basic_primitives.ID },
    // byte vector primitives
    .{ .name = "_StringPrint", .function = bytevector_primitives.StringPrint },
    .{ .name = "_ByteVectorSize", .function = bytevector_primitives.ByteVectorSize },
    .{ .name = "_ByteAt:", .function = bytevector_primitives.ByteAt },
    // number primitives
    .{ .name = "_IntAdd:", .function = number_primitives.IntAdd },
    .{ .name = "_IntSub:", .function = number_primitives.IntSub },
    .{ .name = "_IntLT:", .function = number_primitives.IntLT },
    .{ .name = "_IntEq:", .function = number_primitives.IntEq },
    // object primitives
    .{ .name = "_AddSlots:", .function = object_primitives.AddSlots },
    .{ .name = "_RemoveSlot:IfFail:", .function = object_primitives.RemoveSlot_IfFail },
    .{ .name = "_Inspect", .function = object_primitives.Inspect },
    .{ .name = "_Clone", .function = object_primitives.Clone },
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
    allocator: *Allocator,
    message_range: Range,
    selector: []const u8,
    receiver: Object.Ref,
    arguments: []Object.Ref,
    context: *InterpreterContext,
) !Object.Ref {
    for (PrimitiveRegistry) |primitive| {
        if (std.mem.eql(u8, primitive.name, selector)) {
            return try primitive.function(allocator, message_range, receiver, arguments, context);
        }
    }

    return runtime_error.raiseError(allocator, context, "Unknown primitive \"{s}\" called", .{selector});
}
