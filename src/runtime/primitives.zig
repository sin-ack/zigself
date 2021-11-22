// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("./object.zig");
const InterpreterContext = @import("./interpreter.zig").InterpreterContext;
const runtime_error = @import("./error.zig");

const basic_primitives = @import("./primitives/basic.zig");
const bytevector_primitives = @import("./primitives/bytevector.zig");
const number_primitives = @import("./primitives/number.zig");
const object_primitives = @import("./primitives/object.zig");

/// The list of errors allowed to be raised by primitives.
const PrimitiveError = Allocator.Error || runtime_error.SelfRuntimeError;

/// A primitive specification. The `name` field specifies the exact selector the
/// primitive uses (i.e. `_DoFoo:WithBar:`, or `_StringPrint`), and the
/// `function` is the function which is called to execute the primitive.
const PrimitiveSpec = struct {
    name: []const u8,
    function: fn (allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) PrimitiveError!Object.Ref,
};

const PrimitiveRegistry = &[_]PrimitiveSpec{
    // basic primitives
    .{ .name = "_Nil", .function = basic_primitives.Nil },
    .{ .name = "_Exit:", .function = basic_primitives.Exit },
    .{ .name = "_RunScript", .function = basic_primitives.RunScript },
    // byte vector primitives
    .{ .name = "_StringPrint", .function = bytevector_primitives.StringPrint },
    // number primitives
    .{ .name = "_IntAdd:", .function = number_primitives.IntAdd },
    .{ .name = "_IntSub:", .function = number_primitives.IntSub },
    .{ .name = "_IntLT:", .function = number_primitives.IntLT },
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

pub fn callPrimitive(allocator: *Allocator, selector: []const u8, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    for (PrimitiveRegistry) |primitive| {
        if (std.mem.eql(u8, primitive.name, selector)) {
            return try primitive.function(allocator, receiver, arguments, context);
        }
    }

    return runtime_error.raiseError(allocator, context, "Unknown primitive \"{s}\" called", .{selector});
}
