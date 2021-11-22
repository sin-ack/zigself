// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Print the given ByteVector to stdout, followed by a newline.
pub fn StringPrint(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = arguments;

    const writer = std.io.getStdOut().writer();

    // FIXME: Don't ignore errors.
    switch (receiver.value.content) {
        .ByteVector => |byte_vector| {
            writer.print("{s}", .{byte_vector.values}) catch unreachable;
        },

        .Integer => |integer| {
            writer.print("{d}", .{integer.value}) catch unreachable;
        },

        .FloatingPoint => |floating_point| {
            writer.print("{d}", .{floating_point.value}) catch unreachable;
        },

        else => {
            return runtime_error.raiseError(allocator, context, "Unexpected object type {s} passed as the receiver of _StringPrint", .{@tagName(receiver.value.content)});
        },
    }

    receiver.unref();

    return environment.globalNil();
}

/// Return the size of the byte vector in bytes.
pub fn ByteVectorSize(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = arguments;

    if (!receiver.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector as _ByteVectorSize receiver, got {s}", .{@tagName(receiver.value.content)});
    }
    defer receiver.unref();

    return Object.createFromIntegerLiteral(allocator, receiver.value.content.ByteVector.values.len);
}
