// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");

const basic_primitives = @import("./basic.zig");

/// Print the given ByteVector to stdout, followed by a newline.
pub fn PrintLine(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, lobby: Object.Ref) !Object.Ref {
    _ = arguments;

    const writer = std.io.getStdOut().writer();

    // FIXME: Don't ignore errors.
    switch (receiver.value.content) {
        .ByteVector => |byte_vector| {
            writer.print("{s}\n", .{byte_vector.values}) catch unreachable;
        },

        .Integer => |integer| {
            writer.print("{d}\n", .{integer.value}) catch unreachable;
        },

        .FloatingPoint => |floating_point| {
            writer.print("{d}\n", .{floating_point.value}) catch unreachable;
        },

        else => {
            std.debug.panic("Unexpected object type {s} passed as the receiver of _PrintLine", .{@tagName(receiver.value.content)});
        },
    }

    receiver.unref();

    lobby.ref();
    return basic_primitives.Nil(allocator, lobby, &[_]Object.Ref{}, lobby);
}
