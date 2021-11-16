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

    if (receiver.value.content != .ByteVector) {
        std.debug.panic("Expected a byte vector as the receiver for _PrintLine, got {s}\n", .{@tagName(receiver.value.content)});
    }

    const writer = std.io.getStdOut().writer();
    // FIXME: Don't ignore this error.
    writer.print("{s}\n", .{receiver.value.content.ByteVector.values}) catch unreachable;

    receiver.unref();

    lobby.ref();
    return basic_primitives.Nil(allocator, lobby, &[_]Object.Ref{}, lobby);
}
