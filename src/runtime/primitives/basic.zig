// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");

var static_nil: ?Object.Ref = null;

/// Return the static "nil" slots object.
pub fn Nil(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, lobby: Object.Ref) !Object.Ref {
    _ = lobby;
    _ = arguments;
    receiver.unref();

    if (static_nil) |*nil| {
        nil.ref();
        return nil.*;
    } else {
        static_nil = try Object.createEmpty(allocator);

        static_nil.?.ref();
        return static_nil.?;
    }
}

/// Deinitialize global allocations done by primitives.
pub fn deinitPrimitives(allocator: *Allocator) void {
    _ = allocator;

    if (static_nil) |*nil| {
        nil.unref();
    }
}
