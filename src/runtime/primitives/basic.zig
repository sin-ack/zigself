// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");
const environment = @import("../environment.zig");

/// Return the static "nil" slots object.
pub fn Nil(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, lobby: Object.Ref) !Object.Ref {
    _ = allocator;
    _ = lobby;
    _ = arguments;
    receiver.unref();

    return environment.globalNil();
}
