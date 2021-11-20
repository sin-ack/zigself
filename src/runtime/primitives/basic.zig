// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");
const environment = @import("../environment.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Return the static "nil" slots object.
pub fn Nil(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = allocator;
    _ = context;
    _ = arguments;
    receiver.unref();

    return environment.globalNil();
}
