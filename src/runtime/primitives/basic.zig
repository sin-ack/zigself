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

/// Exit with the given return code.
pub fn Exit(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = allocator;
    _ = receiver;
    _ = context;

    var status_code = arguments[0];
    if (!status_code.value.is(.Integer)) {
        std.debug.panic("Expected Integer for the status code argument of _Exit:, got {s}", .{@tagName(status_code.value.content)});
    }

    std.os.exit(@intCast(u8, status_code.value.content.Integer.value));
}
