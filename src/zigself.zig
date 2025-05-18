// Copyright (c) 2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0

//! zigSelf is an implementation of the Self programming language in Zig.
//! Below is the (currently-unstable) public API surface.
//!
//! See src/main.zig for an example of how to use the zigSelf virtual machine.

const std = @import("std");

pub const language = @import("language.zig");
pub const runtime = @import("runtime.zig");

test {
    // TODO: Make it possible to refAllDeclsRecursive
    std.testing.refAllDecls(language);
    std.testing.refAllDecls(runtime);
}
