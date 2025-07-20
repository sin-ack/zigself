// Copyright (c) 2024-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0

//! zigSelf's runtime facilities, including the virtual machine,
//! bytecode compiler, garbage collector, and primitives.

const std = @import("std");

pub const Activation = @import("runtime/Activation.zig");
pub const Actor = @import("runtime/Actor.zig");
pub const ByteArray = @import("runtime/ByteArray.zig");
pub const Heap = @import("runtime/Heap.zig");
pub const SourceRange = @import("runtime/SourceRange.zig");
pub const VirtualMachine = @import("runtime/VirtualMachine.zig");
pub const bytecode = @import("runtime/bytecode.zig");
pub const Interpreter = @import("runtime/Interpreter.zig");
pub const map = @import("runtime/map.zig");
pub const object = @import("runtime/object.zig");
pub const value = @import("runtime/value.zig");
pub const primitives = @import("runtime/primitives.zig");

test {
    std.testing.refAllDeclsRecursive(Heap);
}
