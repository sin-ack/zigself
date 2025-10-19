// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

pub const instruction = @import("./bytecode/instruction.zig");
pub const block = @import("./bytecode/block.zig");
pub const executable = @import("./bytecode/executable.zig");
pub const ObjectDescriptor = @import("./bytecode/ObjectDescriptor.zig");

pub const astcode = @import("./bytecode/astcode.zig");
pub const lowcode = @import("./bytecode/lowcode.zig");

/// Index of a local slot within an activation record.
pub const LocalIndex = enum(u8) {
    Receiver = 0,
    _,

    /// Initialize from a raw index value.
    pub fn initIndex(index: u8) LocalIndex {
        return @enumFromInt(index + 1);
    }

    pub fn get(self: LocalIndex) u8 {
        return @intFromEnum(self);
    }

    pub fn format(self: LocalIndex, writer: *std.Io.Writer) !void {
        try writer.writeByte('L');
        try writer.printInt(self.get(), 10, .lower, .{});
    }
};

// These are the ones that the rest of the codebase intends to use, so we pull
// it as the default here.
pub const Instruction = lowcode.Instruction;
pub const Block = lowcode.Block;
pub const Executable = lowcode.Executable;
pub const RegisterLocation = lowcode.RegisterLocation;
