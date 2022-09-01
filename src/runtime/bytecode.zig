// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

pub const instruction = @import("./bytecode/instruction.zig");
pub const block = @import("./bytecode/block.zig");
pub const executable = @import("./bytecode/executable.zig");

pub const astcode = @import("./bytecode/astcode.zig");
pub const lowcode = @import("./bytecode/lowcode.zig");

// These are the ones that the rest of the codebase intends to use, so we pull
// it as the default here.
pub const Instruction = lowcode.Instruction;
pub const Block = lowcode.Block;
pub const Executable = lowcode.Executable;
pub const RegisterLocation = lowcode.RegisterLocation;
