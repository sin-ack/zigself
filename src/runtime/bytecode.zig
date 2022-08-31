// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

pub const instruction = @import("./bytecode/instruction.zig");
pub const block = @import("./bytecode/block.zig");
pub const executable = @import("./bytecode/executable.zig");

pub const astcode = @import("./bytecode/astcode.zig");
pub const lowcode = @import("./bytecode/lowcode.zig");

pub const AstcodeInstruction = instruction.AstcodeInstruction;
pub const LowcodeInstruction = instruction.LowcodeInstruction;
pub const AstcodeBlock = block.AstcodeBlock;
pub const LowcodeBlock = block.LowcodeBlock;
pub const AstcodeExecutable = executable.AstcodeExecutable;
pub const LowcodeExecutable = executable.LowcodeExecutable;

// These are the ones that the rest of the codebase intends to use, so we pull
// it as the default here.
pub const Instruction = LowcodeInstruction;
pub const Block = LowcodeBlock;
pub const Executable = LowcodeExecutable;
pub const RegisterLocation = lowcode.RegisterLocation;
