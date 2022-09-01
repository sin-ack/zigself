// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

pub const RegisterLocation = @import("./astcode/register_location.zig").RegisterLocation;
pub const Liveness = @import("./astcode/Liveness.zig");
const instruction = @import("./instruction.zig");
const block = @import("./block.zig");
const executable = @import("./executable.zig");

pub const Instruction = instruction.AstcodeInstruction;
pub const Block = block.AstcodeBlock;
pub const Executable = executable.AstcodeExecutable;
