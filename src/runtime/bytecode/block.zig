// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const instruction = @import("./instruction.zig");

fn Block(comptime InstructionT: type) type {
    return struct {
        instructions: std.ArrayListUnmanaged(Instruction) = .{},

        const Self = @This();
        pub const Instruction = InstructionT;

        pub fn create(allocator: Allocator) !*Self {
            const self = try allocator.create(Self);
            errdefer allocator.destroy(self);

            self.init();
            return self;
        }

        fn init(self: *Self) void {
            self.* = .{};
        }

        fn deinit(self: *Self, allocator: Allocator) void {
            self.instructions.deinit(allocator);
        }

        pub fn destroy(self: *Self, allocator: Allocator) void {
            self.deinit(allocator);
            allocator.destroy(self);
        }

        pub fn getInstruction(self: *Self, index: u32) *Instruction {
            return &self.instructions.items[index];
        }

        pub fn addInstruction(self: *Self, allocator: Allocator, inst: Instruction) !void {
            try self.instructions.append(allocator, inst);
        }

        /// Reserves space for a single instruction and returns the offset to
        /// it. The instruction memory will be uninitialized, so you must
        /// initialize it before the block is finalized.
        pub fn reserveInstruction(self: *Self, allocator: Allocator) !usize {
            try self.instructions.append(allocator, undefined);
            return self.instructions.items.len - 1;
        }

        pub fn format(
            block: Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            for (block.instructions.items) |inst| {
                if (!inst.target.isNothing())
                    try std.fmt.format(writer, "{} = ", .{inst.target})
                else
                    try writer.writeAll(Instruction.RegisterLocation.zeroLabel() ++ "   ");

                try inst.format(fmt, options, writer);
                try writer.writeByte('\n');
            }
        }
    };
}

pub const AstcodeBlock = Block(instruction.AstcodeInstruction);
pub const LowcodeBlock = Block(instruction.LowcodeInstruction);
