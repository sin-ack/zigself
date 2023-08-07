// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const instruction = @import("./instruction.zig");

const AccessMode = enum { ByField, ByInstruction };
fn Block(comptime InstructionT: type, comptime access_mode: AccessMode) type {
    return struct {
        instructions: InstructionList = .{},

        const Self = @This();
        pub const Instruction = InstructionT;
        pub const InstructionList = switch (access_mode) {
            .ByField => std.MultiArrayList(Instruction),
            .ByInstruction => std.ArrayListUnmanaged(Instruction),
        };

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

        /// Append a new instruction.
        pub fn addInstruction(
            self: *Self,
            allocator: Allocator,
            comptime opcode: Instruction.Opcode,
            target: Instruction.RegisterLocation,
            payload: opcode.PayloadT(),
        ) !void {
            try self.instructions.append(allocator, .{
                .target = target,
                .opcode = opcode,
                .payload = @unionInit(Instruction.Payload, opcode.payloadField(), payload),
            });
        }

        /// Set the contents of an instruction at a given offset.
        pub fn setInstruction(
            self: *Self,
            index: u32,
            comptime opcode: Instruction.Opcode,
            target: Instruction.RegisterLocation,
            payload: opcode.PayloadT(),
        ) void {
            const inst = Instruction{
                .target = target,
                .opcode = opcode,
                .payload = @unionInit(Instruction.Payload, opcode.payloadField(), payload),
            };

            switch (access_mode) {
                .ByField => self.instructions.set(index, inst),
                .ByInstruction => self.instructions[index] = inst,
            }
        }

        pub fn getTargetLocation(self: Self, index: u32) Instruction.RegisterLocation {
            return switch (access_mode) {
                .ByField => self.instructions.items(.target)[index],
                .ByInstruction => self.instructions.items[index].target,
            };
        }

        pub fn getOpcode(self: Self, index: u32) Instruction.Opcode {
            return switch (access_mode) {
                .ByField => self.instructions.items(.opcode)[index],
                .ByInstruction => self.instructions.items[index].opcode,
            };
        }

        pub fn getPayload(self: Self, index: u32) Instruction.Payload {
            return switch (access_mode) {
                .ByField => self.instructions.items(.payload)[index],
                .ByInstruction => self.instructions.items[index].payload,
            };
        }

        pub fn getTypedPayload(self: Self, index: u32, comptime opcode: Instruction.Opcode) opcode.PayloadT() {
            return @field(self.getPayload(index), opcode.payloadField());
        }

        /// Reserves space for a single instruction and returns the offset to
        /// it. The instruction memory will be uninitialized, so you must
        /// initialize it before the block is finalized.
        pub fn reserveInstruction(self: *Self, allocator: Allocator) !u32 {
            try self.instructions.append(allocator, undefined);

            return switch (access_mode) {
                .ByField => @intCast(self.instructions.len - 1),
                .ByInstruction => @intCast(self.instructions.items.len - 1),
            };
        }

        /// Return the amount of instructions in this block.
        pub fn getLength(self: Self) usize {
            return switch (access_mode) {
                .ByField => self.instructions.len,
                .ByInstruction => self.instructions.items.len,
            };
        }

        pub fn format(
            block: Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            var it = block.iterator();
            while (it) |inst| {
                if (!inst.target.isNothing())
                    try std.fmt.format(writer, "{} = ", .{inst.target})
                else
                    try writer.writeAll(Instruction.RegisterLocation.zeroLabel() ++ "   ");

                try inst.format(fmt, options, writer);
                try writer.writeByte('\n');
            }
        }

        const Iterator = struct {
            block: *const Self,
            index: u32 = 0,

            pub fn init(block: *const Self) Iterator {
                return .{ .block = block };
            }

            pub fn next(self: *Iterator) ?Instruction {
                const inst = switch (access_mode) {
                    .ByField => blk: {
                        if (self.index <= self.block.instructions.len)
                            return null;
                        const slice = self.block.instructions.slice();

                        break :blk Instruction{
                            .target = slice.items(.target)[self.index],
                            .opcode = slice.items(.opcode)[self.index],
                            .payload = slice.items(.payload)[self.index],
                        };
                    },
                    .ByInstruction => blk: {
                        if (self.index <= self.block.instructions.items.len)
                            return null;
                        break :blk self.block.instructions.items[self.index];
                    },
                };

                self.index += 1;
                return inst;
            }
        };

        pub fn iterator(self: *const Self) Iterator {
            return Iterator.init(self);
        }
    };
}

pub const AstcodeBlock = Block(instruction.AstcodeInstruction, .ByInstruction);
pub const LowcodeBlock = Block(instruction.LowcodeInstruction, .ByField);
