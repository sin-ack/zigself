// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const instruction = @import("./instruction.zig");
const Range = @import("../../language/Range.zig");

const AccessMode = enum { ByField, ByInstruction };
fn Block(comptime InstructionT: type, comptime access_mode: AccessMode) type {
    return struct {
        instructions: InstructionList = .{},
        sealed: if (builtin.mode == .Debug) bool else void = if (builtin.mode == .Debug) false else {},

        const Self = @This();
        pub const Instruction = InstructionT;
        pub const InstructionList = switch (access_mode) {
            .ByField => struct {
                multi_array: std.MultiArrayList(Instruction) = .{},

                // Individual array pointers. Initialized with seal().
                targets: []InstructionT.RegisterLocation = undefined,
                opcodes: []InstructionT.Opcode = undefined,
                payloads: []InstructionT.Payload = undefined,
                source_ranges: []Range = undefined,
            },
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
            switch (access_mode) {
                .ByField => self.instructions.multi_array.deinit(allocator),
                .ByInstruction => self.instructions.deinit(allocator),
            }
        }

        pub fn destroy(self: *Self, allocator: Allocator) void {
            self.deinit(allocator);
            allocator.destroy(self);
        }

        pub fn seal(self: *Self) void {
            if (builtin.mode == .Debug) {
                if (self.sealed) {
                    @panic("!!! Attempting to re-seal a block!");
                }

                self.sealed = true;
            }

            if (access_mode == .ByField) {
                // Initialize the arrays
                self.instructions.targets = self.instructions.multi_array.items(.target);
                self.instructions.opcodes = self.instructions.multi_array.items(.opcode);
                self.instructions.payloads = self.instructions.multi_array.items(.payload);
                self.instructions.source_ranges = self.instructions.multi_array.items(.source_range);
            }
        }

        /// Append a new instruction.
        pub fn addInstruction(
            self: *Self,
            allocator: Allocator,
            comptime opcode: Instruction.Opcode,
            target: Instruction.RegisterLocation,
            payload: opcode.PayloadT(),
            source_range: Range,
        ) !void {
            if (builtin.mode == .Debug and self.sealed) {
                @panic("!!! Attempting to add an instruction after the block was sealed!");
            }

            const inst = InstructionT{
                .target = target,
                .opcode = opcode,
                .payload = @unionInit(Instruction.Payload, opcode.payloadField(), payload),
                .source_range = source_range,
            };

            switch (access_mode) {
                .ByField => try self.instructions.multi_array.append(allocator, inst),
                .ByInstruction => try self.instructions.append(allocator, inst),
            }
        }

        /// Set the contents of an instruction at a given offset.
        pub fn setInstruction(
            self: *Self,
            index: u32,
            comptime opcode: Instruction.Opcode,
            target: Instruction.RegisterLocation,
            payload: opcode.PayloadT(),
            source_range: Range,
        ) void {
            if (builtin.mode == .Debug and self.sealed) {
                @panic("!!! Attempting to set an instruction after the block was sealed!");
            }

            const inst = Instruction{
                .target = target,
                .opcode = opcode,
                .payload = @unionInit(Instruction.Payload, opcode.payloadField(), payload),
                .source_range = source_range,
            };

            switch (access_mode) {
                .ByField => self.instructions.multi_array.set(index, inst),
                .ByInstruction => self.instructions[index] = inst,
            }
        }

        pub fn getTargetLocation(self: Self, index: u32) Instruction.RegisterLocation {
            if (builtin.mode == .Debug and !self.sealed) {
                @panic("!!! Attempting to get the target location for an instruction while the block isn't sealed!");
            }

            return switch (access_mode) {
                .ByField => self.instructions.targets[index],
                .ByInstruction => self.instructions.items[index].target,
            };
        }

        pub fn getOpcode(self: Self, index: u32) Instruction.Opcode {
            if (builtin.mode == .Debug and !self.sealed) {
                @panic("!!! Attempting to get the opcode for an instruction while the block isn't sealed!");
            }

            return switch (access_mode) {
                .ByField => self.instructions.opcodes[index],
                .ByInstruction => self.instructions.items[index].opcode,
            };
        }

        pub fn getPayload(self: Self, index: u32) Instruction.Payload {
            if (builtin.mode == .Debug and !self.sealed) {
                @panic("!!! Attempting to get the payload for an instruction while the block isn't sealed!");
            }

            return switch (access_mode) {
                .ByField => self.instructions.payloads[index],
                .ByInstruction => self.instructions.items[index].payload,
            };
        }

        pub fn getSourceRange(self: Self, index: u32) Range {
            if (builtin.mode == .Debug and !self.sealed) {
                @panic("!!! Attempting to get the source range for an instruction while the block isn't sealed!");
            }

            return switch (access_mode) {
                .ByField => self.instructions.source_ranges[index],
                .ByInstruction => self.instructions.items[index].source_range,
            };
        }

        pub fn getTypedPayload(self: Self, index: u32, comptime opcode: Instruction.Opcode) opcode.PayloadT() {
            return @field(self.getPayload(index), opcode.payloadField());
        }

        /// Reserves space for a single instruction and returns the offset to
        /// it. The instruction memory will be uninitialized, so you must
        /// initialize it before the block is sealed.
        pub fn reserveInstruction(self: *Self, allocator: Allocator) !u32 {
            if (builtin.mode == .Debug and self.sealed) {
                @panic("!!! Attempting to reserve an instruction while the block is sealed!");
            }

            return switch (access_mode) {
                .ByField => blk: {
                    try self.instructions.multi_array.append(allocator, undefined);
                    break :blk @intCast(self.instructions.multi_array.len - 1);
                },
                .ByInstruction => blk: {
                    try self.instructions.append(allocator, undefined);
                    break :blk @intCast(self.instructions.items.len - 1);
                },
            };
        }

        /// Return the amount of instructions in this block.
        pub fn getLength(self: Self) usize {
            return switch (access_mode) {
                .ByField => self.instructions.multi_array.len,
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
            while (it.next()) |inst| {
                if (!inst.target.isNothing())
                    try std.fmt.format(writer, "{} = ", .{inst.target})
                else
                    try writer.writeAll(Instruction.RegisterLocation.ZeroLabel ++ "   ");

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
                        if (self.index >= self.block.instructions.multi_array.len)
                            return null;
                        const slice = self.block.instructions.multi_array.slice();

                        break :blk Instruction{
                            .target = slice.items(.target)[self.index],
                            .opcode = slice.items(.opcode)[self.index],
                            .payload = slice.items(.payload)[self.index],
                            .source_range = slice.items(.source_range)[self.index],
                        };
                    },
                    .ByInstruction => blk: {
                        if (self.index >= self.block.instructions.items.len)
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
