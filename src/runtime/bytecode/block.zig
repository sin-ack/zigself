// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const instruction = @import("./instruction.zig");
const Range = @import("../../language/Range.zig");
const InlineCacheEntry = @import("../inline_cache.zig").InlineCacheEntry;

const SEAL = std.debug.runtime_safety;

fn Block(comptime InstructionT: type) type {
    return struct {
        instructions: InstructionList = .{},
        send_count: u32 = 0,
        sealed: if (SEAL) bool else void = if (SEAL) false else {},

        const Self = @This();
        pub const Instruction = InstructionT;
        pub const InstructionList = struct {
            multi_array: std.MultiArrayList(Instruction) = .{},

            // Individual array pointers. Initialized with seal().
            targets: []InstructionT.RegisterLocation = undefined,
            opcodes: []InstructionT.Opcode = undefined,
            payloads: []InstructionT.Payload = undefined,
            source_ranges: []Range = undefined,
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
            self.instructions.multi_array.deinit(allocator);
        }

        pub fn destroy(self: *Self, allocator: Allocator) void {
            self.deinit(allocator);
            allocator.destroy(self);
        }

        pub fn seal(self: *Self) void {
            if (SEAL) {
                if (self.sealed) {
                    @panic("!!! Attempting to re-seal a block!");
                }

                self.sealed = true;
            }

            self.instructions.targets = self.instructions.multi_array.items(.target);
            self.instructions.opcodes = self.instructions.multi_array.items(.opcode);
            self.instructions.payloads = self.instructions.multi_array.items(.payload);
            self.instructions.source_ranges = self.instructions.multi_array.items(.source_range);
        }

        /// Allocate a new send index and return it.
        pub fn makeSendIndex(self: *Self) u32 {
            if (SEAL and self.sealed) {
                @panic("!!! Attempting to make a send index for a block that is sealed!");
            }

            const index = self.send_count;
            self.send_count += 1;
            return index;
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
            if (SEAL and self.sealed) {
                @panic("!!! Attempting to add an instruction after the block was sealed!");
            }

            const inst = InstructionT{
                .target = target,
                .opcode = opcode,
                .payload = @unionInit(Instruction.Payload, opcode.payloadField(), payload),
                .source_range = source_range,
            };

            try self.instructions.multi_array.append(allocator, inst);
        }

        /// Set the contents of an instruction at a given offset.
        pub fn setInstruction(
            self: *Self,
            index: usize,
            comptime opcode: Instruction.Opcode,
            target: Instruction.RegisterLocation,
            payload: opcode.PayloadT(),
            source_range: Range,
        ) void {
            if (SEAL and self.sealed) {
                @panic("!!! Attempting to set an instruction after the block was sealed!");
            }

            const inst = Instruction{
                .target = target,
                .opcode = opcode,
                .payload = @unionInit(Instruction.Payload, opcode.payloadField(), payload),
                .source_range = source_range,
            };

            self.instructions.multi_array.set(index, inst);
        }

        pub fn getTargetLocation(self: Self, index: usize) Instruction.RegisterLocation {
            if (SEAL and !self.sealed) {
                @panic("!!! Attempting to get the target location for an instruction while the block isn't sealed!");
            }

            return self.instructions.targets[index];
        }

        pub fn getOpcode(self: Self, index: usize) Instruction.Opcode {
            if (SEAL and !self.sealed) {
                @panic("!!! Attempting to get the opcode for an instruction while the block isn't sealed!");
            }

            return self.instructions.opcodes[index];
        }

        pub fn getPayload(self: Self, index: usize) Instruction.Payload {
            if (SEAL and !self.sealed) {
                @panic("!!! Attempting to get the payload for an instruction while the block isn't sealed!");
            }

            return self.instructions.payloads[index];
        }

        pub fn getSourceRange(self: Self, index: usize) Range {
            if (SEAL and !self.sealed) {
                @panic("!!! Attempting to get the source range for an instruction while the block isn't sealed!");
            }

            return self.instructions.source_ranges[index];
        }

        pub fn getTypedPayload(self: Self, index: usize, comptime opcode: Instruction.Opcode) opcode.PayloadT() {
            return @field(self.getPayload(index), opcode.payloadField());
        }

        /// Reserves space for a single instruction and returns the offset to
        /// it. The instruction memory will be uninitialized, so you must
        /// initialize it before the block is sealed.
        pub fn reserveInstruction(self: *Self, allocator: Allocator) !usize {
            if (SEAL and self.sealed) {
                @panic("!!! Attempting to reserve an instruction while the block is sealed!");
            }

            try self.instructions.multi_array.append(allocator, undefined);
            return self.instructions.multi_array.len - 1;
        }

        /// Allocate an inline cache based on the current block.
        pub fn allocateInlineCache(self: *Self, allocator: Allocator) ![]InlineCacheEntry {
            const cache = try allocator.alloc(InlineCacheEntry, self.send_count);
            for (cache) |*entry| {
                entry.* = .init();
            }
            return cache;
        }

        /// Return the amount of instructions in this block.
        pub fn getLength(self: Self) usize {
            return self.instructions.multi_array.len;
        }

        pub fn format(block: Self, writer: *std.io.Writer) !void {
            var it = block.iterator();
            while (it.next()) |inst| {
                if (!inst.target.isNothing())
                    try writer.print("{f} = ", .{inst.target})
                else
                    try writer.writeAll(Instruction.RegisterLocation.ZeroLabel ++ "   ");

                try inst.format(writer);
                try writer.writeByte('\n');
            }
        }

        const Iterator = struct {
            block: *const Self,
            index: usize = 0,

            pub fn init(block: *const Self) Iterator {
                return .{ .block = block };
            }

            pub fn next(self: *Iterator) ?Instruction {
                if (self.index >= self.block.instructions.multi_array.len)
                    return null;
                const slice = self.block.instructions.multi_array.slice();

                const inst = Instruction{
                    .target = slice.items(.target)[self.index],
                    .opcode = slice.items(.opcode)[self.index],
                    .payload = slice.items(.payload)[self.index],
                    .source_range = slice.items(.source_range)[self.index],
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

pub const AstcodeBlock = Block(instruction.AstcodeInstruction);
pub const LowcodeBlock = Block(instruction.LowcodeInstruction);
