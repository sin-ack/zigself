// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const Instruction = @import("./Instruction.zig");
const RegisterPool = @import("./RegisterPool.zig");
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

instructions: std.ArrayListUnmanaged(Instruction) = .{},

const Self = @This();

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

pub fn insertPushRegistersAtPrelude(
    self: *Self,
    allocator: Allocator,
    clobbered_registers: RegisterPool.RegisterBitSet,
) !void {
    try self.instructions.insert(allocator, 0, Instruction.pushRegisters(.zero, clobbered_registers));
}

pub fn format(
    block: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    for (block.instructions.items) |inst| {
        if (inst.target != .zero)
            try std.fmt.format(writer, "{} = ", .{inst.target})
        else
            // '%rN = '
            try writer.writeAll("      ");

        try inst.format(fmt, options, writer);
        try writer.writeByte('\n');
    }
}
