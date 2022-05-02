// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const Instruction = @import("./Instruction.zig");
const Executable = @import("./Executable.zig");
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

instructions: std.ArrayListUnmanaged(Instruction) = .{},
register_count: u32 = 0,

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

pub fn addInstruction(self: *Self, allocator: Allocator, inst: Instruction) !RegisterLocation {
    var inst_copy = inst;
    inst_copy.target = self.makeRegister();

    try self.instructions.append(allocator, inst_copy);

    return inst_copy.target;
}

fn makeRegister(self: *Self) RegisterLocation {
    self.register_count += 1;
    return RegisterLocation.fromIndex(self.register_count);
}

pub fn allocRegisterSlice(self: *Self, allocator: Allocator) ![]Value {
    const register_slice = try allocator.alloc(Value, self.register_count);
    std.mem.set(Value, register_slice, .{ .data = 0 });
    return register_slice;
}

pub fn format(
    block: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    for (block.instructions.items) |inst| {
        try std.fmt.format(writer, "%{} = {}\n", .{ @enumToInt(inst.target), inst });
    }
}
