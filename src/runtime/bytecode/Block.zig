// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const Opcode = @import("./Opcode.zig");
const Executable = @import("./Executable.zig");
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

opcodes: std.ArrayListUnmanaged(Opcode) = .{},
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
    self.opcodes.deinit(allocator);
}

pub fn destroy(self: *Self, allocator: Allocator) void {
    self.deinit(allocator);
    allocator.destroy(self);
}

pub fn getOpcode(self: *Self, index: u32) *Opcode {
    return &self.opcodes.items[index];
}

pub fn addOpcode(self: *Self, allocator: Allocator, opcode: Opcode) !RegisterLocation {
    var opcode_copy = opcode;
    opcode_copy.target = self.makeRegister();

    try self.opcodes.append(allocator, opcode_copy);

    return opcode_copy.target;
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

    for (block.opcodes.items) |opcode| {
        try std.fmt.format(writer, "%{} = {}\n", .{ @enumToInt(opcode.target), opcode });
    }
}
