// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("../Actor.zig");
const Block = @import("./Block.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const Script = @import("../../language/script.zig");
const Completion = @import("../Completion.zig");
const Activation = @import("../Activation.zig");
const SourceRange = @import("../SourceRange.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationStack = Activation.ActivationStack;
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

allocator: Allocator,
definition_script: Script.Ref,
blocks: std.ArrayListUnmanaged(*Block) = .{},

const Self = @This();

pub fn create(allocator: Allocator, script: Script.Ref) !*Self {
    const self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    self.init(allocator, script);

    return self;
}

fn init(self: *Self, allocator: Allocator, script: Script.Ref) void {
    script.ref();

    self.* = .{
        .allocator = allocator,
        .definition_script = script,
    };
}

fn deinit(self: *Self) void {
    self.definition_script.unref();

    for (self.blocks.items) |block| {
        block.destroy(self.allocator);
    }
    self.blocks.deinit(self.allocator);
}

pub fn destroy(self: *Self) void {
    self.deinit();
    self.allocator.destroy(self);
}

pub fn makeBlock(self: *Self) !u32 {
    const block = try Block.create(self.allocator);
    errdefer block.destroy(self.allocator);

    const block_index = self.blocks.items.len;
    try self.blocks.append(self.allocator, block);

    return @intCast(u32, block_index);
}

pub fn getBlock(self: *Self, index: u32) *Block {
    return self.blocks.items[index];
}

pub fn format(
    executable: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try std.fmt.format(writer, "ASTcode executable @ {s} ({} blocks):\n", .{ executable.definition_script.value.file_path, executable.blocks.items.len });
    for (executable.blocks.items) |block, i| {
        try std.fmt.format(writer, "Block {}:\n{}\n", .{ i, block });
    }
}
