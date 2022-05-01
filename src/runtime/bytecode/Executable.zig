// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("../Actor.zig");
const Block = @import("./Block.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Script = @import("../../language/script.zig");
const Completion = @import("../Completion.zig");
const Activation = @import("../Activation.zig");
const SourceRange = @import("../SourceRange.zig");
const ref_counted = @import("../../utility/ref_counted.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationStack = Activation.ActivationStack;
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

ref: ref_counted.RefCount = .{},

allocator: Allocator,
definition_script: Script.Ref,
blocks: std.ArrayListUnmanaged(*Block) = .{},
sealed: bool = false,

const Self = @This();
pub const Ref = ref_counted.RefPtr(Self);

pub fn create(allocator: Allocator, script: Script.Ref) !Ref {
    const self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    self.init(allocator, script);

    return Ref.adopt(self);
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

fn entrypointBlock(self: *Self) *Block {
    return self.blocks.items[0];
}

pub fn pushEntrypointActivation(self: *Self, vm: *VirtualMachine, activation_stack: *ActivationStack) !void {
    try self.pushSubEntrypointActivation(vm, Ref{ .value = self }, RegisterLocation.Nil, activation_stack);
}

/// Pushes an entrypoint activation for this executable, with the creation
/// context pointing at another executable. This is used when executing scripts
/// by _RunScript, for example.
pub fn pushSubEntrypointActivation(self: *Self, vm: *VirtualMachine, executable: Ref, target_location: RegisterLocation, activation_stack: *ActivationStack) !void {
    var source_range = SourceRange.init(executable, .{
        .start = .{ .line_start = 0, .line_end = 2, .line = 1, .column = 1 },
        .end = .{ .line_start = 0, .line_end = 2, .line = 1, .column = 2 },
    });
    defer source_range.deinit();

    const toplevel_context_method = try Object.Method.createTopLevelContextForExecutable(vm, Ref{ .value = self }, self.entrypointBlock());
    const activation_slot = activation_stack.getNewActivationSlot();
    try toplevel_context_method.activateMethod(vm, vm.lobby(), &.{}, target_location, source_range, activation_slot);
    activation_stack.setRegisters(vm);
}

pub fn makeBlock(self: *Self) !u32 {
    if (self.sealed)
        @panic("!!! Attempting to make a block in a sealed executable");

    const block = try Block.create(self.allocator);
    errdefer block.destroy(self.allocator);

    const block_index = self.blocks.items.len;
    try self.blocks.append(self.allocator, block);

    return @intCast(u32, block_index);
}

pub fn getBlock(self: *Self, index: u32) *Block {
    return self.blocks.items[index];
}

pub fn seal(self: *Self) !void {
    self.sealed = true;
}

pub fn format(
    executable: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try std.fmt.format(writer, "Executable @ {s} ({} blocks):\n", .{ executable.definition_script.value.file_path, executable.blocks.items.len });
    for (executable.blocks.items) |block, i| {
        try std.fmt.format(writer, "Block {}:\n{}\n", .{ i, block });
    }
}
