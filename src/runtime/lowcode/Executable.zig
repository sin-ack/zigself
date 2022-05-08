// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Block = @import("./Block.zig");
const Script = @import("../../language/script.zig");
const Object = @import("../Object.zig");
const Activation = @import("../Activation.zig");
const SourceRange = @import("../SourceRange.zig");
const ref_counted = @import("../../utility/ref_counted.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationStack = Activation.ActivationStack;
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

ref: ref_counted.RefCount = .{},

allocator: Allocator,
blocks: std.ArrayListUnmanaged(*Block) = .{},
definition_script: Script.Ref,

const Executable = @This();
pub const Ref = ref_counted.RefPtr(Executable);

pub fn create(allocator: Allocator, script: Script.Ref) !Ref {
    const self = try allocator.create(Executable);
    errdefer allocator.destroy(self);

    self.init(allocator, script);
    return Ref.adopt(self);
}

fn init(self: *Executable, allocator: Allocator, script: Script.Ref) void {
    script.ref();

    self.* = .{
        .allocator = allocator,
        .definition_script = script,
    };
}

fn deinit(self: *Executable) void {
    self.definition_script.unref();

    for (self.blocks.items) |block| {
        block.destroy(self.allocator);
    }
    self.blocks.deinit(self.allocator);
}

pub fn destroy(self: *Executable) void {
    self.deinit();
    self.allocator.destroy(self);
}

pub fn pushEntrypointActivation(self: *Executable, vm: *VirtualMachine, activation_stack: *ActivationStack) !void {
    try self.pushSubEntrypointActivation(vm, Ref{ .value = self }, RegisterLocation.zero, activation_stack);
}

/// Pushes an entrypoint activation for this executable, with the creation
/// context pointing at another executable. This is used when executing scripts
/// by _RunScript, for example.
pub fn pushSubEntrypointActivation(self: *Executable, vm: *VirtualMachine, executable: Ref, target_location: RegisterLocation, activation_stack: *ActivationStack) !void {
    var source_range = SourceRange.initNoRef(executable, .{ .start = 0, .end = 1 });

    const toplevel_context_method = try Object.Method.createTopLevelContextForExecutable(vm, Ref{ .value = self }, self.entrypointBlock());
    const activation_slot = activation_stack.getNewActivationSlot();
    try toplevel_context_method.activateMethod(vm, vm.lobby(), &.{}, target_location, source_range, activation_slot);
}

pub fn makeBlock(self: *Executable) !u32 {
    const block = try Block.create(self.allocator);
    errdefer block.destroy(self.allocator);

    const block_index = self.blocks.items.len;
    try self.blocks.append(self.allocator, block);

    return @intCast(u32, block_index);
}

pub fn getBlock(self: *Executable, index: u32) *Block {
    return self.blocks.items[index];
}

fn entrypointBlock(self: *Executable) *Block {
    return self.blocks.items[0];
}

pub fn format(
    executable: Executable,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try std.fmt.format(writer, "Lowcode executable @ {s} ({} blocks):\n", .{ executable.definition_script.value.file_path, executable.blocks.items.len });
    for (executable.blocks.items) |block, i| {
        try std.fmt.format(writer, "Block {}:\n{}\n", .{ i, block });
    }
}
