// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const bytecode_block = @import("./block.zig");
const Script = @import("../../language/Script.zig");
const ref_counted = @import("../../utility/ref_counted.zig");
const ObjectDescriptor = @import("ObjectDescriptor.zig");

fn Executable(comptime BlockT: type) type {
    return struct {
        allocator: Allocator,
        blocks: std.ArrayListUnmanaged(*Block) = .{},
        object_descriptors: std.ArrayListUnmanaged(ObjectDescriptor) = .empty,
        definition_script: Script.Ref,
        ref: ref_counted.RefCount = .{},

        const Self = @This();
        pub const Block = BlockT;
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

            for (self.object_descriptors.items) |*descriptor| {
                descriptor.deinit(self.allocator);
            }
            self.object_descriptors.deinit(self.allocator);
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

            return @intCast(block_index);
        }

        pub fn getBlock(self: *Self, index: u32) *Block {
            return self.blocks.items[index];
        }

        pub fn getEntrypointBlock(self: *Self) *Block {
            // The entrypoint block is currently the first block in an executable.
            return self.getBlock(0);
        }

        // Add the given object descriptor to the executable and return its index.
        pub fn addObjectDescriptor(self: *Self, descriptor: ObjectDescriptor) !u32 {
            const index = self.object_descriptors.items.len;
            try self.object_descriptors.append(self.allocator, descriptor);
            return @intCast(index);
        }

        // Get the object descriptor at the given index.
        pub fn getObjectDescriptor(self: *Self, index: u32) ObjectDescriptor {
            return self.object_descriptors.items[index];
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
            for (executable.blocks.items, 0..) |block, i| {
                try std.fmt.format(writer, "Block {}:\n{}\n", .{ i, block });
            }
        }
    };
}

pub const AstcodeExecutable = Executable(bytecode_block.AstcodeBlock);
pub const LowcodeExecutable = Executable(bytecode_block.LowcodeBlock);
