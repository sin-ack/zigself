// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

// FIXME: Move this to astcode/.

const std = @import("std");
const Allocator = std.mem.Allocator;

const bytecode = @import("../../bytecode.zig");

intervals: std.ArrayListUnmanaged(Interval) = .{},

const Liveness = @This();
pub const Interval = struct {
    start: u32,
    end: u32,
};

pub fn analyzeBlock(allocator: Allocator, block: *bytecode.AstcodeBlock) !Liveness {
    var self = Liveness{};
    errdefer self.deinit(allocator);

    // FIXME: This is O(n^2).
    for (block.instructions.items) |inst, start| {
        try self.addIntervalForRegister(allocator, inst.target, start, block);
    }

    return self;
}

pub fn deinit(self: *Liveness, allocator: Allocator) void {
    self.intervals.deinit(allocator);
}

fn addIntervalForRegister(self: *Liveness, allocator: Allocator, location: bytecode.astcode.RegisterLocation, start: usize, block: *bytecode.AstcodeBlock) !void {
    if (location == .Nil) return;

    var end = start;

    for (block.instructions.items[start + 1 ..]) |inst, i| {
        if (instructionReferencesRegister(inst, location)) {
            // The register died with this instruction, so it was last alive on the instruction before it.
            end = start + i - 1;
        }
    }

    try self.intervals.append(allocator, .{ .start = @intCast(u32, start), .end = @intCast(u32, end) });
}

fn instructionReferencesRegister(inst: bytecode.AstcodeInstruction, location: bytecode.astcode.RegisterLocation) bool {
    return switch (inst.value) {
        .Send => |payload| blk: {
            break :blk payload.receiver_location == location;
        },
        .PrimSend => |payload| blk: {
            break :blk payload.receiver_location == location;
        },
        .PushConstantSlot, .PushAssignableSlot => |payload| blk: {
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .PushArgumentSlot, .PushInheritedSlot => |payload| blk: {
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .CreateMethod => |payload| blk: {
            break :blk payload.method_name_location == location;
        },
        .Return, .NonlocalReturn => |payload| blk: {
            break :blk payload.value_location == location;
        },
        .PushArg => |payload| blk: {
            break :blk payload.argument_location == location;
        },

        .SelfSend,
        .SelfPrimSend,
        .CreateInteger,
        .CreateFloatingPoint,
        .CreateObject,
        .CreateBlock,
        .CreateByteArray,
        .SetMethodInline,
        .SourceRange,
        .PushArgumentSentinel,
        .PushSlotSentinel,
        .VerifyArgumentSentinel,
        .VerifySlotSentinel,
        => false,

        .PushRegisters => unreachable,
    };
}

pub fn getInterval(self: Liveness, location: bytecode.astcode.RegisterLocation) Interval {
    return self.intervals.items[location.registerOffset()];
}
