// Copyright (c) 2022-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const astcode = @import("../astcode.zig");

intervals: std.ArrayList(Interval) = .empty,

const Liveness = @This();
pub const Interval = struct {
    start: u32,
    end: u32,
};

pub fn analyzeBlock(allocator: Allocator, block: *astcode.Block) !Liveness {
    var self = Liveness{};
    errdefer self.deinit(allocator);

    // FIXME: This is O(n^2).
    for (0..block.getLength()) |index| {
        try self.addIntervalForRegister(allocator, block.getTargetLocation(index), index, block);
    }

    return self;
}

pub fn deinit(self: *Liveness, allocator: Allocator) void {
    self.intervals.deinit(allocator);
}

fn addIntervalForRegister(self: *Liveness, allocator: Allocator, location: astcode.RegisterLocation, start: usize, block: *astcode.Block) !void {
    if (location == .Nil) return;

    var end = start;

    for (start + 1..block.getLength(), 0..) |index, offset| {
        if (instructionReferencesRegister(block, index, location) and start + offset > 0) {
            // The register died with this instruction, so it was last alive on the instruction before it.
            end = start + offset - 1;
        }
    }

    try self.intervals.append(allocator, .{ .start = @intCast(start), .end = @intCast(end) });
}

fn instructionReferencesRegister(block: *astcode.Block, index: usize, location: astcode.RegisterLocation) bool {
    return switch (block.getOpcode(index)) {
        .Send => blk: {
            const payload = block.getTypedPayload(index, .Send);
            break :blk payload.receiver_location == location;
        },
        .PrimSend => blk: {
            const payload = block.getTypedPayload(index, .PrimSend);
            break :blk payload.receiver_location == location;
        },
        .CreateMethod => blk: {
            const payload = block.getTypedPayload(index, .CreateMethod);
            break :blk payload.method_name_location == location;
        },
        .Return, .NonlocalReturn => blk: {
            const payload = block.getTypedPayload(index, .Return);
            break :blk payload.value_location == location;
        },
        .PushArg => blk: {
            const payload = block.getTypedPayload(index, .PushArg);
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
        .PushArgumentSentinel,
        .VerifyArgumentSentinel,
        => false,

        .PushRegisters => unreachable,
    };
}

pub fn getInterval(self: Liveness, location: astcode.RegisterLocation) Interval {
    return self.intervals.items[location.registerOffset()];
}
