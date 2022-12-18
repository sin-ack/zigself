// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

// FIXME: Move this to astcode/.

const std = @import("std");
const Allocator = std.mem.Allocator;

const astcode = @import("../astcode.zig");

intervals: std.ArrayListUnmanaged(Interval) = .{},

const Liveness = @This();
pub const Interval = struct {
    start: u32,
    end: u32,
};

pub fn analyzeBlock(allocator: Allocator, block: *astcode.Block) !Liveness {
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

fn addIntervalForRegister(self: *Liveness, allocator: Allocator, location: astcode.RegisterLocation, start: usize, block: *astcode.Block) !void {
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

fn instructionReferencesRegister(inst: astcode.Instruction, location: astcode.RegisterLocation) bool {
    return switch (inst.opcode) {
        .Send, .PrimSend => blk: {
            break :blk inst.payload.Send.receiver_location == location;
        },
        .PushConstantSlot, .PushAssignableSlot => blk: {
            const payload = inst.payload.PushParentableSlot;
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .PushArgumentSlot, .PushInheritedSlot => blk: {
            const payload = inst.payload.PushNonParentSlot;
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .CreateMethod => blk: {
            break :blk inst.payload.CreateMethod.method_name_location == location;
        },
        .Return, .NonlocalReturn => blk: {
            break :blk inst.payload.Return.value_location == location;
        },
        .PushArg => blk: {
            break :blk inst.payload.PushArg.argument_location == location;
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

pub fn getInterval(self: Liveness, location: astcode.RegisterLocation) Interval {
    return self.intervals.items[location.registerOffset()];
}
