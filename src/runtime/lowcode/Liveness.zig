// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

// FIXME: Move this to astcode/.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Block = @import("../astcode/Block.zig");
const Instruction = @import("../astcode/Instruction.zig");
const RegisterLocation = @import("../astcode/register_location.zig").RegisterLocation;

intervals: std.ArrayListUnmanaged(Interval) = .{},

const Liveness = @This();
pub const Interval = struct {
    start: u32,
    end: u32,
};

pub fn analyzeBlock(allocator: Allocator, block: *Block) !Liveness {
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

fn addIntervalForRegister(self: *Liveness, allocator: Allocator, location: RegisterLocation, start: usize, block: *Block) !void {
    std.debug.assert(self.intervals.items.len == location.registerOffset());
    var end = start;

    for (block.instructions.items[start + 1 ..]) |inst, i| {
        if (instructionReferencesRegister(inst, location)) {
            // The register died with this instruction, so it was last alive on the instruction before it.
            end = start + i - 1;
        }
    }

    try self.intervals.append(allocator, .{ .start = @intCast(u32, start), .end = @intCast(u32, end) });
}

fn instructionReferencesRegister(inst: Instruction, location: RegisterLocation) bool {
    return switch (inst.tag) {
        .Send => blk: {
            const payload = inst.payload(.Send);
            break :blk payload.receiver_location == location;
        },
        .PrimSend => blk: {
            const payload = inst.payload(.PrimSend);
            break :blk payload.receiver_location == location;
        },
        .PushConstantSlot => blk: {
            const payload = inst.payload(.PushConstantSlot);
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .PushAssignableSlot => blk: {
            const payload = inst.payload(.PushAssignableSlot);
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .PushArgumentSlot => blk: {
            const payload = inst.payload(.PushArgumentSlot);
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .PushInheritedSlot => blk: {
            const payload = inst.payload(.PushInheritedSlot);
            break :blk payload.name_location == location or payload.value_location == location;
        },
        .CreateMethod => blk: {
            const payload = inst.payload(.CreateMethod);
            break :blk payload.method_name_location == location;
        },
        .ExitActivation => blk: {
            const payload = inst.payload(.ExitActivation);
            break :blk payload.value_location == location;
        },
        .NonlocalReturn => blk: {
            const payload = inst.payload(.NonlocalReturn);
            break :blk payload.value_location == location;
        },
        .PushArg => blk: {
            const payload = inst.payload(.PushArg);
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
    };
}

pub fn getInterval(self: Liveness, location: RegisterLocation) Interval {
    return self.intervals.items[location.registerOffset()];
}
