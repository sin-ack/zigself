// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const bytecode = @import("../../bytecode.zig");
const Liveness = @import("../astcode.zig").Liveness;

const AstcodeRegisterLocation = bytecode.astcode.RegisterLocation;
const LowcodeRegisterLocation = bytecode.lowcode.RegisterLocation;

allocated_registers: AllocatedRegisterMap = .{},
free_registers: LowcodeRegisterLocation.BitSet = LowcodeRegisterLocation.BitSet.initFull(),
active_intervals: ActiveIntervalArray = .{},
clobbered_registers: LowcodeRegisterLocation.BitSet = LowcodeRegisterLocation.BitSet.initEmpty(),

const RegisterPool = @This();
const ActiveIntervalArray = std.ArrayListUnmanaged(ActiveInterval);
const AllocatedRegisterMap = std.AutoArrayHashMapUnmanaged(AstcodeRegisterLocation, LowcodeRegisterLocation);
pub const ActiveInterval = struct {
    bound_register: LowcodeRegisterLocation,
    interval: Liveness.Interval,
};

pub fn init(allocator: Allocator) !RegisterPool {
    var self = RegisterPool{};
    errdefer self.deinit(allocator);

    return self;
}

pub fn deinit(self: *RegisterPool, allocator: Allocator) void {
    self.allocated_registers.deinit(allocator);
    self.active_intervals.deinit(allocator);
}

pub fn getAllocatedRegisterFor(self: RegisterPool, ast_register: AstcodeRegisterLocation) LowcodeRegisterLocation {
    if (ast_register == .Nil)
        return .zero;

    return self.allocated_registers.get(ast_register).?;
}

pub fn allocateRegister(
    self: *RegisterPool,
    allocator: Allocator,
    block: *bytecode.lowcode.Block,
    liveness: *Liveness,
    ast_register: bytecode.astcode.RegisterLocation,
) !bytecode.lowcode.RegisterLocation {
    if (ast_register == .Nil)
        return .zero;

    std.debug.assert(!self.allocated_registers.contains(ast_register));

    const interval = liveness.getInterval(ast_register);
    var free_register = self.free_registers.toggleFirstSet();
    if (free_register == null) {
        free_register = try self.spillFurthestRegister(allocator, block);
    }

    self.clobbered_registers.set(free_register.?);
    // FIXME: Remove manual register number adjustment!
    const register = LowcodeRegisterLocation.fromInt(@intCast(u32, free_register.? + 2));
    try self.insertActiveInterval(allocator, register, interval);
    try self.allocated_registers.put(allocator, ast_register, register);

    return register;
}

fn insertActiveInterval(
    self: *RegisterPool,
    allocator: Allocator,
    register: bytecode.lowcode.RegisterLocation,
    interval: Liveness.Interval,
) !void {
    const active_interval = ActiveInterval{ .bound_register = register, .interval = interval };

    var did_insert = false;
    for (self.active_intervals.items) |int, i| {
        if (int.interval.end > interval.end) {
            try self.active_intervals.insert(allocator, i, active_interval);
            did_insert = true;
            break;
        }
    }

    if (!did_insert)
        try self.active_intervals.append(allocator, active_interval);
}

fn spillFurthestRegister(self: *RegisterPool, allocator: Allocator, block: *bytecode.lowcode.Block) !usize {
    _ = self;
    _ = allocator;
    _ = block;

    @panic("TODO register spilling");
}

pub fn expireOldIntervals(self: *RegisterPool, start: usize) void {
    var intervals_to_remove: usize = 0;
    for (self.active_intervals.items) |int| {
        if (int.interval.end >= start)
            break;
        intervals_to_remove += 1;

        const register_offset = int.bound_register.registerOffset();
        self.free_registers.set(register_offset);
    }

    // FIXME: This is O(n^2).
    while (intervals_to_remove > 0) : (intervals_to_remove -= 1) {
        _ = self.active_intervals.orderedRemove(0);
    }
}
