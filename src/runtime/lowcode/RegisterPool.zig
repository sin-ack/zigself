// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Block = @import("./Block.zig");
const Liveness = @import("./Liveness.zig");
const AstCodeRegisterLocation = @import("../astcode/register_location.zig").RegisterLocation;
const lowcode_register_location = @import("./register_location.zig");
const LowCodeRegisterLocation = lowcode_register_location.RegisterLocation;

allocated_registers: AllocatedRegisterMap = .{},
free_registers: RegisterBitSet = RegisterBitSet.initFull(),
active_intervals: ActiveIntervalArray = .{},
clobbered_registers: RegisterBitSet = RegisterBitSet.initEmpty(),

const RegisterPool = @This();
pub const RegisterBitSet = std.StaticBitSet(lowcode_register_location.GeneralPurposeRegisterCount);
const ActiveIntervalArray = std.ArrayListUnmanaged(ActiveInterval);
const AllocatedRegisterMap = std.AutoArrayHashMapUnmanaged(AstCodeRegisterLocation, LowCodeRegisterLocation);
pub const ActiveInterval = struct {
    bound_register: LowCodeRegisterLocation,
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

pub fn getAllocatedRegisterFor(self: RegisterPool, ast_register: AstCodeRegisterLocation) LowCodeRegisterLocation {
    if (ast_register == .Nil)
        return .zero;

    return self.allocated_registers.get(ast_register).?;
}

pub fn allocateRegister(self: *RegisterPool, allocator: Allocator, block: *Block, liveness: *Liveness, ast_register: AstCodeRegisterLocation) !LowCodeRegisterLocation {
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
    const register = LowCodeRegisterLocation.fromInt(@intCast(u32, free_register.? + 2));
    try self.insertActiveInterval(allocator, register, interval);
    try self.allocated_registers.put(allocator, ast_register, register);

    // std.debug.print("RegisterPool.allocateRegister: Allocated register: {s} for AST register: {}\n", .{ @tagName(register), ast_register });
    return register;
}

fn insertActiveInterval(self: *RegisterPool, allocator: Allocator, register: LowCodeRegisterLocation, interval: Liveness.Interval) !void {
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

fn spillFurthestRegister(self: *RegisterPool, allocator: Allocator, block: *Block) !usize {
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

    // std.debug.print("Removing {} intervals, free registers: {b}\n", .{ intervals_to_remove, self.free_registers.mask });

    // FIXME: This is O(n^2).
    while (intervals_to_remove > 0) : (intervals_to_remove -= 1) {
        _ = self.active_intervals.orderedRemove(0);
    }
}
