// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

pub const RegisterLocation = enum(u32) {
    Nil,
    _,

    // "%0"
    pub const ZeroLabel: []const u8 = "  ";

    pub fn isFinite() bool {
        return false;
    }

    pub fn isNothing(self: RegisterLocation) bool {
        return self == .Nil;
    }

    pub fn fromIndex(index: u32) RegisterLocation {
        std.debug.assert(index > 0);
        return fromIndexAllowNil(index);
    }

    pub fn fromIndexAllowNil(index: u32) RegisterLocation {
        return @enumFromInt(index);
    }

    pub fn registerOffset(self: RegisterLocation) u32 {
        const offset = @intFromEnum(self);
        std.debug.assert(offset > 0);
        return offset - 1;
    }

    pub fn format(loc: RegisterLocation, writer: *std.io.Writer) !void {
        try writer.writeByte('%');
        try writer.printInt(@intFromEnum(loc), 10, .lower, .{});
    }
};
