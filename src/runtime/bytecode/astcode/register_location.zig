// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

pub const RegisterLocation = enum(u32) {
    Nil,
    _,

    pub fn isFinite() bool {
        return false;
    }

    pub fn isNothing(self: RegisterLocation) bool {
        return self == .Nil;
    }

    pub fn zeroLabel() []const u8 {
        // "%0"
        return "  ";
    }

    pub fn fromIndex(index: u32) RegisterLocation {
        std.debug.assert(index > 0);
        return fromIndexAllowNil(index);
    }

    pub fn fromIndexAllowNil(index: u32) RegisterLocation {
        return @intToEnum(RegisterLocation, index);
    }

    pub fn registerOffset(self: RegisterLocation) u32 {
        const offset = @enumToInt(self);
        std.debug.assert(offset > 0);
        return offset - 1;
    }

    pub fn format(
        loc: RegisterLocation,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        try writer.writeByte('%');
        try std.fmt.formatInt(@enumToInt(loc), 10, .lower, options, writer);
    }
};
