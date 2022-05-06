// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

pub const RegisterLocation = enum(u32) {
    Nil,
    _,

    pub fn fromIndex(index: u32) RegisterLocation {
        std.debug.assert(index > 0);
        return fromIndexAllowNil(index);
    }

    pub fn fromIndexAllowNil(index: u32) RegisterLocation {
        return @intToEnum(RegisterLocation, index);
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
