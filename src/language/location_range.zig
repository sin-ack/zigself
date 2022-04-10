// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Location = @import("./location.zig");

const Self = @This();

start: Location,
end: Location,

pub fn format(
    range: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;

    try range.start.format(fmt, options, writer);
    try writer.writeByte('-');

    if (range.start.line != range.end.line) {
        try range.end.format(fmt, options, writer);
    } else {
        try std.fmt.formatInt(range.end.column, 10, .lower, options, writer);
    }
}
