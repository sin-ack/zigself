// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! This struct is an analogue to Range, but contains full Location information.

const std = @import("std");

const Location = @import("./Location.zig");

const LocationRange = @This();

start: Location,
end: Location,

pub fn format(
    range: LocationRange,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    try range.start.format(fmt, options, writer);
    try writer.writeByte('-');

    if (range.start.line != range.end.line) {
        try range.end.format(fmt, options, writer);
    } else {
        try std.fmt.formatInt(range.end.column, 10, .lower, options, writer);
    }
}
