// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Self = @This();

line: usize,
column: usize,
/// The offset at which the current line starts in the corresponding source file.
line_start: usize,
/// The offset at which the current line ends in the corresponding source file.
/// Points to the newline.
line_end: usize,

pub fn format(
    location: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;

    try std.fmt.formatInt(location.line, 10, .lower, options, writer);
    try writer.writeByte(':');
    try std.fmt.formatInt(location.column, 10, .lower, options, writer);
}
