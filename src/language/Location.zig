// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Location = @This();

line: usize,
column: usize,
/// The offset at which the current line starts in the corresponding source file.
line_start: usize,
/// The offset at which the current line ends in the corresponding source file.
/// Points to the newline.
line_end: usize,

pub fn format(self: Location, writer: *std.io.Writer) !void {
    try writer.printInt(self.line, 10, .lower, .{});
    try writer.writeByte(':');
    try writer.printInt(self.column, 10, .lower, .{});
}
