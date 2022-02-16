// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Range = @import("./location_range.zig");
const Script = @import("./script.zig");

const Self = @This();

/// The script this source range belongs to. When attempting to get a stack
/// trace, this script will be referenced for the source code.
script: Script.Ref,
/// The range of this source range.
range: Range,

/// Refs script.
pub fn init(script: Script.Ref, range: Range) Self {
    script.ref();
    return .{ .script = script, .range = range };
}

pub fn deinit(self: *Self) void {
    self.script.unref();
}

pub fn getStartLine(self: Self) ![]const u8 {
    return self.script.value.parser.lexer.getLineForLocation(self.range.start);
}

pub fn format(self: Self) std.fmt.Formatter(formatSourceRange) {
    return .{ .data = self };
}

fn formatSourceRange(
    source_range: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    try std.fmt.formatText(source_range.script.value.file_path, "s", options, writer);
    try writer.writeByte(':');
    try source_range.range.format().format(fmt, options, writer);
}
