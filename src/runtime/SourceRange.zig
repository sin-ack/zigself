// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Range = @import("../language/location_range.zig");
const Executable = @import("./bytecode/Executable.zig");

const Self = @This();

/// The executable this source range belongs to. When attempting to get a stack
/// trace, this executable's definition script will be referenced for the source
/// code.
executable: Executable.Ref,
/// The range of this source range.
range: Range,

/// Refs executable.
pub fn init(executable: Executable.Ref, range: Range) Self {
    executable.ref();
    return .{ .executable = executable, .range = range };
}

pub fn deinit(self: *Self) void {
    self.executable.unref();
}

pub fn copy(self: Self) Self {
    return init(self.executable, self.range);
}

pub fn getStartLine(self: Self) ![]const u8 {
    // FIXME: This is too complicated.
    return self.executable.value.definition_script.value.parser.buffer[self.range.start.line_start..self.range.start.line_end];
}

pub fn format(
    source_range: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    try std.fmt.formatText(source_range.executable.value.definition_script.value.file_path, "s", options, writer);
    try writer.writeByte(':');
    try source_range.range.format(fmt, options, writer);
}
