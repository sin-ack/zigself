// Copyright (c) 2022-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Range = @import("../language/Range.zig");
const LocationRange = @import("../language/LocationRange.zig");
const Executable = @import("./bytecode.zig").Executable;

/// The executable this source range belongs to. When attempting to get a stack
/// trace, this executable's definition script will be referenced for the source
/// code.
executable: Executable.Ref,
/// The range of this source range. Note that this only contains the start and
/// end offsets within the script file; when necessary, the line and column
/// information can be obtained from the script.
range: Range,

const SourceRange = @This();

/// Refs executable.
pub fn init(executable: Executable.Ref, range: Range) SourceRange {
    executable.ref();
    return initNoRef(executable, range);
}

/// Does NOT ref executable; this is intended to be used in places where a
/// template source range is given to objects which store a copy. Calling deinit
/// on SourceRange objects created this way is an error.
pub fn initNoRef(executable: Executable.Ref, range: Range) SourceRange {
    return .{ .executable = executable, .range = range };
}

pub fn deinit(self: *SourceRange) void {
    self.executable.unref();
}

pub fn copy(self: SourceRange) SourceRange {
    return init(self.executable, self.range);
}

pub fn getLocationRange(self: SourceRange) LocationRange {
    return .{
        .start = self.executable.value.definition_script.value.offsetToLocation(self.range.start),
        // NOTE: Source ranges are end-exclusive.
        .end = self.executable.value.definition_script.value.offsetToLocation(self.range.end - 1),
    };
}

pub fn getStartLine(self: SourceRange) ![]const u8 {
    return self.executable.value.definition_script.value.getSourceLine(self.getLocationRange().start);
}

pub fn format(
    source_range: SourceRange,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    try std.fmt.formatText(source_range.executable.value.definition_script.value.file_path, "s", options, writer);
    try writer.writeByte(':');
    try source_range.getLocationRange().format(fmt, options, writer);
}
