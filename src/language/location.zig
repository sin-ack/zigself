const std = @import("std");

const Self = @This();

pub fn advanceColumn(self: *Self) void {
    self.column += 1;
    self.offset += 1;
}

pub fn advanceLine(self: *Self) void {
    self.line += 1;
    self.column = 1;
    self.offset += 1;
}

pub fn advanceForCharacter(self: *Self, c: u8) void {
    if (c == '\n' or c == '\r') {
        self.advanceLine();
    } else {
        self.advanceColumn();
    }
}

pub fn format(self: Self) std.fmt.Formatter(formatLocation) {
    return .{ .data = self };
}

fn formatLocation(
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

line: usize = 1,
column: usize = 1,
offset: usize = 0,
