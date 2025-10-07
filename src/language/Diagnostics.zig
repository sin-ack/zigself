// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Location = @import("./Location.zig");

allocator: Allocator,
diagnostics: DiagnosticList,

const Diagnostics = @This();

pub const DiagnosticLevel = enum {
    Note,
    Warning,
    Error,
};

pub const Diagnostic = struct {
    level: DiagnosticLevel,
    location: Location,
    message: []const u8,
    message_is_allocated: bool = false,
    // TODO: Filename, location

    pub fn deinit(self: *Diagnostic, allocator: Allocator) void {
        if (self.message_is_allocated) {
            allocator.free(self.message);
        }
    }
};

const DiagnosticList = std.ArrayList(Diagnostic);

pub fn init(allocator: Allocator) !Diagnostics {
    return Diagnostics{
        .allocator = allocator,
        .diagnostics = .empty,
    };
}

pub fn deinit(self: *Diagnostics) void {
    for (self.diagnostics.items) |*diagnostic| {
        diagnostic.deinit(self.allocator);
    }
    self.diagnostics.deinit(self.allocator);
}

pub fn reportDiagnostic(self: *Diagnostics, comptime level: DiagnosticLevel, location: Location, comptime message: []const u8) !void {
    try self.diagnostics.append(self.allocator, .{ .level = level, .location = location, .message = message, .message_is_allocated = false });
}

pub fn reportDiagnosticFormatted(self: *Diagnostics, comptime level: DiagnosticLevel, location: Location, comptime message: []const u8, args: anytype) !void {
    const formatted_message = try std.fmt.allocPrint(self.allocator, message, args);
    try self.diagnostics.append(self.allocator, .{ .level = level, .location = location, .message = formatted_message, .message_is_allocated = true });
}
