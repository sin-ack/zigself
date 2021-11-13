// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Location = @import("./location.zig");

const Self = @This();

pub const DiagnosticLevel = enum {
    Info,
    Warning,
    Error,
};

pub const Diagnostic = struct {
    level: DiagnosticLevel,
    location: Location,
    message: []const u8,
    message_is_allocated: bool = false,
    // TODO: Filename, location

    pub fn deinit(self: *Diagnostic, allocator: *std.mem.Allocator) void {
        if (self.message_is_allocated) {
            allocator.free(self.message);
        }
    }
};

const DiagnosticList = std.ArrayList(Diagnostic);

pub fn init(allocator: *std.mem.Allocator) !Self {
    return Self{
        .allocator = allocator,
        .diagnostics = DiagnosticList.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    for (self.diagnostics.items) |*diagnostic| {
        diagnostic.deinit(self.allocator);
    }
    self.diagnostics.deinit();
}

pub fn reportDiagnostic(self: *Self, comptime level: DiagnosticLevel, location: Location, comptime message: []const u8) !void {
    try self.diagnostics.append(Diagnostic{ .level = level, .location = location, .message = message, .message_is_allocated = false });
}

pub fn reportDiagnosticFormatted(self: *Self, comptime level: DiagnosticLevel, location: Location, comptime message: []const u8, args: anytype) !void {
    const formatted_message = try std.fmt.allocPrint(self.allocator, message, args);
    try self.diagnostics.append(Diagnostic{ .level = level, .location = location, .message = formatted_message, .message_is_allocated = true });
}

allocator: *std.mem.Allocator,
diagnostics: DiagnosticList,
