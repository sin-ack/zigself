// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Parser = @import("./parser.zig");
const Diagnostics = @import("./diagnostics.zig");
const AST = @import("./ast.zig");

const Self = @This();

initialized: bool = false,
ast_root: ?AST.ScriptNode = null,

allocator: *Allocator = undefined,
file_path: []const u8 = undefined,
parser: Parser = undefined,

pub fn initInPlaceFromFilePath(self: *Self, file_path: []const u8, allocator: *Allocator) !void {
    if (self.initialized)
        @panic("Attempting to initialize already-initialized script object");

    self.file_path = file_path;
    self.allocator = allocator;
    self.parser = .{};

    try self.parser.initInPlaceFromFilePath(file_path, allocator);

    self.initialized = true;
}

pub fn deinit(self: *Self) void {
    if (!self.initialized)
        @panic("Attempting to call Script.deinit on uninitialized script object");

    self.parser.deinit();
    if (self.ast_root) |*ast_root| {
        ast_root.deinit(self.allocator);
    }
}

/// Parse the script and expose it in `self.ast_root`. Return whether the script
/// was parsed without any error diagnostics being generated.
pub fn parseScript(self: *Self) !bool {
    if (!self.initialized)
        @panic("Attempting to call Script.parseScript on uninitialized script object");

    self.ast_root = try self.parser.parse();

    for (self.diagnostics().diagnostics.items) |diagnostic| {
        if (diagnostic.level == .Error) {
            return false;
        }
    }

    return true;
}

pub fn diagnostics(self: Self) Diagnostics {
    if (!self.initialized)
        @panic("Attempting to call Script.diagnostics on uninitialized script object");

    return self.parser.diagnostics;
}

pub fn reportDiagnostics(self: Self, writer: anytype) !void {
    if (!self.initialized)
        @panic("Attempting to call Script.reportDiagnostics on uninitialized script object");

    for (self.diagnostics().diagnostics.items) |diagnostic| {
        const line = try self.parser.lexer.getLineForLocation(diagnostic.location);

        try writer.print("{s}:{}: {s}: {s}\n", .{ self.file_path, diagnostic.location.format(), @tagName(diagnostic.level), diagnostic.message });
        try writer.print("{s}\n", .{line});
        try writer.writeByteNTimes(' ', diagnostic.location.column - 1);
        try writer.writeAll("^\n");
    }
}
