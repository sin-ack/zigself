// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const ref_counted = @import("../utility/ref_counted.zig");
const AST = @import("./ast.zig");
const Parser = @import("./parser.zig");
const Diagnostics = @import("./diagnostics.zig");

const Self = @This();
pub const Ref = ref_counted.RefPtr(Self);

ast_root: ?AST.ScriptNode = null,

ref: ref_counted.RefCount,
allocator: *Allocator,
file_path: []const u8,
parser: Parser,

pub fn createFromFilePath(allocator: *Allocator, file_path: []const u8) !Ref {
    const self = try allocator.create(Self);
    try self.initFromFilePath(allocator, file_path);
    return Ref.adopt(self);
}

pub fn destroy(self: *Self) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn initFromFilePath(self: *Self, allocator: *Allocator, file_path: []const u8) !void {
    var file_path_copy = try allocator.dupe(u8, file_path);
    errdefer allocator.free(file_path_copy);

    self.ref = .{};
    self.file_path = file_path_copy;
    self.allocator = allocator;
    self.parser = .{};

    try self.parser.initInPlaceFromFilePath(file_path, allocator);
}

fn deinit(self: *Self) void {
    self.allocator.free(self.file_path);
    self.parser.deinit();
    if (self.ast_root) |*ast_root| {
        ast_root.deinit(self.allocator);
    }
}

/// Parse the script and expose it in `self.ast_root`. Return whether the script
/// was parsed without any error diagnostics being generated.
pub fn parseScript(self: *Self) !bool {
    self.ast_root = try self.parser.parse();

    for (self.diagnostics().diagnostics.items) |diagnostic| {
        if (diagnostic.level == .Error) {
            return false;
        }
    }

    return true;
}

pub fn diagnostics(self: Self) Diagnostics {
    return self.parser.diagnostics;
}

pub fn reportDiagnostics(self: Self, writer: anytype) !void {
    for (self.diagnostics().diagnostics.items) |diagnostic| {
        const line = try self.parser.lexer.getLineForLocation(diagnostic.location);

        try writer.print("{s}:{}: {s}: {s}\n", .{ self.file_path, diagnostic.location.format(), @tagName(diagnostic.level), diagnostic.message });
        try writer.print("{s}\n", .{line});
        try writer.writeByteNTimes(' ', diagnostic.location.column - 1);
        try writer.writeAll("^\n");
    }
}
