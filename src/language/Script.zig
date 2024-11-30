// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const ref_counted = @import("../utility/ref_counted.zig");
const AST = @import("./ast.zig");
const Parser = @import("./Parser.zig");
const Diagnostics = @import("./Diagnostics.zig");
const Location = @import("./Location.zig");

const Script = @This();
pub const Ref = ref_counted.RefPtr(Script);

ast_root: ?AST.ScriptNode = null,

ref: ref_counted.RefCount,
allocator: Allocator,
file_path: []const u8,
// Not owned by this member. Same as file_path for file scripts and the owner script for
// string scripts.
running_path: []const u8,
parser: *Parser,

pub fn createFromFilePath(allocator: Allocator, file_path: []const u8) !Ref {
    const self = try allocator.create(Script);
    errdefer allocator.destroy(self);

    try self.initFromFilePath(allocator, file_path);
    return Ref.adopt(self);
}

pub fn createFromString(allocator: Allocator, running_path: []const u8, contents: []const u8) !Ref {
    const self = try allocator.create(Script);
    try self.initFromString(allocator, running_path, contents);
    return Ref.adopt(self);
}

pub fn destroy(self: *Script) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn initFromFilePath(self: *Script, allocator: Allocator, file_path: []const u8) !void {
    const file_path_copy = try allocator.dupe(u8, file_path);
    errdefer allocator.free(file_path_copy);

    self.file_path = file_path_copy;
    self.running_path = file_path_copy;

    self.parser = try Parser.createFromFile(allocator, file_path);
    errdefer self.parser.destroy(allocator);
    try self.initCommon(allocator);
}

fn initFromString(self: *Script, allocator: Allocator, running_path: []const u8, contents: []const u8) !void {
    const file_path_copy = try allocator.dupe(u8, "<a string>");
    errdefer allocator.free(file_path_copy);

    self.file_path = file_path_copy;
    self.running_path = running_path;

    self.parser = try Parser.createFromString(allocator, contents);
    try self.initCommon(allocator);
}

fn initCommon(self: *Script, allocator: Allocator) !void {
    self.ref = .{};
    self.allocator = allocator;
}

fn deinit(self: *Script) void {
    self.allocator.free(self.file_path);
    self.parser.destroy();
    if (self.ast_root) |*ast_root| {
        ast_root.deinit(self.allocator);
    }
}

/// Parse the script and expose it in `self.ast_root`. Return whether the script
/// was parsed without any error diagnostics being generated.
pub fn parseScript(self: *Script) !bool {
    self.ast_root = try self.parser.parseScript();

    for (self.diagnostics().diagnostics.items) |diagnostic| {
        if (diagnostic.level == .Error) {
            return false;
        }
    }

    return true;
}

pub fn diagnostics(self: Script) Diagnostics {
    return self.parser.diagnostics;
}

pub fn reportDiagnostics(self: Script, writer: anytype) !void {
    for (self.diagnostics().diagnostics.items) |diagnostic| {
        const line = self.parser.buffer[diagnostic.location.line_start..diagnostic.location.line_end];

        try writer.print("{s}:{}: {s}: {s}\n", .{ self.file_path, diagnostic.location, @tagName(diagnostic.level), diagnostic.message });
        try writer.print("{s}\n", .{line});
        try writer.writeByteNTimes(' ', diagnostic.location.column - 1);
        try writer.writeAll("^\n");
    }
}

pub fn offsetToLocation(self: Script, offset: usize) Location {
    return self.parser.offsetToLocation(offset);
}

pub fn getSourceLine(self: Script, location: Location) []const u8 {
    return self.parser.buffer[location.line_start..location.line_end];
}
