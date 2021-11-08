const std = @import("std");
const Lexer = @import("./lexer.zig");

const Self = @This();

// TODO: When Zig's RLS actually starts working properly, make this a static
//       function.
pub fn initInPlaceFromFilePath(self: *Self, file_path: [*:0]const u8, allocator: *std.mem.Allocator) !void {
    if (self.initialized)
        @panic("Attempting to initialize already-initialized parser");

    self.lexer = Lexer{};
    try self.lexer.initInPlaceFromFilePath(file_path, allocator);

    self.allocator = allocator;
    self.initialized = true;
}

pub fn deinit(self: *Self) void {
    if (!self.initialized)
        @panic("Attempting to deinitialize uninitialized parser");

    self.lexer.deinit();
}

pub fn parse(self: *Self) !void {
    if (!self.initialized)
        @panic("Attempting to call Parser.parse on uninitialized parser");

    while (true) {
        const token = try self.lexer.nextToken();
        if (token == .EOF) {
            return;
        }
        std.debug.print("Got token: {s}\n", .{token.toString()});
        switch (token) {
            .String => {
                std.debug.print("  Value: {s}\n", .{token.String});
                self.allocator.free(token.String);
            },
            .Identifier => std.debug.print("  Value: {s}\n", .{token.Identifier}),
            .Integer => std.debug.print("  Value: {}\n", .{token.Integer}),
            .FloatingPoint => std.debug.print("  Value: {}\n", .{token.FloatingPoint}),
            else => {},
        }
    }
}

initialized: bool = false,
allocator: *std.mem.Allocator = undefined,
lexer: Lexer = undefined,
