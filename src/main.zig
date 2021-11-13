// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const zig_args = @import("zig-args");

const Parser = @import("./language/parser.zig");
const AST = @import("./language/ast.zig");

const ArgumentSpec = struct {
    help: bool = false,

    pub const shorthands = .{
        .h = "help",
    };
};

const Allocator = std.heap.GeneralPurposeAllocator(.{});

const usage_text =
    \\Usage: self [--help] <path>
    \\
    \\This is the Self interpreter.
    \\
    \\Arguments:
    \\  path              File path for the entrypoint of the Self program.
    \\Options:
    \\  --help, -h        Print this help output.
    \\
;

fn printUsage() !void {
    const stderr = std.io.getStdErr();
    _ = try stderr.write(usage_text);
}

pub fn main() !u8 {
    var general_purpose_allocator = Allocator{};
    defer _ = general_purpose_allocator.deinit();
    var allocator = &general_purpose_allocator.allocator;

    const arguments = zig_args.parseForCurrentProcess(ArgumentSpec, allocator, .print) catch {
        try printUsage();
        return 1;
    };
    defer arguments.deinit();

    if (arguments.options.help) {
        try printUsage();
        return 0;
    }

    if (arguments.positionals.len != 1) {
        const stderr = std.io.getStdErr();
        _ = try stderr.write("Error: Must provide exactly one argument\n");
        try printUsage();
        return 1;
    }

    const file_path = arguments.positionals[0];

    var parser = Parser{};
    try parser.initInPlaceFromFilePath(file_path, allocator);
    defer parser.deinit();

    var script_node = try parser.parse();
    defer script_node.deinit(allocator);

    for (parser.diagnostics.diagnostics.items) |diagnostic| {
        std.debug.print("{}: {s}: {s}\n", .{ diagnostic.location.format(), @tagName(diagnostic.level), diagnostic.message });
    }

    var printer = AST.ASTPrinter.init(2, allocator);
    defer printer.deinit();
    script_node.dumpTree(&printer);

    return 0;
}
