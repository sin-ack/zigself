// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const zig_args = @import("zig-args");

const Script = @import("./language/script.zig");
const ASTPrinter = @import("./language/ast_printer.zig");

const Object = @import("./runtime/object.zig");
const interpreter = @import("./runtime/interpreter.zig");
const environment = @import("./runtime/environment.zig");

const ArgumentSpec = struct {
    help: bool = false,
    @"dump-ast": bool = false,

    pub const shorthands = .{
        .h = "help",
        .A = "dump-ast",
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
    \\  --dump-ast, -A    Dump the AST tree for the input file and exit.
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

    const file_path_sentinel = arguments.positionals[0];
    const file_path = std.mem.sliceTo(file_path_sentinel, 0);

    var did_pass_script_to_interpreter = false;
    var entrypoint_script = try Script.createFromFilePath(allocator, file_path);
    errdefer if (!did_pass_script_to_interpreter) entrypoint_script.unref();

    const did_parse_without_errors = try entrypoint_script.value.parseScript();
    try entrypoint_script.value.reportDiagnostics(std.io.getStdErr().writer());

    if (arguments.options.@"dump-ast") {
        var printer = ASTPrinter.init(2, allocator);
        defer printer.deinit();
        printer.dumpScript(entrypoint_script.value.ast_root.?);
        return 0;
    }

    // If we had parsing errors then we cannot proceed further.
    if (!did_parse_without_errors) {
        return 1;
    }

    Object.setupObjectRefTracker(allocator);
    defer Object.teardownObjectRefTrackerAndReportAliveRefs();

    var lobby = try environment.prepareRuntimeEnvironment(allocator);
    defer lobby.unref();
    defer environment.teardownGlobalObjects();

    did_pass_script_to_interpreter = true;
    if (try interpreter.executeScript(allocator, entrypoint_script, lobby)) |result| {
        result.unref();
    }

    return 0;
}
