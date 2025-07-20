// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const zig_args = @import("zig-args");
const tracy = @import("tracy");

const zigself = @import("zigself");
const Script = zigself.language.Script;
const ASTPrinter = zigself.language.ASTPrinter;
const VirtualMachine = zigself.runtime.VirtualMachine;

const ArgumentSpec = struct {
    help: bool = false,
    @"dump-ast": bool = false,

    pub const shorthands = .{
        .h = "help",
        .A = "dump-ast",
    };
};

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
    const stderr = std.fs.File.stderr();
    _ = try stderr.write(usage_text);
}

const GPA = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !u8 {
    var gpa = GPA{};
    defer _ = gpa.deinit();

    var tracy_allocator = if (tracy.enable_allocation) tracy.tracyAllocator(gpa.allocator()) else {};
    const allocator = if (tracy.enable_allocation) tracy_allocator.allocator() else gpa.allocator();

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
        const stderr = std.fs.File.stderr();
        _ = try stderr.write("Error: Must provide exactly one argument\n");
        try printUsage();
        return 1;
    }

    const file_path_sentinel = arguments.positionals[0];
    const file_path = std.mem.sliceTo(file_path_sentinel, 0);

    var entrypoint_script = try Script.createFromFilePath(allocator, file_path);
    defer entrypoint_script.unref();

    const did_parse_without_errors = try entrypoint_script.value.parseScript();

    var writer_buffer: [4096]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&writer_buffer);
    try entrypoint_script.value.reportDiagnostics(&writer.interface);

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

    var vm = try VirtualMachine.create(allocator);
    defer vm.destroy();

    const result = try vm.executeEntrypointScript(entrypoint_script);
    return if (result == null) 1 else 0;
}
