// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Activation = @import("./Activation.zig");
const SourceRange = @import("./SourceRange.zig");

fn writeTraceForFrame(message_name: []const u8, source_range: SourceRange) void {
    const range = source_range.getLocationRange();
    const source_line = source_range.getStartLine() catch unreachable;

    // FIXME: Take a writer here instead printing to stderr directly.
    // FIXME: Propagate errors
    var writer_buffer: [4096]u8 = undefined;
    var file_writer = std.fs.File.stderr().writer(&writer_buffer);
    const writer = &file_writer.interface;

    writer.print("  at {s} ({f})\n", .{ message_name, source_range }) catch unreachable;
    // NOTE: The spaces are to get the arrow aligned with the source code
    writer.print("  \x1b[37m{d:<3} |\x1b[0m {s}\n\x1b[92m        ", .{ range.start.line, source_line }) catch unreachable;

    writer.splatByteAll(' ', range.start.column - 1) catch unreachable;
    if (range.start.line == range.end.line) {
        writer.splatByteAll('^', @max(1, range.end.column - range.start.column)) catch unreachable;
    } else {
        writer.splatByteAll('^', source_line.len - range.start.column + 1) catch unreachable;
        writer.splatByteAll('.', 3) catch unreachable;
    }
    writer.print("\x1b[0m\n", .{}) catch unreachable;
    writer.flush() catch unreachable;
}

/// Using the given activation object stack, print a stack trace to stderr until
/// the end or the activation given at `until` is reached.
/// The stack trace is indented with two spaces.
pub fn printTraceFromActivationStackUntil(stack: []Activation, first_source_range: SourceRange, until: ?*Activation) void {
    if (stack.len == 0) {
        return;
    }

    var called_from = first_source_range;

    var i: isize = @intCast(stack.len - 1);
    while (i >= 0) : (i -= 1) {
        const activation_ptr = &stack[@intCast(i)];

        if (until) |until_ptr| {
            if (until_ptr == activation_ptr) break;
        }

        const message_name = activation_ptr.creator_message.getValues();
        writeTraceForFrame(message_name, called_from);

        called_from = activation_ptr.created_from;
    }
}

/// Using the given activation object stack, print a stack trace to stderr.
/// The stack trace is indented with two spaces.
pub fn printTraceFromActivationStack(stack: []Activation, first_source_range: SourceRange) void {
    printTraceFromActivationStackUntil(stack, first_source_range, null);
}
