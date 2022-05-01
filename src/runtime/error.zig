// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Activation = @import("./Activation.zig");
const SourceRange = @import("./SourceRange.zig");

fn writeTraceForFrame(message_name: []const u8, source_range: SourceRange) void {
    std.debug.print("  at {s} ({})\n", .{ message_name, source_range });

    const range = source_range.range;
    const source_line = source_range.getStartLine() catch unreachable;
    // NOTE: The spaces are to get the arrow aligned with the source code
    std.debug.print("  \x1b[37m{d:<3} |\x1b[0m {s}\n\x1b[92m        ", .{ source_range.range.start.line, source_line });

    // FIXME: Make this nicer
    const writer = std.io.getStdErr().writer();

    writer.writeByteNTimes(' ', range.start.column - 1) catch unreachable;
    if (range.start.line == range.end.line) {
        writer.writeByteNTimes('^', std.math.max(1, range.end.column - range.start.column)) catch unreachable;
    } else {
        writer.writeByteNTimes('^', source_line.len - range.start.column + 1) catch unreachable;
        writer.writeByteNTimes('.', 3) catch unreachable;
    }
    std.debug.print("\x1b[0m\n", .{});
}

/// Using the given activation object stack, print a stack trace to stderr until
/// the end or the activation given at `until` is reached.
/// The stack trace is indented with two spaces.
pub fn printTraceFromActivationStackUntil(stack: []Activation, first_source_range: SourceRange, until: ?*Activation) void {
    if (stack.len == 0) {
        return;
    }

    var called_from = first_source_range;

    var i = @intCast(isize, stack.len - 1);
    while (i >= 0) : (i -= 1) {
        const activation_ptr = &stack[@intCast(usize, i)];

        if (until) |until_ptr| {
            if (until_ptr == activation_ptr) break;
        }

        const message_name = activation_ptr.creator_message.asByteArray().getValues();
        writeTraceForFrame(message_name, called_from);

        called_from = activation_ptr.created_from;
    }
}

/// Using the given activation object stack, print a stack trace to stderr.
/// The stack trace is indented with two spaces.
pub fn printTraceFromActivationStack(stack: []Activation, first_source_range: SourceRange) void {
    printTraceFromActivationStackUntil(stack, first_source_range, null);
}
