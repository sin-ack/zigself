// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Activation = @import("./activation.zig");

/// Using the given activation object stack, print a stack trace to stderr.
/// The stack trace is indented with two spaces.
pub fn printTraceFromActivationStack(stack: []Activation) void {
    if (stack.len == 0) {
        return;
    }

    var i = @intCast(isize, stack.len - 1);
    while (i >= 0) : (i -= 1) {
        const activation = stack[@intCast(usize, i)];
        const context = activation.creation_context;

        // const activation_object = activation.activation_object;
        // const receiver = activation_object.asObject().asActivationObject().receiver;

        std.debug.print("  at {s} ({})\n", .{ context.message.getValue().asByteVector().getValues(), context.source_range.format() });

        const range = context.source_range.range;
        const source_line = context.source_range.getStartLine() catch unreachable;
        // NOTE: The spaces are to get the arrow aligned with the source code
        std.debug.print("  \x1b[37m{d:<3} |\x1b[0m {s}\n\x1b[92m        ", .{ context.source_range.range.start.line, source_line });

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
}
