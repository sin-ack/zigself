// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("./object.zig");
const InterpreterContext = @import("./interpreter.zig").InterpreterContext;

pub const SelfRuntimeError = error{RuntimeError};

/// Raises an error. If this error bubbles up to executeScript, it will be
/// displayed as a runtime error.
///
/// `context.current_error` should be freed by whoever handles the error.
pub fn raiseError(allocator: *Allocator, context: *InterpreterContext, comptime fmt: []const u8, args: anytype) (SelfRuntimeError || Allocator.Error) {
    context.current_error = try std.fmt.allocPrint(allocator, fmt, args);
    return SelfRuntimeError.RuntimeError;
}

/// Using the given activation object stack, print a stack trace to stderr.
/// The stack trace is indented with two spaces.
pub fn printTraceFromActivationStack(stack: []Object.Ref) void {
    if (stack.len == 0) {
        return;
    }

    var i = @intCast(isize, stack.len - 1);
    while (i >= 0) : (i -= 1) {
        const activation = stack[@intCast(usize, i)];
        const message_name = activation.value.content.Activation.message_name;
        const message_script = activation.value.content.Activation.message_script;
        const message_range = activation.value.content.Activation.message_range;
        const activation_object = activation.value.content.Activation.activation_object;
        const receiver = activation_object.value.lookup(null, "_parent", .Value) catch unreachable;

        std.debug.print("  at {s} ({s}:{}) (receiver is ", .{ message_name, message_script.value.file_path, message_range.format() });
        if (receiver) |obj| {
            std.debug.print("<*{d}>", .{obj.value.id});
        } else {
            std.debug.print("<gone>", .{});
        }
        std.debug.print(")\n", .{});

        const source_line = message_script.value.parser.lexer.getLineForLocation(message_range.start) catch unreachable;
        // NOTE: The spaces are to get the arrow aligned with the source code
        std.debug.print("  \x1b[37m{d:<3} |\x1b[0m {s}\n\x1b[92m        ", .{ message_range.start.line, source_line });

        // FIXME: Make this nicer
        const writer = std.io.getStdErr().writer();

        writer.writeByteNTimes(' ', message_range.start.column - 1) catch unreachable;
        if (message_range.start.line == message_range.end.line) {
            writer.writeByteNTimes('^', std.math.max(1, message_range.end.column - message_range.start.column)) catch unreachable;
        } else {
            writer.writeByteNTimes('^', source_line.len - message_range.start.column + 1) catch unreachable;
            writer.writeByteNTimes('.', 3) catch unreachable;
        }
        std.debug.print("\x1b[0m\n", .{});
    }
}
