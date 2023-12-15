// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! An unrecoverable runtime error. Errors of this kind are raised by user code
//! when invariants are violated, and by primitives. Runtime errors put the
//! actor in the RuntimeError state and pauses it, which is useful for attaching
//! a debugger and seeing the problem.

const std = @import("std");
const Allocator = std.mem.Allocator;

const context = @import("context.zig");
const Activation = @import("./Activation.zig");
const SourceRange = @import("./SourceRange.zig");
const VirtualMachine = @import("./VirtualMachine.zig");

/// The error message. This can be either Literal (non-owned) or Formatted (owned).
message: Message,
/// The source range at which this error occurred.
source_range: SourceRange,

const RuntimeError = @This();

const Message = union(enum) {
    Literal: []const u8,
    Formatted: []const u8,
};

/// Initialize this error with a literal message. Copies the source range
/// object.
pub fn initLiteral(source_range: SourceRange, message: []const u8) RuntimeError {
    return RuntimeError{
        .message = .{ .Literal = message },
        .source_range = source_range.copy(),
    };
}

/// Create a new runtime error by formatting an error message. An error message
/// is allocated. Copies the source range object.
pub fn initFormatted(source_range: SourceRange, comptime fmt: []const u8, args: anytype) Allocator.Error!RuntimeError {
    const message = try std.fmt.allocPrint(context.getVM().allocator, fmt, args);
    return RuntimeError{
        .message = .{ .Formatted = message },
        .source_range = source_range.copy(),
    };
}

/// Createa new runtime error by formatting an error message at comptime. No
/// allocation is done at runtime. Copies the source range object.
pub fn initFormattedComptime(source_range: SourceRange, comptime fmt: []const u8, comptime args: anytype) RuntimeError {
    const message = std.fmt.comptimePrint(fmt, args);
    return RuntimeError{
        .message = .{ .Literal = message },
        .source_range = source_range.copy(),
    };
}

pub fn deinit(self: *RuntimeError, allocator: Allocator) void {
    switch (self.message) {
        .Literal => {},
        .Formatted => |m| allocator.free(m),
    }
    self.source_range.deinit();
}

pub fn getMessage(self: RuntimeError) []const u8 {
    return switch (self.message) {
        inline else => |m| m,
    };
}
