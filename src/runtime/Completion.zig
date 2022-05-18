// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./Heap.zig");
const Value = @import("./value.zig").Value;
const Activation = @import("./Activation.zig");
const interpreter = @import("./interpreter.zig");
const SourceRange = @import("./SourceRange.zig");
const VirtualMachine = @import("./VirtualMachine.zig");

const Self = @This();

/// The types of completion that can happen.
pub const CompletionData = union(enum) {
    /// A normal completion which returns a simple value.
    Normal: Value,
    /// A runtime error.
    RuntimeError: RuntimeErrorCompletionData,
    /// A completion telling the current method or block to restart its execution
    /// from the first statement.
    Restart: void,
};

/// The data that's required to perform a runtime error return.
pub const RuntimeErrorCompletionData = struct {
    /// The error message. Should be deinitialized by the handler.
    message: []const u8,
    /// The source range at which this error occurred.
    source_range: SourceRange,
};

data: CompletionData,

/// Initializes a new normal completion with the given value.
pub fn initNormal(value: Value) Self {
    return .{ .data = .{ .Normal = value } };
}

/// Creates a new runtime error completion with the given format string and parameters.
/// Copies the source range object.
pub fn initRuntimeError(vm: *VirtualMachine, source_range: SourceRange, comptime fmt: []const u8, args: anytype) Allocator.Error!Self {
    const error_message = try std.fmt.allocPrint(vm.allocator, fmt, args);
    return Self{ .data = .{ .RuntimeError = .{ .message = error_message, .source_range = source_range.copy() } } };
}

/// Creates a restart completion.
pub fn initRestart() Self {
    return .{ .data = .{ .Restart = .{} } };
}

/// Deinitializes values in this completion as necessary.
pub fn deinit(self: *Self, vm: *VirtualMachine) void {
    switch (self.data) {
        .Normal, .Restart => {},
        .RuntimeError => |*err| {
            vm.allocator.free(err.message);
            err.source_range.deinit();
        },
    }
}

pub fn isNormal(self: Self) bool {
    return self.data == .Normal;
}
