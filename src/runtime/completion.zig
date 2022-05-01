// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Activation = @import("./activation.zig");
const interpreter = @import("./interpreter.zig");
const SourceRange = @import("./SourceRange.zig");
const VirtualMachine = @import("./VirtualMachine.zig");

const Self = @This();

/// The types of completion that can happen.
pub const CompletionData = union(enum) {
    /// A normal completion which returns a simple value.
    Normal: Value,
    /// A non-local return, which will rise through the call stack until it
    /// reaches the method in which the block was defined, where it will become
    /// a normal completion.
    NonlocalReturn: NonlocalReturnCompletionData,
    /// A runtime error.
    RuntimeError: RuntimeErrorCompletionData,
    /// A completion telling the current method or block to restart its execution
    /// from the first statement.
    Restart: void,
};

/// The data that's required to perform a nonlocal return.
pub const NonlocalReturnCompletionData = struct {
    /// The activation at which this non-local return should become the
    /// regular return value.
    target_activation: Activation.ActivationRef,
    // FIXME: We shouldn't allocate while a non-local return is bubbling,
    //        so this tracking is pointless. Turn it into a regular Value.
    /// The value that should be returned when the non-local return reaches
    /// its destination.
    value: Heap.Tracked,
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

/// Initializes a new non-local return completion.
pub fn initNonlocalReturn(target_activation: Activation.ActivationRef, value: Heap.Tracked) Self {
    return .{ .data = .{ .NonlocalReturn = .{ .target_activation = target_activation, .value = value } } };
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
        .NonlocalReturn => |nonlocal_return| {
            nonlocal_return.value.untrack(vm.heap);
        },
        .RuntimeError => |*err| {
            vm.allocator.free(err.message);
            err.source_range.deinit();
        },
    }
}

pub fn isNormal(self: Self) bool {
    return self.data == .Normal;
}
