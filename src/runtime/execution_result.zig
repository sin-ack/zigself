// Copyright (c) 2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Value = @import("value.zig").Value;
const RuntimeError = @import("RuntimeError.zig");

/// The result of running an instruction. This value is handled after each
/// instruction execution at the opcode level.
pub const ExecutionResult = union(enum) {
    /// The instruction has completed normally without anything special.
    Normal,
    /// The execution has resolved to a certain value. This is interpreted in
    /// two ways:
    ///
    /// 1. If this is received as the result of a message send (whether
    /// primitive or user), it is treated as the immediate result of that
    /// message send and execution continues as normal.
    /// 2. If this is received at the top level of the interpreter, it is
    /// treated as the actor finishing normally with a value and the interpreter
    /// exits.
    Resolved: Value,
    /// The current actor has yielded in some manner, and we should switch to
    /// the newly activated actor.
    ActorYielded,
    /// We have entered a new activation, or exited the current one.
    ActivationChanged,
    /// The current activation has restarted through the use of _Restart.
    Restarted,
    /// An unrecoverable runtime error has occurred.
    RuntimeError: RuntimeError,

    pub fn normal() ExecutionResult {
        return .{ .Normal = {} };
    }

    pub fn resolve(value: Value) ExecutionResult {
        return .{ .Resolved = value };
    }

    pub fn yield() ExecutionResult {
        return .{ .ActorYielded = {} };
    }

    pub fn changeActivation() ExecutionResult {
        return .{ .ActivationChanged = {} };
    }

    pub fn restart() ExecutionResult {
        return .{ .Restarted = {} };
    }

    pub fn runtimeError(err: RuntimeError) ExecutionResult {
        return .{ .RuntimeError = err };
    }
};
