// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Script = @import("../../language/script.zig");
const AstGen = @import("../AstGen.zig");
const CodeGen = @import("../CodeGen.zig");
const Completion = @import("../Completion.zig");
const interpreter = @import("../interpreter.zig");
const error_set_utils = @import("../../utility/error_set.zig");
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

const runtime_error = @import("../error.zig");

/// Return the static "nil" slots object.
pub fn Nil(context: PrimitiveContext) !?Completion {
    return Completion.initNormal(context.vm.nil());
}

/// Return the given path relative to the current activation's definition
/// script's running path. The caller must free the returned slice.
fn getRelativePathToScript(context: PrimitiveContext, path: []const u8) ![]const u8 {
    const current_activation = context.actor.activation_stack.getCurrent();
    const current_script = current_activation.definitionExecutable().value.definition_script.value;
    const running_script_path = current_script.running_path;

    const paths_to_join = &[_][]const u8{ std.fs.path.dirname(running_script_path) orelse ".", path };
    const target_path = try std.fs.path.join(context.vm.allocator, paths_to_join);
    return target_path;
}

/// Run the given script file, and return the result of the last expression.
pub fn RunScript(context: PrimitiveContext) !?Completion {
    const receiver = context.receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected ByteArray for the receiver of _RunScript", .{});
    }

    // FIXME: Find a way to handle errors here. These hacks are nasty.

    const receiver_byte_array = receiver.asObject().asByteArrayObject();
    const target_path = try getRelativePathToScript(context, receiver_byte_array.getValues());
    defer context.vm.allocator.free(target_path);

    var script = Script.createFromFilePath(context.vm.allocator, target_path) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |e| return try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "An unexpected error was raised from script.initInPlaceFromFilePath: {s}",
            .{@errorName(e)},
        ),
    };
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |e| return try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "An unexpected error was raised from script.parseScript: {s}",
            .{@errorName(e)},
        ),
    };

    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;
    if (!did_parse_without_errors) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Failed parsing the script passed to _RunScript", .{});
    }

    const ast_executable = AstGen.generateExecutableFromScript(context.vm.allocator, script) catch |err| switch (err) {
        error.AstGenFailure => return try Completion.initRuntimeError(context.vm, context.source_range, "Code generation for the script passed to _RunScript failed", .{}),
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer ast_executable.destroy();

    const executable = try CodeGen.lowerExecutable(context.vm.allocator, ast_executable);
    defer executable.unref();

    try executable.value.pushSubEntrypointActivation(context.vm, context.source_range.executable, context.target_location, &context.actor.activation_stack);
    return null;
}

pub fn EvaluateStringIfFail(context: PrimitiveContext) !?Completion {
    const receiver = context.receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected ByteArray for the receiver of _RunScript", .{});
    }

    const receiver_byte_array = receiver.asObject().asByteArrayObject();
    const running_script_path = context.actor.activation_stack.getCurrent().definitionExecutable().value.definition_script.value.running_path;

    var script = try Script.createFromString(context.vm.allocator, running_script_path, receiver_byte_array.getValues());
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |e| {
            // TODO: Instead of printing this error like this, pass it to the failure block.
            std.debug.print("An unexpected error was raised from script.parseScript: {s}\n", .{@errorName(e)});
            return try interpreter.sendMessage(context.vm, context.actor, context.arguments[0], "value", context.target_location, context.source_range);
        },
    };

    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;
    if (!did_parse_without_errors) {
        // TODO: Pass error information to the failure block.
        return try interpreter.sendMessage(context.vm, context.actor, context.arguments[0], "value", context.target_location, context.source_range);
    }

    const ast_executable = AstGen.generateExecutableFromScript(context.vm.allocator, script) catch |err| switch (err) {
        error.AstGenFailure => {
            // TODO: Pass error information to the failure block.
            std.debug.print("Code generation for the script passed to _EvaluateStringIfFail: failed", .{});
            return try interpreter.sendMessage(context.vm, context.actor, context.arguments[0], "value", context.target_location, context.source_range);
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer ast_executable.destroy();

    const executable = try CodeGen.lowerExecutable(context.vm.allocator, ast_executable);
    defer executable.unref();

    const stack_snapshot = context.vm.takeStackSnapshot();
    const activation_before_script = context.actor.activation_stack.getCurrent();
    try executable.value.pushSubEntrypointActivation(context.vm, context.source_range.executable, context.target_location, &context.actor.activation_stack);

    var completion = try context.actor.executeActivationStack(context.vm, activation_before_script);
    switch (completion.data) {
        .RuntimeError => |err| {
            defer completion.deinit(context.vm);

            // TODO: Pass error information to failure block
            std.debug.print("Received error while evaluating string: {s}\n", .{err.message});
            runtime_error.printTraceFromActivationStackUntil(context.actor.activation_stack.getStack(), err.source_range, activation_before_script);

            context.actor.activation_stack.restoreTo(activation_before_script);
            context.vm.restoreStackSnapshot(stack_snapshot);

            return try interpreter.sendMessage(context.vm, context.actor, context.arguments[0], "value", context.target_location, context.source_range);
        },
        else => return completion,
    }
}

/// Raise the argument as an error. The argument must be a byte vector.
pub fn Error(context: PrimitiveContext) !?Completion {
    var argument = context.arguments[0];
    if (!(argument.isObjectReference() and argument.asObject().isByteArrayObject())) {
        return try Completion.initRuntimeError(context.vm, context.source_range, "Expected ByteArray as _Error: argument", .{});
    }

    return try Completion.initRuntimeError(context.vm, context.source_range, "Error raised in Self code: {s}", .{argument.asObject().asByteArrayObject().getValues()});
}

/// Restarts the current method, executing it from the first statement.
/// This primitive is intended to be used internally only.
pub fn Restart(context: PrimitiveContext) !?Completion {
    _ = context;
    return Completion.initRestart();
}
