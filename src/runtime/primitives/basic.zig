// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const tracy = @import("tracy");

const Script = @import("../../language/Script.zig");
const AstGen = @import("../bytecode/AstGen.zig");
const CodeGen = @import("../bytecode/CodeGen.zig");
const Selector = @import("../Selector.zig");
const interpreter = @import("../interpreter.zig");
const RuntimeError = @import("../RuntimeError.zig");
const ExecutionResult = @import("../execution_result.zig").ExecutionResult;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

const stack_trace = @import("../stack_trace.zig");

/// Return the static "nil" slots object.
pub fn Nil(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    return ExecutionResult.resolve(context.vm.global_nil);
}

/// Return the given path relative to the current activation's definition
/// script's running path. The caller must free the returned slice.
fn getRelativePathToScript(context: *PrimitiveContext, path: []const u8) ![]const u8 {
    const current_activation = context.actor.activation_stack.getCurrent();
    const current_script = current_activation.definitionExecutable().value.definition_script.value;
    const running_script_path = current_script.running_path;

    const paths_to_join = &[_][]const u8{ std.fs.path.dirname(running_script_path) orelse ".", path };
    const target_path = try std.fs.path.join(context.vm.allocator, paths_to_join);
    return target_path;
}

/// Run the given script file, and return the result of the last expression.
pub fn RunScript(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const arguments = context.getArguments("_RunScript:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);

    // FIXME: Find a way to handle errors here. These hacks are nasty.

    const target_path = try getRelativePathToScript(context, receiver.getValues());
    defer context.vm.allocator.free(target_path);

    var script = Script.createFromFilePath(context.vm.allocator, target_path) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |e| return ExecutionResult.runtimeError(try RuntimeError.initFormatted(
            context.source_range,
            "An unexpected error was raised from script.initInPlaceFromFilePath: {s}",
            .{@errorName(e)},
        )),
    };
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err| switch (err) {
        error.OutOfMemory => return err,
    };

    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;
    if (!did_parse_without_errors) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "Failed parsing the script passed to _RunScript",
        ));
    }

    const ast_executable = AstGen.generateExecutableFromScript(context.vm.allocator, script) catch |err| switch (err) {
        error.AstGenFailure => return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "Code generation for the script passed to _RunScript failed",
        )),
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer ast_executable.unref();

    const executable = try CodeGen.lowerExecutable(context.vm.allocator, ast_executable.value);
    defer executable.unref();

    // Advance the instruction for the activation that will be returned to.
    _ = context.actor.activation_stack.getCurrent().advanceInstruction();

    try context.actor.activation_stack.pushSubEntrypointActivation(context.target_location, executable);
    return ExecutionResult.changeActivation();
}

pub fn EvaluateStringIfFail(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    // FIXME: _EvaluateStringIfFail: should be replaced with _EvaluateString and
    //       fail the current actor the same way _RunScript does. The REPL
    //       should create a new actor for each string evaluation.
    if (context.vm.isInActorMode())
        @panic("TODO make '_EvaluateStringIfFail:' work with actor mode");

    const arguments = context.getArguments("_EvaluateStringIfFail:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    const failure_block = arguments.getValue(0);

    const running_script_path = context.actor.activation_stack.getCurrent().definitionExecutable().value.definition_script.value.running_path;
    var script = try Script.createFromString(context.vm.allocator, running_script_path, receiver.getValues());
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };

    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;
    if (!did_parse_without_errors) {
        // TODO: Pass error information to the failure block.
        return try interpreter.sendMessage(
            context.vm,
            failure_block,
            Selector.well_known.value,
            null,
            context.target_location,
            context.source_range,
        );
    }

    const ast_executable = AstGen.generateExecutableFromScript(context.vm.allocator, script) catch |err| switch (err) {
        error.AstGenFailure => {
            // TODO: Pass error information to the failure block.
            std.debug.print("Code generation for the script passed to _EvaluateStringIfFail: failed", .{});
            return try interpreter.sendMessage(
                context.vm,
                failure_block,
                Selector.well_known.value,
                null,
                context.target_location,
                context.source_range,
            );
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer ast_executable.unref();

    const executable = try CodeGen.lowerExecutable(context.vm.allocator, ast_executable.value);
    defer executable.unref();

    const stack_snapshot = context.vm.takeStackSnapshot();
    var activation_before_script = context.actor.activation_stack.getCurrent();
    try context.actor.activation_stack.pushSubEntrypointActivation(context.target_location, executable);

    const activation_before_script_ref = activation_before_script.takeRef(context.actor.activation_stack);

    var actor_result = try context.actor.executeUntil(activation_before_script_ref);
    switch (actor_result) {
        .Switched => unreachable,
        .Finished => |value| return ExecutionResult.resolve(value),
        .RuntimeError => |*err| {
            defer err.deinit(context.vm.allocator);

            // Refresh activation pointer
            activation_before_script = activation_before_script_ref.get(context.actor.activation_stack).?;

            // TODO: Pass error information to failure block
            std.debug.print("Received error while evaluating string: {s}\n", .{err.getMessage()});
            stack_trace.printTraceFromActivationStackUntil(context.actor.activation_stack.getStack(), err.source_range, activation_before_script);

            context.actor.activation_stack.restoreTo(activation_before_script);
            context.vm.restoreStackSnapshot(stack_snapshot);

            const block_result = try interpreter.sendMessage(
                context.vm,
                failure_block,
                Selector.well_known.value,
                null,
                context.target_location,
                context.source_range,
            );
            switch (block_result) {
                .ActivationChanged => {
                    // Because we're changing the activation, we need to manually adjust the PC.
                    _ = activation_before_script.advanceInstruction();
                    return ExecutionResult.changeActivation();
                },
                else => return block_result,
            }
        },
    }
}

/// Raise the argument as an error. The argument must be a byte vector.
pub fn Error(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const arguments = context.getArguments("_Error:");
    const message = try arguments.getObject(0, .ByteArray);

    return ExecutionResult.runtimeError(try RuntimeError.initFormatted(
        context.source_range,
        "Error raised in Self code: {s}",
        .{message.getValues()},
    ));
}

/// Restarts the current method, executing it from the first statement.
/// This primitive is intended to be used internally only.
pub fn Restart(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    _ = context;
    return ExecutionResult.restart();
}
