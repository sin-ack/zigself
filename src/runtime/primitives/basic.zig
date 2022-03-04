// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../heap.zig");
const Script = @import("../../language/script.zig");
const Completion = @import("../completion.zig");
const interpreter = @import("../interpreter.zig");
const error_set_utils = @import("../../utility/error_set.zig");
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

const message_interpreter = @import("../interpreter/message.zig");
const runtime_error = @import("../error.zig");

/// Return the static "nil" slots object.
pub fn Nil(context: PrimitiveContext) !Completion {
    _ = context;
    return Completion.initNormal(context.vm.nil());
}

/// Exit with the given return code.
pub fn Exit(context: PrimitiveContext) !Completion {
    var status_code = context.arguments[0].getValue();
    if (!status_code.isInteger()) {
        return Completion.initRuntimeError(context.vm, context.source_range, "Expected integer for the status code argument of _Exit:, got {s}", .{@tagName(status_code.getType())});
    }

    // The ultimate in garbage collection.
    std.os.exit(@intCast(u8, status_code.asUnsignedInteger()));
}

/// Run the given script file, and return the result of the last expression.
pub fn RunScript(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.vm, context.source_range, "Expected ByteArray for the receiver of _RunScript", .{});
    }

    const receiver_byte_array = receiver.asObject().asByteArrayObject();

    // FIXME: Find a way to handle errors here. These hacks are nasty.

    const requested_script_path = receiver_byte_array.getValues();
    const running_script_path = context.interpreter_context.script.value.running_path;

    const paths_to_join = &[_][]const u8{
        std.fs.path.dirname(running_script_path) orelse ".",
        requested_script_path,
    };

    const target_path = std.fs.path.join(context.vm.allocator, paths_to_join) catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        return Completion.initRuntimeError(context.vm, context.source_range, "An unexpected error was raised from std.fs.path.relative: {s}", .{@errorName(err)});
    };
    defer context.vm.allocator.free(target_path);

    var script = Script.createFromFilePath(context.vm.allocator, target_path) catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        return Completion.initRuntimeError(context.vm, context.source_range, "An unexpected error was raised from script.initInPlaceFromFilePath: {s}", .{@errorName(err)});
    };
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        return Completion.initRuntimeError(context.vm, context.source_range, "An unexpected error was raised from script.parseScript: {s}", .{@errorName(err)});
    };
    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;

    if (!did_parse_without_errors) {
        return Completion.initRuntimeError(context.vm, context.source_range, "Failed parsing the script passed to _RunScript", .{});
    }

    // FIXME: This is too much work. Just return the original value.
    var result_value: Heap.Tracked = try context.vm.heap.track(context.vm.nil());
    defer result_value.untrack(context.vm.heap);

    if (try interpreter.executeSubScript(context.interpreter_context, script)) |script_completion| {
        if (script_completion.isNormal()) {
            result_value.untrack(context.vm.heap);
            result_value = try context.vm.heap.track(script_completion.data.Normal);
        } else {
            return script_completion;
        }
    }

    return Completion.initNormal(result_value.getValue());
}

pub fn EvaluateStringIfFail(context: PrimitiveContext) !Completion {
    const receiver = context.receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.vm, context.source_range, "Expected ByteArray for the receiver of _RunScript", .{});
    }

    const receiver_byte_array = receiver.asObject().asByteArrayObject();
    const running_script_path = context.interpreter_context.script.value.running_path;

    var script = try Script.createFromString(context.vm.allocator, running_script_path, receiver_byte_array.getValues());
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        // TODO: Instead of printing this error like this, pass it to the failure block.
        std.debug.print("An unexpected error was raised from script.parseScript: {s}\n", .{@errorName(err)});
        return message_interpreter.sendMessage(context.interpreter_context, context.arguments[0], "value", &.{}, context.source_range);
    };
    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;

    if (!did_parse_without_errors) {
        // TODO: Pass error information to the failure block.
        return message_interpreter.sendMessage(context.interpreter_context, context.arguments[0], "value", &.{}, context.source_range);
    }

    // FIXME: This is too much work. Just return the original value.
    var result_value: Heap.Tracked = try context.vm.heap.track(context.vm.nil());
    defer result_value.untrack(context.vm.heap);

    const current_activation = context.interpreter_context.activation_stack.getCurrent().?;

    if (try interpreter.executeSubScript(context.interpreter_context, script)) |*script_completion| {
        switch (script_completion.data) {
            .Normal => |result| {
                result_value.untrack(context.vm.heap);
                result_value = try context.vm.heap.track(result);
            },
            .RuntimeError => |err| {
                defer script_completion.deinit(context.vm);

                // TODO: Pass error information to failure block
                std.debug.print("Received error while evaluating string: {s}\n", .{err.message});
                runtime_error.printTraceFromActivationStackUntil(context.interpreter_context.activation_stack.getStack(), err.source_range, current_activation);
                context.interpreter_context.activation_stack.restoreTo(current_activation);

                return message_interpreter.sendMessage(context.interpreter_context, context.arguments[0], "value", &.{}, context.source_range);
            },
            else => return script_completion.*,
        }
    }

    return Completion.initNormal(result_value.getValue());
}

/// Raise the argument as an error. The argument must be a byte vector.
pub fn Error(context: PrimitiveContext) !Completion {
    var argument = context.arguments[0].getValue();
    if (!(argument.isObjectReference() and argument.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(context.vm, context.source_range, "Expected ByteArray as _Error: argument", .{});
    }

    return Completion.initRuntimeError(context.vm, context.source_range, "Error raised in Self code: {s}", .{argument.asObject().asByteArrayObject().getValues()});
}

/// Restarts the current method, executing it from the first statement.
/// This primitive is intended to be used internally only.
pub fn Restart(context: PrimitiveContext) !Completion {
    _ = context;
    return Completion.initRestart();
}
