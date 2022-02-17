// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Script = @import("../../language/script.zig");
const Completion = @import("../completion.zig");
const environment = @import("../environment.zig");
const interpreter = @import("../interpreter.zig");
const SourceRange = @import("../../language/source_range.zig");
const runtime_error = @import("../error.zig");
const error_set_utils = @import("../../utility/error_set.zig");

const InterpreterContext = interpreter.InterpreterContext;

/// Return the static "nil" slots object.
pub fn Nil(allocator: Allocator, heap: *Heap, receiver: Heap.Tracked, arguments: []Heap.Tracked, source_range: SourceRange, context: *InterpreterContext) !Completion {
    _ = allocator;
    _ = heap;
    _ = receiver;
    _ = arguments;
    _ = source_range;
    _ = context;

    return Completion.initNormal(environment.globalNil());
}

/// Exit with the given return code.
pub fn Exit(allocator: Allocator, heap: *Heap, receiver: Heap.Tracked, arguments: []Heap.Tracked, source_range: SourceRange, context: *InterpreterContext) !Completion {
    _ = heap;
    _ = receiver;
    _ = context;

    var status_code = arguments[0].getValue();
    if (!status_code.isInteger()) {
        return Completion.initRuntimeError(allocator, source_range, "Expected integer for the status code argument of _Exit:, got {s}", .{@tagName(status_code.getType())});
    }

    // The ultimate in garbage collection.
    std.os.exit(@intCast(u8, status_code.asUnsignedInteger()));
}

/// Run the given script file, and return the result of the last expression.
pub fn RunScript(allocator: Allocator, heap: *Heap, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, source_range: SourceRange, context: *InterpreterContext) !Completion {
    _ = arguments;

    const receiver = tracked_receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected ByteArray for the receiver of _RunScript", .{});
    }

    const receiver_byte_array = receiver.asObject().asByteArrayObject();

    // FIXME: Find a way to handle errors here. These hacks are nasty.

    const requested_script_path = receiver_byte_array.getValues();
    const running_script_path = context.script.value.file_path;

    const paths_to_join = &[_][]const u8{
        std.fs.path.dirname(running_script_path) orelse ".",
        requested_script_path,
    };

    const target_path = std.fs.path.join(allocator, paths_to_join) catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        return Completion.initRuntimeError(allocator, source_range, "An unexpected error was raised from std.fs.path.relative: {s}", .{@errorName(err)});
    };
    defer allocator.free(target_path);

    var script = Script.createFromFilePath(allocator, target_path) catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        return Completion.initRuntimeError(allocator, source_range, "An unexpected error was raised from script.initInPlaceFromFilePath: {s}", .{@errorName(err)});
    };
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        return Completion.initRuntimeError(allocator, source_range, "An unexpected error was raised from script.parseScript: {s}", .{@errorName(err)});
    };
    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;

    if (!did_parse_without_errors) {
        return Completion.initRuntimeError(allocator, source_range, "Failed parsing the script passed to _RunScript", .{});
    }

    // FIXME: This is too much work. Just return the original value.
    var result_value: Heap.Tracked = try heap.track(environment.globalNil());
    defer result_value.untrack(heap);

    if (try interpreter.executeSubScript(allocator, heap, script, context)) |script_completion| {
        if (script_completion.isNormal()) {
            result_value.untrack(heap);
            result_value = try heap.track(script_completion.data.Normal);
        } else {
            return script_completion;
        }
    }

    return Completion.initNormal(result_value.getValue());
}

/// Raise the argument as an error. The argument must be a byte vector.
pub fn Error(allocator: Allocator, heap: *Heap, receiver: Heap.Tracked, arguments: []Heap.Tracked, source_range: SourceRange, context: *InterpreterContext) !Completion {
    _ = heap;
    _ = receiver;
    _ = context;

    var argument = arguments[0].getValue();
    if (!(argument.isObjectReference() and argument.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(allocator, source_range, "Expected ByteArray as _Error: argument", .{});
    }

    return Completion.initRuntimeError(allocator, source_range, "Error raised in Self code: {s}", .{argument.asObject().asByteArrayObject().getValues()});
}

/// Restarts the current method, executing it from the first statement.
/// This primitive is intended to be used internally only.
pub fn Restart(allocator: Allocator, heap: *Heap, receiver: Heap.Tracked, arguments: []Heap.Tracked, source_range: SourceRange, context: *InterpreterContext) !Completion {
    _ = allocator;
    _ = heap;
    _ = receiver;
    _ = arguments;
    _ = source_range;
    _ = context;

    return Completion.initRestart();
}
