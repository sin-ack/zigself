// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Range = @import("../../language/location_range.zig");
const Script = @import("../../language/script.zig");
const environment = @import("../environment.zig");
const interpreter = @import("../interpreter.zig");
const runtime_error = @import("../error.zig");
const error_set_utils = @import("../../utility/error_set.zig");

const InterpreterContext = interpreter.InterpreterContext;

/// Return the static "nil" slots object.
pub fn Nil(allocator: Allocator, heap: *Heap, message_range: Range, receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = allocator;
    _ = heap;
    _ = message_range;
    _ = receiver;
    _ = arguments;
    _ = context;

    return environment.globalNil();
}

/// Exit with the given return code.
pub fn Exit(allocator: Allocator, heap: *Heap, message_range: Range, receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;
    _ = receiver;

    var status_code = arguments[0].getValue();
    if (!status_code.isInteger()) {
        return runtime_error.raiseError(allocator, context, "Expected integer for the status code argument of _Exit:, got {s}", .{@tagName(status_code.getType())});
    }

    // The ultimate in garbage collection, no need to unref.
    std.os.exit(@intCast(u8, status_code.asUnsignedInteger()));
}

/// Run the given script file, and return the result of the last expression.
pub fn RunScript(allocator: Allocator, heap: *Heap, message_range: Range, tracked_receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = arguments;
    _ = message_range;

    const receiver = tracked_receiver.getValue();
    if (!(receiver.isObjectReference() and receiver.asObject().isByteVectorObject())) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector for the receiver of _RunScript", .{});
    }

    const receiver_byte_vector = receiver.asObject().asByteVectorObject();

    // FIXME: Find a way to handle errors here. These hacks are nasty.

    const requested_script_path = receiver_byte_vector.getValues();
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
        return runtime_error.raiseError(allocator, context, "An unexpected error was raised from std.fs.path.relative: {s}", .{@errorName(err)});
    };
    defer allocator.free(target_path);

    var script = Script.createFromFilePath(allocator, target_path) catch |err|
        if (error_set_utils.errSetContains(Allocator.Error, err))
    {
        return @errSetCast(Allocator.Error, err);
    } else {
        return runtime_error.raiseError(allocator, context, "An unexpected error was raised from script.initInPlaceFromFilePath: {s}", .{@errorName(err)});
    };

    {
        errdefer script.unref();

        const did_parse_without_errors = script.value.parseScript() catch |err|
            if (error_set_utils.errSetContains(Allocator.Error, err))
        {
            return @errSetCast(Allocator.Error, err);
        } else {
            return runtime_error.raiseError(allocator, context, "An unexpected error was raised from script.parseScript: {s}", .{@errorName(err)});
        };
        script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;

        if (!did_parse_without_errors) {
            return runtime_error.raiseError(allocator, context, "Failed parsing the script passed to _RunScript", .{});
        }
    }

    var result_value: Heap.Tracked = try heap.track(environment.globalNil());
    defer result_value.untrackAndDestroy(heap);

    if (try interpreter.executeSubScript(allocator, heap, script, context)) |script_result| {
        result_value.untrackAndDestroy(heap);
        result_value = try heap.track(script_result);
    }

    return result_value.getValue();
}

/// Raise the argument as an error. The argument must be a byte vector.
pub fn Error(allocator: Allocator, heap: *Heap, message_range: Range, receiver: Heap.Tracked, arguments: []Heap.Tracked, context: *InterpreterContext) !Value {
    _ = heap;
    _ = message_range;
    _ = receiver;

    var argument = arguments[0].getValue();
    if (!(argument.isObjectReference() and argument.asObject().isByteVectorObject())) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector as _Error: argument", .{});
    }

    return runtime_error.raiseError(allocator, context, "Error raised in Self code: {s}", .{argument.asObject().asByteVectorObject().getValues()});
}
