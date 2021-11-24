// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Range = @import("../../language/location_range.zig");
const Script = @import("../../language/script.zig");
const Object = @import("../object.zig");
const environment = @import("../environment.zig");
const interpreter = @import("../interpreter.zig");
const runtime_error = @import("../error.zig");
const error_set_utils = @import("../../utility/error_set.zig");

const InterpreterContext = interpreter.InterpreterContext;

/// Return the static "nil" slots object.
pub fn Nil(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = context;
    _ = allocator;
    _ = arguments;
    _ = message_range;
    receiver.unref();

    return environment.globalNil();
}

/// Exit with the given return code.
pub fn Exit(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = receiver;
    _ = message_range;

    var status_code = arguments[0];
    if (!status_code.value.is(.Integer)) {
        return runtime_error.raiseError(allocator, context, "Expected Integer for the status code argument of _Exit:, got {s}", .{@tagName(status_code.value.content)});
    }

    std.os.exit(@intCast(u8, status_code.value.content.Integer.value));
}

/// Run the given script file, and return the result of the last expression.
pub fn RunScript(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = arguments;
    _ = message_range;

    defer receiver.unref();

    if (!receiver.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector for the receiver of _RunScript, got {s}", .{@tagName(receiver.value.content)});
    }

    // FIXME: Find a way to handle errors here. These hacks are nasty.

    const requested_script_path = receiver.value.content.ByteVector.values;
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

    var result_value = environment.globalNil();
    if (try interpreter.executeSubScript(allocator, script, context)) |script_result| {
        result_value.unref();
        result_value = script_result;
    }

    return result_value;
}

/// Return the receiver's global ID.
pub fn ID(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = context;
    _ = arguments;
    _ = message_range;

    defer receiver.unref();

    return try Object.createFromIntegerLiteral(allocator, receiver.value.id);
}

/// Raise the argument as an error. The argument must be a byte vector.
pub fn Error(allocator: *Allocator, message_range: Range, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) !Object.Ref {
    _ = message_range;

    defer receiver.unref();

    var argument = arguments[0];
    defer argument.unref();
    if (!argument.value.is(.ByteVector)) {
        return runtime_error.raiseError(allocator, context, "Expected ByteVector as _Error: argument, got {s}", .{@tagName(argument.value.content)});
    }

    return runtime_error.raiseError(allocator, context, "Error raised in Self code: {s}", .{argument.value.content.ByteVector.values});
}
