// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const Completion = @import("../Completion.zig");
const ManagedObject = @import("../object/managed.zig");
const FileDescriptor = ManagedObject.FileDescriptor;
const value_inspector = @import("../value_inspector.zig");

const interpreter = @import("../interpreter.zig");

const ExecutionResult = interpreter.ExecutionResult;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

fn callFailureBlock(
    context: *PrimitiveContext,
    errno: std.os.system.E,
    block: Value,
) !ExecutionResult {
    const errno_int = @enumToInt(errno);
    const errno_value = Value.fromInteger(errno_int);

    try context.actor.argument_stack.push(context.vm.allocator, errno_value);
    if (try interpreter.sendMessage(context.vm, context.actor, block, "value:", context.target_location, context.source_range)) |completion| {
        return ExecutionResult.completion(completion);
    }

    return ExecutionResult.activationChange();
}

/// Open the path with the given flags and return a file descriptor.
/// On failure, call the given block with the errno as the argument.
pub fn Open_WithFlags_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_Open:WithFlags:IfFail:");
    const file_path = try arguments.getObject(0, .ByteArray);
    const flags = try arguments.getInteger(1, .Unsigned);
    const failure_block = arguments.getValue(2);

    // FIXME: Handle this error
    const null_terminated_path = std.os.toPosixPath(file_path.getValues()) catch unreachable;

    const rc = std.os.system.open(&null_terminated_path, @intCast(u32, flags), 0);
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        var fd = FileDescriptor.adopt(@intCast(std.os.fd_t, rc));
        errdefer fd.close();

        const managed_fd = try Object.Managed.create(context.vm.heap, .FileDescriptor, fd.toValue());
        return ExecutionResult.completion(Completion.initNormal(managed_fd.asValue()));
    }

    return try callFailureBlock(context, errno, failure_block);
}

/// Read the given amount of bytes into the given byte array at the given offset
/// from the given file descriptor. If the byte array is smaller than the given
/// amount of bytes, raises an error. On success, returns the amount of bytes
/// read. On failure, call the given block with the errno as the argument.
pub fn Read_BytesInto_AtOffset_From_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_Read:BytesInto:AtOffset:From:IfFail:");
    const bytes_to_read = try arguments.getInteger(0, .Unsigned);
    const byte_array = try arguments.getObject(1, .ByteArray);
    const offset = try arguments.getInteger(2, .Unsigned);
    const fd = try arguments.getObject(3, .Managed);
    const failure_block = arguments.getValue(4);

    if (fd.getManagedType() != .FileDescriptor) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected file descriptor as argument 4 of _Read:BytesInto:AtOffset:From:IfFail:",
            .{},
        ));
    }

    if (byte_array.getValues().len < offset) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Offset argument is larger than byte array size",
            .{},
        ));
    }

    if (byte_array.getValues().len - @intCast(usize, offset) < bytes_to_read) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Bytes to read argument is larger than byte array size - offset",
            .{},
        ));
    }

    const fd_value = FileDescriptor.fromValue(fd.value).fd;
    const rc = std.os.system.read(fd_value, byte_array.getValues().ptr + @intCast(usize, offset), @intCast(usize, bytes_to_read));

    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        return ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(@intCast(usize, rc))));
    }

    return try callFailureBlock(context, errno, failure_block);
}

/// Write the given amount of bytes from the given byte array at the given offset
/// into the given file descriptor. If the byte array is smaller than the given
/// amount of bytes, raises an error. On success, returns the amount of bytes
/// written. On failure, call the given block with the errno as the argument.
pub fn Write_BytesFrom_AtOffset_Into_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_Write:BytesFrom:AtOffset:Into:IfFail:");
    const bytes_to_write = try arguments.getInteger(0, .Unsigned);
    const byte_array = try arguments.getObject(1, .ByteArray);
    const offset = try arguments.getInteger(2, .Unsigned);
    const fd = try arguments.getObject(3, .Managed);
    const failure_block = arguments.getValue(4);

    if (fd.getManagedType() != .FileDescriptor) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected file descriptor as argument 4 of _Write:BytesFrom:AtOffset:Into:IfFail:",
            .{},
        ));
    }

    if (byte_array.getValues().len < offset) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Offset argument is larger than byte array size",
            .{},
        ));
    }

    if (byte_array.getValues().len - @intCast(usize, offset) < bytes_to_write) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Bytes to write argument is larger than byte array size - offset",
            .{},
        ));
    }

    const fd_value = FileDescriptor.fromValue(fd.value).fd;
    const rc = std.os.system.write(fd_value, byte_array.getValues().ptr + @intCast(usize, offset), @intCast(usize, bytes_to_write));

    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        return ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(@intCast(usize, rc))));
    }

    return try callFailureBlock(context, errno, failure_block);
}

/// Closes a file descriptor.
/// If the file descriptor was already closed, this primitive does nothing.
pub fn Close(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_Close:");
    const fd_object = try arguments.getObject(0, .Managed);

    if (fd_object.getManagedType() != .FileDescriptor) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected file descriptor as argument 1 of _Close:",
            .{},
        ));
    }

    var fd = FileDescriptor.fromValue(fd_object.value);
    fd.close();
    // NOTE: Now that we closed the fd, we need to stuff this value back into
    //       the managed object as it operates independently.
    fd_object.value = fd.toValue();
    return ExecutionResult.completion(Completion.initNormal(context.vm.nil()));
}

/// Exit with the given return code.
pub fn Exit(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_Exit:");
    const status_code = try arguments.getInteger(0, .Unsigned);

    // The ultimate in garbage collection.
    std.os.exit(@intCast(u8, status_code));
}

/// The maximum amount of pollfd structures that can be on the stack before
/// switching to heap allocation.
const MaximumInlinePollFDs = 32;

/// Poll the given file descriptors with the associated events to listen for.
/// Write the returned events to the events parameter. Wait for events up to
/// `WaitingForMS' milliseconds. If an error is encountered with the system
/// call, send `value:' to `IfFail' with the errno.
pub fn PollFDs_Events_WaitingForMS_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_PollFDs:Events:WaitingForMS:IfFail:");
    const fds = try arguments.getObject(0, .Array);
    const events = try arguments.getObject(1, .Array);
    const timeout_ms = try arguments.getInteger(2, .Signed);
    const failure_block = arguments.getValue(3);

    if (fds.getSize() != events.getSize()) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Argument 1 and 2 of _PollFDs:Events:WaitingForMS: must have the same size",
            .{},
        ));
    }

    for (fds.getValues()) |fd| {
        if (!fd.isInteger()) {
            return ExecutionResult.completion(try Completion.initRuntimeError(
                context.vm,
                context.source_range,
                "Argument 1 of _PollFDs:Events:WaitingForMS: must be an array of integers",
                .{},
            ));
        }
    }

    for (events.getValues()) |event_flags| {
        if (!event_flags.isInteger()) {
            return ExecutionResult.completion(try Completion.initRuntimeError(
                context.vm,
                context.source_range,
                "Argument 2 of _PollFDs:Events:WaitingForMS: must be an array of integers",
                .{},
            ));
        }
    }

    const fd_count = fds.getSize();
    var inline_poll_fds: [MaximumInlinePollFDs]std.os.pollfd = undefined;
    var poll_fds = poll_fds: {
        if (fd_count <= MaximumInlinePollFDs) {
            break :poll_fds inline_poll_fds[0..fd_count];
        } else {
            break :poll_fds try context.vm.allocator.alloc(std.os.pollfd, fd_count);
        }
    };

    defer {
        if (fd_count > MaximumInlinePollFDs)
            context.vm.allocator.free(poll_fds);
    }

    const fd_values = fds.getValues();
    const event_values = events.getValues();
    for (fd_values) |fd, i| {
        const event_flags = event_values[i];

        poll_fds[i] = .{
            .fd = @intCast(i32, fd.asInteger()),
            .events = @intCast(i16, event_flags.asInteger()),
            .revents = 0,
        };
    }

    const rc = std.os.system.poll(poll_fds.ptr, fds.getSize(), @intCast(i32, timeout_ms));
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        for (poll_fds) |pollfd, i| {
            event_values[i] = Value.fromInteger(@intCast(i64, pollfd.revents));
        }
        return ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(@intCast(usize, rc))));
    }

    return try callFailureBlock(context, errno, failure_block);
}
