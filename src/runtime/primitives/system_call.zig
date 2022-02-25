// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Completion = @import("../completion.zig");
const environment = @import("../environment.zig");
const ManagedObject = @import("../object/managed.zig");
const FileDescriptor = ManagedObject.FileDescriptor;

const message_interpreter = @import("../interpreter/message.zig");

const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

fn callFailureBlock(
    context: PrimitiveContext,
    errno: std.os.system.E,
    block: Heap.Tracked,
) !Completion {
    const errno_int = @enumToInt(errno);
    const errno_value = Value.fromInteger(errno_int);
    const tracked_errno_value = try context.interpreter_context.heap.track(errno_value);
    defer tracked_errno_value.untrack(context.interpreter_context.heap);

    const completion = try message_interpreter.executeBlockMessage(context.interpreter_context, block, &.{tracked_errno_value}, context.source_range);
    if (!completion.isNormal()) {
        return completion;
    }

    return Completion.initNormal(environment.globalNil());
}

/// Open the path with the given flags and return a file descriptor.
/// On failure, call the given block with the errno as the argument.
pub fn Open_WithFlags_IfFail(context: PrimitiveContext) !Completion {
    const file_path = context.arguments[0].getValue();
    const flags_value = context.arguments[1].getValue();
    const failure_block = context.arguments[2].getValue();

    if (!(file_path.isObjectReference() and file_path.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected byte array as the first argument to _Open:WithFlags:IfFail:",
            .{},
        );
    }

    if (!flags_value.isInteger()) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected integer as the second argument to _Open:WithFlags:IfFail:",
            .{},
        );
    }

    if (!(failure_block.isObjectReference() and failure_block.asObject().isBlockObject())) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected block as the third argument to _Open:WithFlags:IfFail:",
            .{},
        );
    }

    const flags = flags_value.asInteger();
    if (flags < 0) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Flags argument to _Open:WithFlags:IfFail: must be positive",
            .{},
        );
    }

    // FIXME: Handle this error
    const null_terminated_path = std.os.toPosixPath(file_path.asObject().asByteArrayObject().getValues()) catch unreachable;

    const rc = std.os.system.open(&null_terminated_path, @intCast(u32, flags), 0);
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        var fd = FileDescriptor.adopt(@intCast(std.os.fd_t, rc));
        errdefer fd.close();

        const managed_fd = try Object.Managed.create(context.interpreter_context.heap, .FileDescriptor, fd.toValue());
        return Completion.initNormal(managed_fd.asValue());
    }

    return callFailureBlock(context, errno, context.arguments[2]);
}

/// Read the given amount of bytes into the given byte array at the given offset
/// from the given file descriptor. If the byte array is smaller than the given
/// amount of bytes, raises an error. On success, returns the amount of bytes
/// read. On failure, call the given block with the errno as the argument.
pub fn Read_BytesInto_AtOffset_From_IfFail(context: PrimitiveContext) !Completion {
    const bytes_to_read_value = context.arguments[0].getValue();
    const byte_array_value = context.arguments[1].getValue();
    const offset_value = context.arguments[2].getValue();
    const fd_value = context.arguments[3].getValue();
    const failure_block = context.arguments[4].getValue();

    if (!bytes_to_read_value.isInteger()) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected integer as the first argument to _Read:BytesInto:AtOffset:From:IfFail:",
            .{},
        );
    }

    if (!(byte_array_value.isObjectReference() and byte_array_value.asObject().isByteArrayObject())) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected byte array object as the second argument to _Read:BytesInto:AtOffset:From:IfFail:",
            .{},
        );
    }

    if (!offset_value.isInteger()) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected integer as the third argument to _Read:BytesInto:AtOffset:From:IfFail:",
            .{},
        );
    }

    if (!(fd_value.isObjectReference() and fd_value.asObject().isManaged() and fd_value.asObject().asManaged().getManagedType() == .FileDescriptor)) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected file descriptor as the fourth argument to _Read:BytesInto:AtOffset:From:IfFail:",
            .{},
        );
    }

    if (!(failure_block.isObjectReference() and failure_block.asObject().isBlockObject())) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected block as the fifth argument to _Read:BytesInto:AtOffset:From:IfFail:",
            .{},
        );
    }

    const bytes_to_read = bytes_to_read_value.asInteger();
    if (bytes_to_read < 0) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Bytes to read argument to _Read:BytesInto:AtOffset:From:IfFail: must be positive",
            .{},
        );
    }

    const offset = offset_value.asInteger();
    if (offset < 0) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Offset argument to _Read:BytesInto:AtOffset:From:IfFail: must be positive",
            .{},
        );
    }

    const byte_array = byte_array_value.asObject().asByteArrayObject();
    if (byte_array.getValues().len < bytes_to_read) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Bytes to read argument is larger than byte array size",
            .{},
        );
    }

    const fd = FileDescriptor.fromValue(fd_value.asObject().asManaged().value).fd;
    const rc = std.os.system.read(fd, byte_array.getValues().ptr + @intCast(usize, offset), @intCast(usize, bytes_to_read));

    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        return Completion.initNormal(Value.fromUnsignedInteger(@intCast(usize, rc)));
    }

    return callFailureBlock(context, errno, context.arguments[3]);
}

/// Closes a file descriptor.
/// If the file descriptor was already closed, this primitive does nothing.
pub fn Close(context: PrimitiveContext) !Completion {
    const fd_value = context.arguments[0].getValue();

    if (!(fd_value.isObjectReference() and fd_value.asObject().isManaged() and fd_value.asObject().asManaged().getManagedType() == .FileDescriptor)) {
        return Completion.initRuntimeError(
            context.interpreter_context.allocator,
            context.source_range,
            "Expected file descriptor as the argument to _Close:",
            .{},
        );
    }

    const managed_fd = fd_value.asObject().asManaged();
    var fd = FileDescriptor.fromValue(managed_fd.value);
    fd.close();
    // NOTE: Now that we closed the fd, we need to stuff this value back into
    //       the managed object as it operates independently.
    managed_fd.value = fd.toValue();
    return Completion.initNormal(environment.globalNil());
}
