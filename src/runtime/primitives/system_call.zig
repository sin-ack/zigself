// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Value = value_import.Value;
const Object = @import("../Object.zig");
const ByteArray = @import("../ByteArray.zig");
const Completion = @import("../Completion.zig");
const value_import = @import("../value.zig");
const ManagedValue = value_import.ManagedValue;
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

    // FIXME: Allow the user to pass permissions via the primitive.
    const rc = std.os.system.open(&null_terminated_path, @intCast(u32, flags), @intCast(u32, 0));
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        var fd = FileDescriptor.adopt(@intCast(std.os.fd_t, rc));
        errdefer fd.close();

        var token = try context.vm.heap.getAllocation(Object.Managed.requiredSizeForAllocation());
        const managed_fd = try Object.Managed.create(&token, context.actor.id, .FileDescriptor, fd.toValue());
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
    return switch (errno) {
        .SUCCESS => ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(@intCast(usize, rc)))),
        .AGAIN => blk: {
            if (context.vm.isInRegularActor()) {
                context.actor.yield_reason = .Blocked;
                context.actor.blocked_fd = ManagedValue.init(fd);

                context.vm.switchToActor(context.vm.genesis_actor.?);
                break :blk ExecutionResult.actorSwitch();
            }

            break :blk try callFailureBlock(context, errno, failure_block);
        },
        else => try callFailureBlock(context, errno, failure_block),
    };
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
        if (fd.isObjectReference()) {
            if (fd.asObject().asType(.Managed)) |managed_fd| {
                if (managed_fd.getManagedType() == .FileDescriptor)
                    continue;
            }
        }

        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Argument 1 of _PollFDs:Events:WaitingForMS: must be an array of file descriptors",
            .{},
        ));
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
        const managed_fd = fd.asObject().asType(.Managed).?;
        const fd_value = FileDescriptor.fromValue(managed_fd.value);
        const event_flags = event_values[i];

        poll_fds[i] = .{
            .fd = fd_value.fd,
            .events = @intCast(i16, event_flags.asInteger()),
            .revents = 0,
        };
    }

    const rc = std.os.system.poll(poll_fds.ptr, @intCast(u32, fds.getSize()), @intCast(i32, timeout_ms));
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        for (poll_fds) |pollfd, i| {
            event_values[i] = Value.fromInteger(@intCast(i64, pollfd.revents));
        }
        return ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(@intCast(usize, rc))));
    }

    return try callFailureBlock(context, errno, failure_block);
}

/// Perform the getaddrinfo system call and return an array of getaddrinfo
/// objects.
pub fn GetAddrInfoForHost_Port_Family_SocketType_Protocol_Flags_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const primitive_name = "_GetAddrInfoForHost:Port:Family:SocketType:Protocol:Flags:IfFail:";
    const arguments = context.getArguments(primitive_name);
    const host = arguments.getValue(0);
    const port = try arguments.getInteger(1, .Unsigned);
    const family = try arguments.getInteger(2, .Signed);
    const socket_type = try arguments.getInteger(3, .Signed);
    const protocol = try arguments.getInteger(4, .Signed);
    const flags = try arguments.getInteger(5, .Signed);
    const failure_block = arguments.getValue(6);

    // FIXME: Do not directly intCast here
    const hints = std.os.addrinfo{
        .family = @intCast(i32, family),
        .socktype = @intCast(i32, socket_type),
        .protocol = @intCast(i32, protocol),
        .flags = std.c.AI.NUMERICSERV | @intCast(i32, flags),

        .addrlen = 0,
        .addr = null,
        .canonname = null,
        .next = null,
    };

    const node_c: ?[:0]const u8 = node_c: {
        if (host.data == context.vm.nil().data) {
            break :node_c null;
        }

        if (host.isObjectReference()) {
            if (host.asObject().asType(.ByteArray)) |host_byte_array| {
                break :node_c try std.cstr.addNullByte(context.vm.allocator, host_byte_array.getValues());
            }
        }

        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected nil or byte array object for argument 1 of " ++ primitive_name,
            .{},
        ));
    };
    defer if (node_c) |str| context.vm.allocator.free(str);

    const service_c = try std.fmt.allocPrintZ(context.vm.allocator, "{}", .{port});
    defer context.vm.allocator.free(service_c);

    var result_ptr: *std.os.addrinfo = undefined;
    const rc = std.os.system.getaddrinfo(if (node_c) |s| s.ptr else null, service_c, &hints, &result_ptr);
    switch (rc) {
        @intToEnum(std.os.system.EAI, 0) => {},

        // FIXME: Handle errors
        .ADDRFAMILY,
        .AGAIN,
        .BADFLAGS,
        .FAIL,
        .FAMILY,
        .MEMORY,
        .NODATA,
        .NONAME,
        .SERVICE,
        .SOCKTYPE,
        .SYSTEM,
        => unreachable,

        else => unreachable,
    }
    _ = failure_block;

    defer std.os.system.freeaddrinfo(result_ptr);

    var required_memory: usize = 0;
    var result_count: usize = 0;
    var addrinfo_prototype = context.vm.addrinfo_prototype.getValue().asObject().asType(.Slots).?;
    {
        var it: ?*std.os.addrinfo = result_ptr;
        while (it) |result| : (it = result.next) {
            required_memory += addrinfo_prototype.getSizeInMemory();
            required_memory += Object.ByteArray.requiredSizeForAllocation(result.addrlen);
            result_count += 1;
        }
    }

    required_memory += Object.Map.Array.requiredSizeForAllocation();
    required_memory += Object.Array.requiredSizeForAllocation(result_count);

    var token = try context.vm.heap.getAllocation(required_memory);

    // Refresh pointers
    addrinfo_prototype = context.vm.addrinfo_prototype.getValue().asObject().asType(.Slots).?;

    const result_array_map = Object.Map.Array.create(&token, result_count);
    const result_array = Object.Array.createWithValues(&token, context.actor.id, result_array_map, &.{}, context.vm.nil());

    const result_values = result_array.getValues();
    {
        var it: ?*std.os.addrinfo = result_ptr;
        var i: usize = 0;
        while (it) |result| : ({
            it = result.next;
            i += 1;
        }) {
            const sockaddr_memory = @ptrCast([*]u8, result.addr.?);
            const sockaddr_bytes_object = try Object.ByteArray.createWithValues(&token, context.actor.id, sockaddr_memory[0..result.addrlen]);

            const addrinfo_copy: *Object.Slots = addrinfo_prototype.clone(&token, context.actor.id);
            const addrinfo_value = addrinfo_copy.asValue();

            // FIXME: VM-generated structs already know where each slot is.
            //        Instead of doing a manual lookup, be smarter here.
            addrinfo_value.lookup(context.vm, "family:").Assignment.value_ptr.* = Value.fromInteger(result.family);
            addrinfo_value.lookup(context.vm, "socketType:").Assignment.value_ptr.* = Value.fromInteger(result.socktype);
            addrinfo_value.lookup(context.vm, "protocol:").Assignment.value_ptr.* = Value.fromInteger(result.protocol);
            addrinfo_value.lookup(context.vm, "flags:").Assignment.value_ptr.* = Value.fromInteger(result.flags);
            addrinfo_value.lookup(context.vm, "sockaddrBytes:").Assignment.value_ptr.* = sockaddr_bytes_object.asValue();

            result_values[i] = addrinfo_value;
        }
    }

    return ExecutionResult.completion(Completion.initNormal(result_array.asValue()));
}

/// Create a new socket with the given family, type and protocol.
pub fn SocketWithFamily_Type_Protocol_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_SocketWithFamily:Type:Protocol:IfFail:");
    const family = try arguments.getInteger(0, .Unsigned);
    const socket_type = try arguments.getInteger(1, .Unsigned);
    const protocol = try arguments.getInteger(2, .Unsigned);
    const failure_block = arguments.getValue(3);

    // FIXME: Check before casting
    // FIXME: SOCK_NONBLOCK and SOCK_CLOEXEC only works on Linux! Make a fallback.
    var full_socket_type = @intCast(c_uint, socket_type) | std.os.SOCK.CLOEXEC;

    if (context.vm.isInRegularActor()) {
        full_socket_type |= std.os.SOCK.NONBLOCK;
    }

    const rc = std.os.system.socket(@intCast(c_uint, family), full_socket_type, @intCast(c_uint, protocol));
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        var fd = FileDescriptor.adopt(@intCast(std.os.fd_t, rc));
        errdefer fd.close();

        var token = try context.vm.heap.getAllocation(Object.Managed.requiredSizeForAllocation());
        const managed_fd = try Object.Managed.create(&token, context.actor.id, .FileDescriptor, fd.toValue());
        return ExecutionResult.completion(Completion.initNormal(managed_fd.asValue()));
    }

    return try callFailureBlock(context, errno, failure_block);
}

/// Bind the given fd to the address defined in the given sockaddr structure.
pub fn BindFD_ToSockaddrBytes_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_BindFD:ToSockaddrBytes:IfFail:");
    const fd_object = try arguments.getObject(0, .Managed);
    const sockaddr_object = try arguments.getObject(1, .ByteArray);
    const failure_block = arguments.getValue(2);

    if (fd_object.getManagedType() != .FileDescriptor) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected file descriptor as argument 1 of _BindFD:ToSockaddrBytes:IfFail:",
            .{},
        ));
    }

    const fd = FileDescriptor.fromValue(fd_object.value);
    const sockaddr_bytes: []const u8 = sockaddr_object.getValues();

    // FIXME: Check before casting
    const sockaddr = std.os.system.sockaddr;
    const rc = std.os.system.bind(fd.fd, @ptrCast(*const sockaddr, @alignCast(@alignOf(sockaddr), sockaddr_bytes.ptr)), @intCast(u32, sockaddr_bytes.len));
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        return ExecutionResult.completion(Completion.initNormal(context.vm.nil()));
    }

    return try callFailureBlock(context, errno, failure_block);
}

/// Set the given fd as a passive socket with the given backlog.
pub fn ListenOnFD_WithBacklog_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_ListenOnFD:WithBacklog:IfFail:");
    const fd_object = try arguments.getObject(0, .Managed);
    const backlog = try arguments.getInteger(1, .Signed);
    const failure_block = arguments.getValue(2);

    if (fd_object.getManagedType() != .FileDescriptor) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected file descriptor as argument 1 of _BindFD:ToSockaddrBytes:IfFail:",
            .{},
        ));
    }

    // FIXME: Check before casting
    const fd = FileDescriptor.fromValue(fd_object.value);
    const rc = std.os.system.listen(fd.fd, @intCast(c_uint, backlog));
    const errno = std.os.system.getErrno(rc);
    if (errno == .SUCCESS) {
        return ExecutionResult.completion(Completion.initNormal(context.vm.nil()));
    }

    return try callFailureBlock(context, errno, failure_block);
}

/// Accept a connection from the given fd, returning the new fd.
pub fn AcceptFromFD_IfFail(context: *PrimitiveContext) !ExecutionResult {
    // TODO: Return the struct sockaddr with the connection details
    const arguments = context.getArguments("_AcceptFromFD:IfFail:");
    const fd_object = try arguments.getObject(0, .Managed);
    const failure_block = arguments.getValue(1);

    if (fd_object.getManagedType() != .FileDescriptor) {
        return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "Expected file descriptor as argument 1 of _AcceptFromFD:IfFail:",
            .{},
        ));
    }

    const fd = FileDescriptor.fromValue(fd_object.value);

    const rc = std.os.system.accept(fd.fd, null, null);
    const errno = std.os.system.getErrno(rc);
    return switch (errno) {
        .SUCCESS => {
            const new_fd_value = @intCast(std.os.fd_t, rc);
            _ = std.os.fcntl(new_fd_value, std.os.F.SETFD, std.os.FD_CLOEXEC) catch unreachable;

            if (context.vm.isInRegularActor()) {
                _ = std.os.fcntl(new_fd_value, std.os.F.SETFL, std.os.O.NONBLOCK) catch unreachable;
            }

            var new_fd = FileDescriptor.adopt(new_fd_value);
            errdefer new_fd.close();

            var token = try context.vm.heap.getAllocation(Object.Managed.requiredSizeForAllocation());
            const managed_new_fd = try Object.Managed.create(&token, context.actor.id, .FileDescriptor, new_fd.toValue());
            return ExecutionResult.completion(Completion.initNormal(managed_new_fd.asValue()));
        },
        .AGAIN => blk: {
            if (context.vm.isInRegularActor()) {
                context.actor.yield_reason = .Blocked;
                context.actor.blocked_fd = ManagedValue.init(fd_object);

                context.vm.switchToActor(context.vm.genesis_actor.?);
                break :blk ExecutionResult.actorSwitch();
            }

            break :blk callFailureBlock(context, errno, failure_block);
        },
        else => try callFailureBlock(context, errno, failure_block),
    };
}
