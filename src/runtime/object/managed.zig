// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const stage2_compat = @import("../../utility/stage2_compat.zig");

const ManagedTypeShift = Object.ObjectTypeShift + Object.ObjectTypeBits;
const ManagedTypeBits = 1;
const ManagedTypeMask: u64 = ((1 << ManagedTypeBits) - 1) << ManagedTypeShift;

const ManagedType = enum(u64) {
    FileDescriptor = 0b0 << ManagedTypeShift,
};

/// A file descriptor value consists of two things:
/// - 30 bits of flags, currently only used for holding the "closed" state of a fd.
/// - 32 bits for the actual file descriptor.
pub const FileDescriptor = struct {
    fd: std.os.fd_t,
    flags: u30,

    const ClosedFlag: u30 = 1;

    pub fn adopt(fd: std.os.fd_t) FileDescriptor {
        return .{
            .flags = 0,
            .fd = fd,
        };
    }

    pub fn fromValue(value: Value) FileDescriptor {
        const raw_value = value.asUnsignedInteger();
        return .{
            .flags = @intCast(u30, raw_value >> 32),
            .fd = @intCast(std.os.fd_t, raw_value & @as(u64, 0xFFFFFFFF)),
        };
    }

    pub fn toValue(self: FileDescriptor) Value {
        const value: u62 = (@as(u62, self.flags) << 32) | @intCast(u32, self.fd);
        return Value.fromUnsignedInteger(value);
    }

    /// Return whether this file descriptor has been closed.
    pub fn isClosed(self: FileDescriptor) bool {
        return self.flags & ClosedFlag != 0;
    }

    /// Set whether this fd is closed or not.
    pub fn setClosed(self: *FileDescriptor, closed: bool) void {
        self.flags = (self.flags & ~ClosedFlag) + (if (closed) ClosedFlag else 0);
    }

    pub fn close(self: *FileDescriptor) void {
        if (self.isClosed()) return;
        std.os.close(self.fd);
        self.setClosed(true);
    }
};

/// An object containing a managed value, and its type. When this object is
/// finalized, it will perform the associated finalization step.
pub const ManagedObject = extern struct {
    header: Object.Header align(@alignOf(u64)),
    value: Value align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(ManagedObject, .Mutable);

    pub fn create(map_map: Value, token: *Heap.AllocationToken, actor_id: u31, managed_type: ManagedType, value: Value) !ManagedObject.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self = @ptrCast(ManagedObject.Ptr, memory_area);
        self.init(actor_id, map_map, managed_type, value);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(self: ManagedObject.Ptr, actor_id: u31, map: Value, managed_type: ManagedType, value: Value) void {
        self.header.init(.Managed, actor_id, map);
        self.setManagedType(managed_type);
        self.value = value;
    }

    fn setManagedType(self: ManagedObject.Ptr, managed_type: ManagedType) void {
        self.header.object_information = (self.header.object_information & ~ManagedTypeMask) | @enumToInt(managed_type);
    }

    pub fn getManagedType(self: ManagedObject.Ptr) ManagedType {
        const raw_managed_type = self.header.object_information & ManagedTypeMask;
        return std.meta.intToEnum(ManagedType, raw_managed_type) catch |err| switch (err) {
            std.meta.IntToEnumError.InvalidEnumTag => std.debug.panic(
                "Unexpected managed type {x} on object at {*}\n",
                .{ raw_managed_type >> ManagedTypeShift, self },
            ),
        };
    }

    pub fn finalize(self: ManagedObject.Ptr, allocator: Allocator) void {
        _ = allocator;
        switch (self.getManagedType()) {
            .FileDescriptor => {
                var fd = FileDescriptor.fromValue(self.value);
                fd.close();
            },
        }
    }

    pub fn asObjectAddress(self: ManagedObject.Ptr) [*]u64 {
        return @ptrCast([*]u64, self);
    }

    pub fn asValue(self: ManagedObject.Ptr) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getSizeInMemory(self: ManagedObject.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ManagedObject);
    }
};
