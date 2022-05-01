// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");

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

// FIXME: This isn't thread safe!
var singleton_managed_map: ?Heap.Tracked = null;

fn getOrCreateManagedMap(heap: *Heap) !Value {
    if (singleton_managed_map) |map| return map.getValue();

    const map = try Object.Map.Slots.create(heap, 0);
    singleton_managed_map = try heap.track(map.asValue());
    return map.asValue();
}

fn requiredSizeForManagedMap() usize {
    return if (singleton_managed_map != null) 0 else Object.Map.Slots.requiredSizeForAllocation(0);
}

/// An object containing a managed value, and its type. When this object is
/// finalized, it will perform the associated finalization step.
pub const ManagedObject = packed struct {
    header: Object.Header,
    value: Value,

    pub fn create(heap: *Heap, managed_type: ManagedType, value: Value) !*ManagedObject {
        const managed_map = try getOrCreateManagedMap(heap);

        const memory_area = try heap.allocateInObjectSegment(requiredSizeForAllocation());
        const self = @ptrCast(*ManagedObject, memory_area);
        self.init(managed_map, managed_type, value);

        try heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(self: *ManagedObject, map: Value, managed_type: ManagedType, value: Value) void {
        self.header.init(.Managed, map);
        self.setManagedType(managed_type);
        self.value = value;
    }

    fn setManagedType(self: *ManagedObject, managed_type: ManagedType) void {
        self.header.object_information = (self.header.object_information & ~ManagedTypeMask) | @enumToInt(managed_type);
    }

    pub fn getManagedType(self: *ManagedObject) ManagedType {
        const raw_managed_type = self.header.object_information & ManagedTypeMask;
        return std.meta.intToEnum(ManagedType, raw_managed_type) catch |err| switch (err) {
            std.meta.IntToEnumError.InvalidEnumTag => std.debug.panic(
                "Unexpected managed type {x} on object at {*}\n",
                .{ raw_managed_type >> ManagedTypeShift, self },
            ),
        };
    }

    pub fn finalize(self: *ManagedObject, allocator: Allocator) void {
        _ = allocator;
        switch (self.getManagedType()) {
            .FileDescriptor => {
                var fd = FileDescriptor.fromValue(self.value);
                fd.close();
            },
        }
    }

    pub fn asObjectAddress(self: *ManagedObject) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *ManagedObject) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getSizeInMemory(self: *ManagedObject) usize {
        _ = self;
        // NOTE: Managed map will have been created at this point.
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return requiredSizeForManagedMap() + @sizeOf(ManagedObject);
    }
};
