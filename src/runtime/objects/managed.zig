// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("../Heap.zig");
const debug = @import("../../debug.zig");
const Object = @import("../object.zig").Object;
const value_import = @import("../value.zig");
const GenericValue = value_import.Value;
const stage2_compat = @import("../../utility/stage2_compat.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// A file descriptor value consists of two things:
/// - 30 bits of flags.
/// - 32 bits for the actual file descriptor.
pub const FileDescriptor = packed struct(u62) {
    flags: Flags,
    fd: std.os.fd_t,

    pub const Flags = packed struct(u30) {
        // Whether the FD is already closed.
        is_closed: bool = false,
        // Whether the FD should be closed during finalization.
        close_during_finalization: bool = true,
        // Reserved for future use.
        reserved: u28 = 0,
    };

    pub fn adopt(fd: std.os.fd_t, flags: Flags) FileDescriptor {
        return .{
            .fd = fd,
            .flags = flags,
        };
    }

    pub fn fromValue(value: GenericValue) FileDescriptor {
        const raw_value = value.asUnsignedInteger();
        return @bitCast(FileDescriptor, @intCast(u62, raw_value));
    }

    pub fn toValue(self: FileDescriptor) GenericValue {
        return GenericValue.fromUnsignedInteger(@bitCast(u62, self));
    }

    pub fn close(self: *FileDescriptor) void {
        if (self.flags.is_closed) return;
        std.os.close(self.fd);
        self.flags.is_closed = true;
    }

    pub fn finalize(self: *FileDescriptor) void {
        if (!self.flags.close_during_finalization) return;
        self.close();
    }
};

/// An object containing a managed value, and its type. When this object is
/// finalized, it will perform the associated finalization step.
pub const Managed = extern struct {
    object: Object align(@alignOf(u64)),
    value: GenericValue align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(Managed, .Mutable);
    pub const Type = .Managed;
    pub const Value = value_import.ObjectValue(Managed);

    const ManagedType = enum(u8) {
        FileDescriptor = 0b0,
    };

    pub fn create(map_map: Map.Ptr, token: *Heap.AllocationToken, actor_id: u31, managed_type: ManagedType, value: GenericValue) !Managed.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self = @ptrCast(Managed.Ptr, memory_area);
        self.init(actor_id, map_map, managed_type, value);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(self: Managed.Ptr, actor_id: u31, map: Map.Ptr, managed_type: ManagedType, value: GenericValue) void {
        self.object = .{
            .object_information = .{
                .object_type = .Managed,
                .actor_id = actor_id,
            },
            .map = map.asValue(),
        };
        self.setManagedType(managed_type);
        self.value = value;
    }

    fn setManagedType(self: Managed.Ptr, managed_type: ManagedType) void {
        self.object.object_information.extra = @enumToInt(managed_type);
    }

    pub fn getManagedType(self: Managed.Ptr) ManagedType {
        return @intToEnum(ManagedType, self.object.object_information.extra);
    }

    pub fn canFinalize(self: Managed.Ptr) bool {
        _ = self;
        return true;
    }

    pub fn finalize(self: Managed.Ptr, allocator: Allocator) void {
        _ = allocator;
        switch (self.getManagedType()) {
            .FileDescriptor => {
                var fd = FileDescriptor.fromValue(self.value);
                fd.finalize();
            },
        }
    }
    pub fn lookup(self: Managed.Ptr, vm: *VirtualMachine, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = vm;
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("Managed.lookup: Looking at a managed object type: {}\n", .{self.asManaged().getManagedType()});
        if (selector_hash.regular == object_lookup.value_hash) {
            return object_lookup.LookupResult{ .Regular = self.value };
        }

        return object_lookup.LookupResult.nothing;
    }

    pub fn asObjectAddress(self: Managed.Ptr) [*]u64 {
        return @ptrCast([*]u64, self);
    }

    pub fn asValue(self: Managed.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getSizeInMemory(self: Managed.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn getSizeForCloning(self: Managed.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(Managed);
    }

    pub fn humanReadableName() []const u8 {
        return "a managed object";
    }
};
