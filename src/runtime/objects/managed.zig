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
/// - 30 bits of flags, currently only used for holding the "closed" state of a fd.
/// - 32 bits for the actual file descriptor.
pub const FileDescriptor = packed struct(u62) {
    flags: u30,
    fd: std.os.fd_t,

    const ClosedFlag: u30 = 1;

    pub fn adopt(fd: std.os.fd_t) FileDescriptor {
        return .{
            .flags = 0,
            .fd = fd,
        };
    }

    pub fn fromValue(value: GenericValue) FileDescriptor {
        const raw_value = value.asUnsignedInteger();
        return .{
            .flags = @intCast(u30, raw_value >> 32),
            .fd = @intCast(std.os.fd_t, raw_value & @as(u64, 0xFFFFFFFF)),
        };
    }

    pub fn toValue(self: FileDescriptor) GenericValue {
        const value: u62 = (@as(u62, self.flags) << 32) | @intCast(u32, self.fd);
        return GenericValue.fromUnsignedInteger(value);
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
                fd.close();
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

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(Managed);
    }

    pub fn humanReadableName() []const u8 {
        return "a managed object";
    }
};
