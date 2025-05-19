// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("../Actor.zig");
const debug = @import("../../debug.zig");
const Object = @import("../object.zig").Object;
const pointer = @import("../../utility/pointer.zig");
const Selector = @import("../Selector.zig");
const heap_import = @import("../Heap.zig");
const value_import = @import("../value.zig");
const GenericValue = value_import.Value;
const LookupResult = @import("../object_lookup.zig").LookupResult;

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// A file descriptor value consists of two things:
/// - 31 bits of flags.
/// - 32 bits for the actual file descriptor.
pub const FileDescriptor = packed struct(GenericValue.Data) {
    flags: Flags,
    fd: std.posix.fd_t,

    pub const Flags = packed struct(u31) {
        // Whether the FD is already closed.
        is_closed: bool = false,
        // Whether the FD should be closed during finalization.
        close_during_finalization: bool = true,
        // Reserved for future use.
        reserved: u29 = 0,
    };

    pub fn adopt(fd: std.posix.fd_t, flags: Flags) FileDescriptor {
        return .{
            .fd = fd,
            .flags = flags,
        };
    }

    pub fn fromValue(value: GenericValue) FileDescriptor {
        const raw_value: GenericValue.Data = @intCast(value.asUnsignedInteger().?);
        return @bitCast(raw_value);
    }

    pub fn toValue(self: FileDescriptor) GenericValue {
        const self_as_int: GenericValue.Data = @bitCast(self);
        return GenericValue.fromUnsignedInteger(self_as_int);
    }

    pub fn close(self: *FileDescriptor) void {
        if (self.flags.is_closed) return;
        std.posix.close(self.fd);
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

    pub const Ptr = pointer.HeapPtr(Managed, .Mutable);
    pub const Type = .Managed;
    pub const Value = value_import.ObjectValue(Managed);

    const ManagedType = enum(u8) {
        FileDescriptor = 0b0,
    };
    pub const ExtraBits = Object.ExtraBits.reserve(ManagedType);

    // XXX: heap is really Heap(anytype), but can't express that in Zig. This is required because
    //      Managed.create is used in a Heap test.
    // TODO: Write Zig interfaces proposal
    pub fn create(heap: anytype, token: *heap_import.AllocationToken, actor_id: Actor.ActorID, managed_type: ManagedType, value: GenericValue) !Managed.Ptr {
        const memory_area = token.allocate(requiredSizeForAllocation());
        const self: Managed.Ptr = @ptrCast(memory_area);
        self.init(actor_id, managed_type, value);

        try heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(self: Managed.Ptr, actor_id: Actor.ActorID, managed_type: ManagedType, value: GenericValue) void {
        self.object.init(.Managed, actor_id);
        self.setManagedType(managed_type);
        self.value = value;
    }

    fn setManagedType(self: Managed.Ptr, managed_type: ManagedType) void {
        Managed.ExtraBits.write(self.object.getMetadata(), managed_type);
    }

    pub fn getManagedType(self: Managed.Ptr) ManagedType {
        return Managed.ExtraBits.read(self.object.getMetadata().*);
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

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: Managed.Ptr, visitor: anytype) !void {
        try visitor.visit(&self.value, @ptrCast(self));
    }

    pub fn lookup(self: Managed.Ptr, selector: Selector, previously_visited: ?*const Selector.VisitedValueLink) LookupResult {
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("Managed.lookup: Looking at a managed object type: {}\n", .{self.getManagedType()});
        if (selector.equals(Selector.well_known.value)) {
            return LookupResult{ .Regular = self.value };
        }

        return LookupResult.nothing;
    }

    pub fn asObjectAddress(self: Managed.Ptr) [*]u64 {
        return @ptrCast(self);
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
