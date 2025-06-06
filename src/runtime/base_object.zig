// Copyright (c) 2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Actor = @import("Actor.zig");
const debug = @import("../debug.zig");
const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const pointer = @import("../utility/pointer.zig");
const Reference = @import("value.zig").Reference;
const ObjectLike = @import("value.zig").ObjectLike;
const heap_import = @import("Heap.zig");
const VirtualMachine = @import("VirtualMachine.zig");

const STRICT_CAST_CHECKING = debug.STRICT_CAST_CHECKING;

fn BaseObjectT(comptime object_type: BaseObject.Type) type {
    return switch (object_type) {
        .Object => Object,
        .Map => Map,
    };
}

/// BaseObject forms a common base ground for objects and maps.
pub const BaseObject = extern struct {
    /// Metadata about the object.
    metadata: Metadata align(@alignOf(u64)),

    /// The type of this base object.
    pub const Type = enum(u1) { Object, Map };
    /// Whether this object is reachable. Globally-reachable objects are
    /// read-only in actor mode.
    pub const Reachability = enum(u1) { Local, Global };
    pub const Metadata = packed struct(u64) {
        /// The value type of this base object. Used for GC.
        value_type: Value.Type = .Object,
        /// The type of this object with respect to whether it's a reference.
        /// Used for casting from Value.
        object_like_type: ObjectLike.Type = .Object,
        /// The type of this base object. A base object can be an object or a map.
        type: Type,
        /// Bits which are unused in BaseObject, but will be used in objects and maps.
        unused: u60 = 0,
        /// The reachability of the base object.
        reachability: Reachability = .Local,
    };

    pub const Ptr = pointer.HeapPtr(BaseObject, .Mutable);

    pub fn fromAddress(address: [*]u64) BaseObject.Ptr {
        const metadata: *Metadata = @ptrCast(address);

        if (builtin.mode == .Debug) {
            if (metadata.value_type != .Object) {
                std.debug.panic("!!! Address {*} is not a valid object!\n", .{address});
            }

            if (metadata.object_like_type != .Object) {
                std.debug.panic("!!! Address {*} is not a valid object!\n", .{address});
            }
        }

        return @ptrCast(address);
    }

    pub fn asObject(self: BaseObject.Ptr) ?Object.Ptr {
        if (self.metadata.type == .Object) return self.unsafeAsObject();
        return null;
    }

    /// Cast to an `Object` without checking. Only call this if you are
    /// absolutely sure that the object is an `Object`; otherwise call
    /// `asObject`.
    pub fn unsafeAsObject(self: BaseObject.Ptr) Object.Ptr {
        if (STRICT_CAST_CHECKING) std.debug.assert(self.metadata.type == .Object);
        return @ptrCast(self);
    }

    pub fn asMap(self: BaseObject.Ptr) ?Map.Ptr {
        if (self.metadata.type == .Map) return self.unsafeAsMap();
        return null;
    }

    /// Cast to a `Map` without checking. Only call this if you are absolutely
    /// sure that the object is a `Map`; otherwise call `asMap`.
    pub fn unsafeAsMap(self: BaseObject.Ptr) Map.Ptr {
        if (STRICT_CAST_CHECKING) std.debug.assert(self.metadata.type == .Map);
        return @ptrCast(self);
    }

    pub fn getAddress(self: BaseObject.Ptr) [*]u64 {
        return @ptrCast(self);
    }

    pub fn asValue(self: BaseObject.Ptr) Value {
        return Value.fromObjectAddress(self.getAddress());
    }

    pub fn asReference(self: BaseObject.Ptr) Reference {
        return @bitCast(self);
    }

    // --- Common methods ---

    /// Perform a dynamic dispatch based on the current object type.
    fn dispatch(self: BaseObject.Ptr, comptime ReturnType: type, comptime name: []const u8, args: anytype) ReturnType {
        return switch (self.metadata.type) {
            inline else => |t| {
                const self_ptr: BaseObjectT(t).Ptr = @ptrCast(self);
                return @call(.auto, @field(BaseObjectT(t), name), .{self_ptr} ++ args);
            },
        };
    }

    /// Get the size of this object (NOT its dependencies) in memory.
    pub fn getSizeInMemory(self: BaseObject.Ptr) usize {
        return self.dispatch(usize, "getSizeInMemory", .{});
    }

    /// Get the size required for cloning this object (including all private
    /// dependencies it has).
    pub fn getSizeForCloning(self: BaseObject.Ptr) usize {
        return self.dispatch(usize, "getSizeForCloning", .{});
    }

    /// Create a new shallow copy of this object. The map is not affected.
    pub fn clone(self: BaseObject.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) Allocator.Error!BaseObject.Ptr {
        // NOTE: Inlining the delegation here because we need to cast the result into the base object.
        return switch (self.metadata.type) {
            inline else => |t| {
                if (!@hasDecl(BaseObjectT(t), "clone")) unreachable;

                const self_ptr: BaseObjectT(t).Ptr = @ptrCast(self);
                return @ptrCast(try self_ptr.clone(allocator, heap, token, actor_id));
            },
        };
    }

    /// Finalize this object. Skip if the object does not support finalization.
    pub fn finalize(self: BaseObject.Ptr, allocator: Allocator) void {
        self.dispatch(void, "finalize", .{allocator});
    }

    /// Visit all the edges of this object.
    pub fn visitEdges(self: BaseObject.Ptr, visitor: anytype) !void {
        const Error = blk: {
            comptime var Visitor = @TypeOf(visitor);
            if (@typeInfo(Visitor) == .pointer) {
                Visitor = @typeInfo(Visitor).pointer.child;
            }

            const visit_info = @typeInfo(@TypeOf(Visitor.visit));
            break :blk @typeInfo(visit_info.@"fn".return_type.?).error_union.error_set;
        };

        return self.dispatch(Error!void, "visitEdges", .{visitor});
    }
};
