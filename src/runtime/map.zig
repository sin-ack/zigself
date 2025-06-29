// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("Actor.zig");
const debug = @import("../debug.zig");
const Value = @import("value.zig").Value;
const pointer = @import("../utility/pointer.zig");
const BaseObject = @import("base_object.zig").BaseObject;
const ObjectLike = @import("value.zig").ObjectLike;
const heap_import = @import("Heap.zig");
const scoped_bits = @import("../utility/scoped_bits.zig");
const VirtualMachine = @import("VirtualMachine.zig");

const STRICT_CAST_CHECKING = debug.STRICT_CAST_CHECKING;

// TODO: Unify MapType and MapRegistry once Zig stops raising false dependency loops.
pub const MapType = enum(u5) {
    Slots,
    Method,
    Block,
    Array,
    // Intrinsic maps
    AddrInfo,
};

/// A registry of map types. Works basically identically to the object registry.
pub const MapRegistry = union(MapType) {
    Slots: @import("objects/slots.zig").SlotsMap,
    Method: @import("objects/method.zig").MethodMap,
    Block: @import("objects/block.zig").BlockMap,
    Array: @import("objects/array.zig").ArrayMap,
    // Intrinsic maps
    AddrInfo: @import("objects/intrinsic/addrinfo.zig").AddrInfoMap,
};

pub fn MapT(comptime map_type: MapType) type {
    const map_type_name = @tagName(map_type);
    inline for (@typeInfo(MapRegistry).@"union".fields) |field| {
        if (std.mem.eql(u8, map_type_name, field.name))
            return field.type;
    }

    @compileError("Unknown map type " ++ @tagName(map_type));
}

/// The map provides immutable metadata about a set of objects. The map is immutable
/// and is not cloned when an object is cloned. It's how zigSelf's object cloning
/// stays efficient: Only the assignable slot values are cloned, the rest is shared.
///
/// In V8 a similar concept exists as "shapes" and "hidden classes", but Self
/// invented it first.
pub const Map = extern struct {
    base_object: BaseObject align(@alignOf(u64)),

    /// The BaseObject metadata is cast to this type in order to utilize the
    /// unused bits.
    pub const Metadata = packed struct(u64) {
        value_type: Value.Type = .Object,
        object_like_type: ObjectLike.Type = .Object,
        base_type: BaseObject.Type = .Map,
        type: MapType,

        /// Unused bits that can be utilized for extra metadata on specialized maps.
        /// Reserve bits from the closest ExtraBits in the inheritance chain to use
        /// it.
        extra: u24 = 0,

        // Object info for slots (these are very common and thus inlined)
        /// The amount of constant slots present on this map. 0 for non-Slots maps.
        slots: u16 = 0,
        /// The amount of assignable slot values present on objects using this map.
        /// 0 for non-Slots maps.
        assignable_slots: u15 = 0,
        /// The reachability of the map.
        reachability: BaseObject.Reachability = .Local,
    };

    pub const ExtraBits = scoped_bits.ScopedBitsOffsetLimit(Metadata, 8, 32);
    pub const Ptr = pointer.HeapPtr(Map, .Mutable);

    pub fn getMetadata(self: Map.Ptr) *Metadata {
        return @ptrCast(&self.base_object.metadata);
    }

    // --- Initialization ---

    /// Initialize the map with the given type.
    pub fn init(self: Ptr, map_type: MapType) void {
        self.getMetadata().* = .{
            .type = map_type,
        };
    }

    // --- Casting ---

    /// If the map is of the given type, return it as that type. Otherwise return null.
    pub fn asType(self: Ptr, comptime map_type: MapType) ?MapT(map_type).Ptr {
        if (self.getMetadata().type == map_type) return self.unsafeAsType(map_type);
        return null;
    }

    /// Force-cast this map to the given map type. This is unsafe and should
    /// only be used when it is absolutely certain that the map is of the given
    /// type. Most code should use `asType` instead.
    pub fn unsafeAsType(self: Ptr, comptime map_type: MapType) MapT(map_type).Ptr {
        if (STRICT_CAST_CHECKING) std.debug.assert(self.getMetadata().type == map_type);
        return @ptrCast(self);
    }

    /// Return the map's address.
    pub fn asAddress(self: Map.Ptr) [*]u64 {
        return @ptrCast(self);
    }

    /// Return the map as a Value.
    pub fn asValue(self: Map.Ptr) Value {
        return Value.fromObjectAddress(self.asAddress());
    }

    // --- Common methods ---

    // TODO: Unify this with the object dispatch code.
    /// How to handle missing methods during dynamic dispatch.
    const MissingMethodHandling = enum {
        /// Require the method to be present. If the method isn't defined on
        /// the map, it is a compile-time error.
        Required,
        /// If the method is not defined on the map, panic.
        Panic,
    };

    /// Dynamically dispatch a method on the map.
    inline fn dispatch(self: Ptr, comptime missing_method_handling: MissingMethodHandling, comptime ReturnType: type, comptime name: []const u8, args: anytype) ReturnType {
        return switch (self.getMetadata().type) {
            inline else => |t| {
                if (!@hasDecl(MapT(t), name)) {
                    switch (missing_method_handling) {
                        .Required => @compileError("!!! " ++ @tagName(t) ++ " does not implement `" ++ name ++ "`, and it is required."),
                        .Panic => @panic("!!! " ++ @tagName(t) ++ " does not implement `" ++ name ++ "`!"),
                    }
                }

                const self_ptr: MapT(t).Ptr = @ptrCast(self);
                return @call(.always_inline, @field(MapT(t), name), .{self_ptr} ++ args);
            },
        };
    }

    pub fn finalize(self: Map.Ptr, allocator: Allocator) void {
        self.dispatch(.Panic, void, "finalize", .{allocator});
    }

    /// Visit all the edges of this map.
    pub fn visitEdges(self: Map.Ptr, visitor: anytype) !void {
        const Error = blk: {
            comptime var Visitor = @TypeOf(visitor);
            if (@typeInfo(Visitor) == .pointer) {
                Visitor = @typeInfo(Visitor).pointer.child;
            }

            const visit_info = @typeInfo(@TypeOf(Visitor.visit));
            break :blk @typeInfo(visit_info.@"fn".return_type.?).error_union.error_set;
        };

        return self.dispatch(.Required, Error!void, "visitEdges", .{visitor});
    }

    pub fn getSizeInMemory(self: Map.Ptr) usize {
        return self.dispatch(.Required, usize, "getSizeInMemory", .{});
    }

    pub fn getSizeForCloning(self: Map.Ptr) usize {
        return self.dispatch(.Required, usize, "getSizeForCloning", .{});
    }

    pub fn clone(self: Map.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) Allocator.Error!Map.Ptr {
        _ = allocator;
        _ = actor_id;

        // NOTE: Inlining the dispatch here because we need to cast the result into the generic object type.
        return switch (self.getMetadata().type) {
            inline else => |t| {
                if (!@hasDecl(MapT(t), "clone")) unreachable;

                const map_ptr: MapT(t).Ptr = @ptrCast(self);
                const result_or_error = map_ptr.clone(heap, token);
                const result = if (@typeInfo(@TypeOf(result_or_error)) == .error_union) try result_or_error else result_or_error;
                return @ptrCast(result);
            },
        };
    }
};
