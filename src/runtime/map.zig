// Copyright (c) 2021-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("Heap.zig");
const Actor = @import("Actor.zig");
const Value = @import("value.zig").Value;
const pointer = @import("../utility/pointer.zig");
const BaseObject = @import("base_object.zig").BaseObject;
const scoped_bits = @import("../utility/scoped_bits.zig");

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

// Comptime map checks
comptime {
    for (@typeInfo(MapRegistry).Union.fields) |field| {
        // Ensure that all objects are at least two machine words long. This is
        // required because when we overwrite objects with forwarding addresses,
        // the second word is used to store the new location (we can't use the
        // first word because that's the object header).
        if (@sizeOf(field.type) < 2 * @sizeOf(u64))
            @compileError("!!! Map " ++ @typeName(field.type) ++ " must be at least 2 machine words wide!");
    }
}

pub fn MapT(comptime map_type: MapType) type {
    const map_type_name = @tagName(map_type);
    inline for (@typeInfo(MapRegistry).Union.fields) |field| {
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
        marker: BaseObject.Marker = @intFromEnum(Value.ValueType.ObjectMarker),
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
        if (self.getMetadata().type == map_type) return @ptrCast(self);
        return null;
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

    /// Dynamically dispatch a method on the map.
    fn dispatch(self: Ptr, comptime ReturnType: type, comptime name: []const u8, args: anytype) ReturnType {
        return switch (self.getMetadata().type) {
            inline else => |t| {
                const self_ptr: MapT(t).Ptr = @ptrCast(self);
                return @call(.auto, @field(MapT(t), name), .{self_ptr} ++ args);
            },
        };
    }

    pub fn canFinalize(self: Map.Ptr) bool {
        return self.dispatch(bool, "canFinalize", .{});
    }

    pub fn finalize(self: Map.Ptr, allocator: Allocator) void {
        std.debug.assert(self.canFinalize());
        self.dispatch(void, "finalize", .{allocator});
    }

    pub fn getSizeInMemory(self: Map.Ptr) usize {
        return self.dispatch(usize, "getSizeInMemory", .{});
    }

    pub fn getSizeForCloning(self: Map.Ptr) usize {
        return self.dispatch(usize, "getSizeForCloning", .{});
    }

    pub fn clone(self: Map.Ptr, token: *Heap.AllocationToken, actor_id: Actor.ActorID) Allocator.Error!Map.Ptr {
        _ = actor_id;

        // NOTE: Inlining the dispatch here because we need to cast the result into the generic object type.
        return switch (self.getMetadata().type) {
            inline else => |t| {
                if (!@hasDecl(MapT(t), "clone")) unreachable;

                const map_ptr: MapT(t).Ptr = @ptrCast(self);
                const result_or_error = map_ptr.clone(token);
                const result = if (@typeInfo(@TypeOf(result_or_error)) == .ErrorUnion) try result_or_error else result_or_error;
                return @ptrCast(result);
            },
        };
    }
};
