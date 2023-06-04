// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig").Object;
const stage2_compat = @import("../../utility/stage2_compat.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");

// TODO: Unify MapType and MapRegistry once Zig stops raising false dependency loops.
pub const MapType = enum(u30) {
    MapMap,
    Slots,
    Method,
    Block,
    Array,
    // Intrinsic maps
    AddrInfo,
};

/// A registry of map types. Works basically identically to the object registry.
pub const MapRegistry = union(enum(u30)) {
    MapMap: Map,
    Slots: @import("slots.zig").SlotsMap,
    Method: @import("method.zig").MethodMap,
    Block: @import("block.zig").BlockMap,
    Array: @import("array.zig").ArrayMap,
    // Intrinsic maps
    AddrInfo: @import("intrinsic/addrinfo.zig").AddrInfoMap,
};

pub fn MapT(comptime map_type: MapType) type {
    const map_type_name = @tagName(map_type);
    inline for (@typeInfo(MapRegistry).Union.fields) |field| {
        if (std.mem.eql(u8, map_type_name, field.name))
            return field.type;
    }

    unreachable;
}

pub const Map = extern struct {
    object: Object align(@alignOf(u64)),
    map_information: MapInformation align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(Map, .Mutable);
    const MapInformation = packed struct(u64) {
        marker: u2 = @enumToInt(Value.ValueType.Integer),
        map_type: MapType,
        /// Data placed here won't be touched by the base map object, and can be
        /// used by more specialized maps.
        extra: u32 = 0,
    };

    pub fn init(self: Map.Ptr, map_type: MapType, map_map: Map.Ptr) void {
        self.object = .{
            .object_information = .{
                .object_type = .Map,
                // NOTE: Maps are immutable, so it's fine to consider all of
                //       them as being owned by the global actor.
                .actor_id = 0,
            },
            .map = map_map.asValue(),
        };
        self.map_information = .{
            .map_type = map_type,
        };
    }

    pub fn asAddress(self: Map.Ptr) [*]u64 {
        return @ptrCast([*]u64, self);
    }

    pub fn asValue(self: Map.Ptr) Value {
        return Value.fromObjectAddress(self.asAddress());
    }

    /// Delegate to the correct method for each map type.
    fn delegate(self: Ptr, comptime ReturnType: type, comptime name: []const u8, args: anytype) ReturnType {
        return switch (self.map_information.map_type) {
            // Breaks the cycle.
            .MapMap => @call(.auto, @field(Map, name ++ "MapMap"), .{self} ++ args),
            inline else => |t| @call(.auto, @field(MapT(t), name), .{@ptrCast(MapT(t).Ptr, self)} ++ args),
        };
    }

    /// If the map is of the given type, return it as that type. Otherwise return null.
    pub fn asType(self: Ptr, comptime map_type: MapType) ?MapT(map_type).Ptr {
        if (self.map_information.map_type != map_type) {
            return null;
        }

        return @ptrCast(MapT(map_type).Ptr, self);
    }

    /// Return the map as the given type, panicking in safe release modes if
    /// it's not the correct type. In release modes without runtime safety,
    /// asking for a map of different type is undefined behavior.
    pub fn mustBeType(self: Ptr, comptime map_type: MapType) MapT(map_type).Ptr {
        if (std.debug.runtime_safety) {
            if (self.asType(map_type)) |ptr| {
                return ptr;
            }

            std.debug.panic("!!! mustBeType tried to cast a {*} of type '{s}' to type '{s}'!", .{ self, @tagName(self.map_information.map_type), @tagName(map_type) });
        }

        return @ptrCast(MapT(map_type).Ptr, self);
    }

    // Delegated functions

    pub fn canFinalize(self: Map.Ptr) bool {
        return self.delegate(bool, "canFinalize", .{});
    }

    pub fn finalize(self: Map.Ptr, allocator: Allocator) void {
        std.debug.assert(self.canFinalize());
        self.delegate(void, "finalize", .{allocator});
    }

    pub fn getSizeInMemory(self: Map.Ptr) usize {
        return self.delegate(usize, "getSizeInMemory", .{});
    }

    pub fn getSizeForCloning(self: Map.Ptr) usize {
        return self.delegate(usize, "getSizeForCloning", .{});
    }

    pub fn clone(self: Map.Ptr, token: *Heap.AllocationToken, actor_id: u31) Allocator.Error!Map.Ptr {
        _ = actor_id;

        // NOTE: Inlining the delegation here because we need to cast the result into the generic object type.
        return switch (self.map_information.map_type) {
            .MapMap => self,
            inline else => |t| {
                if (!@hasDecl(MapT(t), "clone")) unreachable;

                const map_map = self.object.getMap();
                const result_or_error = @ptrCast(MapT(t).Ptr, self).clone(map_map, token);
                const result = if (@typeInfo(@TypeOf(result_or_error)) == .ErrorUnion) try result_or_error else result_or_error;
                return @ptrCast(Map.Ptr, result);
            },
        };
    }

    pub fn lookup(self: Map.Ptr, vm: *VirtualMachine, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = self;
        _ = vm;
        _ = selector_hash;
        _ = previously_visited;

        @panic("Attempted to call Map.lookup");
    }

    // Map-map

    pub fn requiredSizeForAllocatingMapMap() usize {
        return @sizeOf(Map);
    }

    /// Create the special map-map object.
    pub fn createMapMap(token: *Heap.AllocationToken) Value {
        const size = requiredSizeForAllocatingMapMap();
        var memory_area = token.allocate(.Object, size);

        var map_map = @ptrCast(Map.Ptr, memory_area);
        // A defined undefined value.
        map_map.init(.MapMap, @intToPtr(Map.Ptr, 0x13370));

        // FIXME: This is kinda crude. Let's give ourselves a way to set this
        //        after-the-fact without reaching into the header of the object.
        map_map.object.map = map_map.asValue();
        return map_map.object.map;
    }

    pub fn getSizeInMemoryMapMap(self: Map.Ptr) usize {
        _ = self;
        return requiredSizeForAllocatingMapMap();
    }

    pub fn getSizeForCloningMapMap(self: Map.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn canFinalizeMapMap(self: Map.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalizeMapMap(self: Map.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;

        @panic("Attempted to call Map.finalizeMapMap");
    }
};
