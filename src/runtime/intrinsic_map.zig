// Copyright (c) 2023-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("Heap.zig");
const Actor = @import("Actor.zig");
const value = @import("value.zig");
const pointer = @import("../utility/pointer.zig");
const Selector = @import("Selector.zig");
const MapObject = @import("object.zig").MapObject;
const ObjectType = @import("object.zig").ObjectType;
const LookupResult = @import("object_lookup.zig").LookupResult;

/// A shaped object makes it easy to define an object with well-known slots for
/// use in primitives. The values can currently only be constants. This function
/// also generates an object type corresponding to the map type automatically.
///
/// Example usage:
///
///     pub const MyObject = MyObjectMap.ObjectType;
///
///     pub const MyObjectMap = extern struct {
///         map: Map,
///         value1: Value align(@alignOf(u64)),
///         value2: Value align(@alignOf(u64)),
///
///         pub fn create(token: *Heap.AllocationToken, ...) MyObjectMap {
///             // Do allocation, initialization here...
///         }
///
///         pub fn clone(self: MyObjectMap.Ptr, token: *Heap.AllocationToken) MyObjectMap.Ptr {
///             return create(map_map, token, ...);
///         }
///
///         pub usingnamespace IntrinsicMap(MyObjectMap);
///     }
pub fn IntrinsicMap(comptime MapT: type, comptime object_type: ObjectType) type {
    const type_info = @typeInfo(MapT);
    if (type_info != .Struct) {
        @compileError("A struct must be passed to IntrinsicMap");
    }

    const struct_info = type_info.Struct;
    if (struct_info.layout != .@"extern") {
        @compileError("IntrinsicMap requires an extern struct");
    }

    const struct_fields = struct_info.fields;
    comptime var field_names: []const []const u8 = &.{};
    inline for (struct_fields, 0..) |field, i| {
        if (i == 0 and field.type != Map) {
            @compileError("The first field of IntrinsicMaps must be Map");
        }

        // TODO: Allow the types derived from Value to also be accepted for
        //       faster VM access.
        if (field.type == value.Value) {
            field_names = field_names ++ &[_][]const u8{field.name};
        }
    }

    return extern struct {
        pub const Ptr = pointer.HeapPtr(MapT, .Mutable);
        pub const ObjectType = IntrinsicObject(MapT, field_names, object_type);

        // The user is responsible for clone, since the create method is also
        // defined by the user.

        pub fn asAddress(self: Ptr) [*]u64 {
            return @ptrCast(self);
        }

        pub fn asValue(self: Ptr) value.Value {
            return value.Value.fromObjectAddress(self.asAddress());
        }

        pub fn getSizeInMemory(self: Ptr) usize {
            _ = self;
            return requiredSizeForAllocation();
        }

        pub fn getSizeForCloning(self: Ptr) usize {
            return self.getSizeInMemory();
        }

        pub fn canFinalize(self: Ptr) bool {
            _ = self;
            // TODO: The user should be able to let themselves be finalized.
            return false;
        }

        pub fn finalize(self: Ptr, allocator: Allocator) void {
            _ = self;
            _ = allocator;
            unreachable;
        }

        pub fn requiredSizeForAllocation() usize {
            return @sizeOf(MapT);
        }

        pub fn createObject(self: Ptr, token: *Heap.AllocationToken, actor_id: Actor.ActorID) @This().ObjectType.Ptr {
            return @This().ObjectType.create(token, self, actor_id);
        }
    };
}

fn IntrinsicObject(comptime MapT: type, comptime field_names: []const []const u8, comptime object_type: ObjectType) type {
    const field_selectors = blk: {
        var selectors: []const Selector = &.{};

        inline for (field_names) |name| {
            selectors = selectors ++ &[_]Selector{Selector.fromName(name)};
        }

        break :blk selectors;
    };

    return extern struct {
        object: MapObject align(@alignOf(u64)),

        const Self = @This();
        pub const Ptr = pointer.HeapPtr(Self, .Mutable);
        pub const Type = object_type;
        pub const Value = value.ObjectValue(Self);

        pub fn create(token: *Heap.AllocationToken, map: MapT.Ptr, actor_id: Actor.ActorID) Ptr {
            const memory = token.allocate(.Object, requiredSizeForAllocation());
            const self: Ptr = @ptrCast(memory);

            self.init(actor_id, map);
            return self;
        }

        fn init(self: Ptr, actor_id: Actor.ActorID, map: MapT.Ptr) void {
            self.object.init(object_type, actor_id, map.asValue());
        }

        pub fn asAddress(self: Ptr) [*]u64 {
            return @ptrCast(self);
        }

        pub fn asValue(self: Ptr) value.Value {
            return value.Value.fromObjectAddress(self.asAddress());
        }

        pub fn getMap(self: Ptr) MapT.Ptr {
            return @ptrCast(self.object.getMap());
        }

        pub fn getSizeInMemory(self: Ptr) usize {
            _ = self;
            return requiredSizeForAllocation();
        }

        pub fn getSizeForCloning(self: Ptr) usize {
            return self.getSizeInMemory();
        }

        pub fn canFinalize(self: Ptr) bool {
            _ = self;
            // TODO: The user should be able to let themselves be finalized.
            return false;
        }

        pub fn finalize(self: Ptr, allocator: Allocator) void {
            _ = self;
            _ = allocator;
            unreachable;
        }

        pub fn lookup(
            self: Ptr,
            selector: Selector,
            previously_visited: ?*const Selector.VisitedValueLink,
        ) LookupResult {
            _ = previously_visited;

            const map = self.getMap();

            inline for (field_selectors) |field_selector| {
                if (selector.equals(field_selector)) {
                    return LookupResult{ .Regular = @field(map, field_selector.name) };
                }
            }

            return LookupResult.nothing;
        }

        pub fn requiredSizeForAllocation() usize {
            return @sizeOf(Self);
        }
    };
}
