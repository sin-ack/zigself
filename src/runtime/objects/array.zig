// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("../Heap.zig");
const debug = @import("../../debug.zig");
const Object = @import("../object.zig").Object;
const IntegerValue = value_import.IntegerValue;
const GenericValue = @import("../value.zig").Value;
const value_import = @import("../value.zig");
const pointer = @import("../../utility/pointer.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

pub const Array = extern struct {
    object: Object align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Array, .Mutable);
    pub const Value = value_import.ObjectValue(Array);

    /// Create a new array with the given values and filling extra items with
    /// the filler value. If filler value is null, expects values to be at least
    /// as long as the size described in the map. If values is longer than the
    /// size N specified in the map, copies the first N items.
    pub fn createWithValues(token: *Heap.AllocationToken, actor_id: u31, map: ArrayMap.Ptr, values: []GenericValue, filler: ?GenericValue) Array.Ptr {
        if (filler == null and values.len < map.getSize()) {
            std.debug.panic(
                "!!! Array.createWithValues given values slice that's too short, and no filler was given!",
                .{},
            );
        }

        const size = requiredSizeForAllocation(map.getSize());

        const memory_area = token.allocate(.Object, size);
        var self: Array.Ptr = @ptrCast(memory_area);
        self.init(actor_id, map, values, filler);

        return self;
    }

    fn init(self: Array.Ptr, actor_id: u31, map: ArrayMap.Ptr, values: []GenericValue, filler: ?GenericValue) void {
        self.object = .{
            .object_information = .{
                .object_type = .Array,
                .actor_id = actor_id,
            },
            .map = map.asValue(),
        };

        const values_to_copy = values[0..@min(values.len, map.getSize())];
        @memcpy(self.getValues()[0..values_to_copy.len], values_to_copy);

        if (map.getSize() > values.len) {
            @memset(self.getValues()[values.len..], filler.?);
        }
    }

    pub fn asObjectAddress(self: Array.Ptr) [*]u64 {
        return @ptrCast(@alignCast(self));
    }

    pub fn asValue(self: Array.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getMap(self: Array.Ptr) ArrayMap.Ptr {
        return self.object.getMap().mustBeType(.Array);
    }

    pub fn getSize(self: Array.Ptr) usize {
        return self.getMap().getSize();
    }

    pub fn canFinalize(self: Array.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: Array.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call Array.finalize");
    }

    pub fn lookup(self: Array.Ptr, vm: *VirtualMachine, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = self;
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("Array.lookup: Looking at traits array\n", .{});
        const array_traits = vm.array_traits.getValue();
        if (selector_hash.regular == object_lookup.parent_hash)
            return object_lookup.LookupResult{ .Regular = array_traits };

        return array_traits.lookupByHash(vm, selector_hash);
    }

    pub fn getValues(self: Array.Ptr) []GenericValue {
        const object_memory: [*]u8 = @ptrCast(self);
        const start_of_items = object_memory + @sizeOf(Array);

        return @alignCast(std.mem.bytesAsSlice(GenericValue, start_of_items[0 .. self.getSize() * @sizeOf(GenericValue)]));
    }

    pub fn clone(self: Array.Ptr, vm: *VirtualMachine, token: *Heap.AllocationToken, actor_id: u31) Array.Ptr {
        _ = vm;
        return createWithValues(token, actor_id, self.getMap(), self.getValues(), null);
    }

    pub fn getSizeInMemory(self: Array.Ptr) usize {
        return requiredSizeForAllocation(self.getSize());
    }

    pub fn getSizeForCloning(self: Array.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation(size: usize) usize {
        return @sizeOf(Array) + size * @sizeOf(GenericValue);
    }

    pub fn humanReadableName() []const u8 {
        return "an array";
    }
};

/// A map for an array object.
pub const ArrayMap = extern struct {
    map: Map align(@alignOf(u64)),
    size: IntegerValue(.Unsigned) align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(ArrayMap, .Mutable);

    pub fn create(map_map: Map.Ptr, token: *Heap.AllocationToken, size: usize) ArrayMap.Ptr {
        const memory_size = requiredSizeForAllocation();

        const memory_area = token.allocate(.Object, memory_size);
        var self: ArrayMap.Ptr = @ptrCast(memory_area);
        self.init(map_map, size);

        return self;
    }

    fn init(self: ArrayMap.Ptr, map_map: Map.Ptr, size: usize) void {
        self.map.init(.Array, map_map);
        self.size = IntegerValue(.Unsigned).init(@as(u64, size));
    }

    pub fn asValue(self: ArrayMap.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(@ptrCast(@alignCast(self)));
    }

    pub fn getSize(self: ArrayMap.Ptr) usize {
        return @intCast(self.size.get());
    }

    pub fn getSizeInMemory(self: ArrayMap.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn getSizeForCloning(self: ArrayMap.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ArrayMap);
    }

    pub fn clone(self: ArrayMap.Ptr, vm: *VirtualMachine, token: *Heap.AllocationToken) ArrayMap.Ptr {
        return create(vm.getMapMap(), token, self.getSize());
    }

    pub fn canFinalize(self: ArrayMap.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: ArrayMap.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call ArrayMap.finalize");
    }
};
