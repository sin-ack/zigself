// Copyright (c) 2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Map = @import("map.zig").Map;
const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const ValueSlot = @import("object_lookup.zig").ValueSlot;
const BaseObject = @import("base_object.zig").BaseObject;
const LookupTarget = @import("object_lookup.zig").LookupTarget;

/// An entry in an executable map's inline cache. For the moment, all inline
/// caches are monomorphic.
pub const InlineCacheEntry = struct {
    /// The map for which the cache was saved. If this is null, this entry is
    /// empty.
    map: ?Map.Ptr,
    /// The object on which the looked up slot was found. If this is null and
    /// `map` is not null, this means the lookup result was on the object
    /// itself.
    object: ?Object.Ptr,
    /// The index of the value slot to retrieve from the object when a cache
    /// hit occurs.
    value_slot_index: usize,

    // --- Construction ---

    /// Create a new empty inline cache entry.
    pub fn init() InlineCacheEntry {
        return .{
            .map = null,
            .object = null,
            .value_slot_index = 0,
        };
    }

    // --- Accessors ---

    /// Get the value at the `slot_index`th slot of the object in this inline
    /// cache entry, given the `receiver` the lookup was performed on.
    pub fn getValueSlot(self: InlineCacheEntry, map: Map.Ptr, receiver: Value) ?ValueSlot {
        if (self.map != map) {
            // Cache miss.
            return null;
        }

        const target_object = if (self.object) |obj|
            // The result of the lookup was somewhere in the prototype chain.
            obj
        else
            // When we last looked up the slot, it was on the object itself,
            // which means each object will contain its own version of that
            // slot.
            receiver.unsafeAsObject();

        return target_object.getValueSlot(self.value_slot_index);
    }

    /// Write the given lookup target into this entry. This will overwrite
    /// any existing entry.
    pub fn write(self: *InlineCacheEntry, map: Map.Ptr, receiver: Value, target: LookupTarget) void {
        const object_to_write = if (receiver.asObject()) |object| blk: {
            if (target.object == object) {
                // The value was found on the object itself, so we need to mark
                // it as such.
                break :blk null;
            } else {
                // The value was found on the prototype chain, so we need to
                // store the object it was found on.
                break :blk target.object;
            }
        } else target.object;

        self.map = map;
        self.object = object_to_write;
        self.value_slot_index = target.value_slot_index;
    }

    // --- Heap interaction ---

    /// Visit the edges of this inline cache entry.
    pub fn visitEdges(self: *InlineCacheEntry, visitor: anytype, containing_object: BaseObject.Ptr) !void {
        // FIXME: We shouldn't strongly reference the map! This will keep it
        //        alive for much longer than necessary (until the inline cache
        //        is overwritten or the map/block is collected). To fix this,
        //        the heap needs to support weak references.
        if (self.map) |map| {
            var map_value = map.asValue();
            try visitor.visit(&map_value, containing_object);
            self.map = map_value.unsafeAsMap();
        }

        if (self.object) |object| {
            var object_value = object.asValue();
            try visitor.visit(&object_value, containing_object);
            self.object = object_value.unsafeAsObject();
        }
    }
};
