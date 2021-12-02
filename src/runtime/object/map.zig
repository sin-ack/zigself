// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../heap.zig");
const Slot = @import("../slot.zig").Slot;
const hash = @import("../../utility/hash.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");

var static_map_map: ?Value = null;

pub fn getMapMap(heap: *Heap) !Value {
    if (static_map_map) |m| return m;

    var new_map = try heap.allocateInObjectSegment(@sizeOf(SlotsMap));

    // FIXME: Clean this up
    var header = @ptrCast(*Object.Header, new_map);
    header.object_information = 0b11;
    header.setObjectType(.Map);
    var map = @ptrCast(*Map, header);
    map.setMapType(.Slots);
    var slots_map = map.asSlotsMap();
    slots_map.properties = 0;
    slots_map.slot_count = 0;

    var map_value = Value.fromObjectAddress(new_map);
    header.map_pointer = map_value;
    static_map_map = map_value;
    return map_value;
}

// 2 bits for marker + 3 bits for object type
const MapTypeShift = 2 + 3;
const MapTypeMask: u64 = 0b111 << MapTypeShift;

const MapType = enum(u64) {
    Slots = 0b000 << MapTypeShift,
};

pub const Map = packed struct {
    header: Object.Header,

    pub const Slots = SlotsMap;

    fn init(self: *Map, map_type: MapType, map_map: Value) void {
        self.header.init(.Map, map_map);
        self.setMapType(map_type);
    }

    pub fn getMapType(self: *Map) MapType {
        const raw_map_type = self.header.object_information & MapTypeMask;
        return std.meta.intToEnum(MapType, raw_map_type) catch |err| switch (err) {
            std.meta.IntToEnumError.InvalidEnumTag => std.debug.panic(
                "Unexpected map type {x} on object at {*}\n",
                .{ raw_map_type >> MapTypeShift, self },
            ),
        };
    }

    pub fn setMapType(self: *Map, map_type: MapType) void {
        self.header.object_information = (self.header.object_information & ~MapTypeMask) | @enumToInt(map_type);
    }

    pub fn isSlotsMap(self: *Map) bool {
        return self.getMapType() == .Slots;
    }

    fn mustBeSlotsMap(self: *Map) void {
        if (!self.isSlotsMap()) {
            std.debug.panic("Expected the object at {*} to be a slots map", .{self});
        }
    }

    pub fn asSlotsMap(self: *Map) *Slots {
        self.mustBeSlotsMap();
        return @ptrCast(*Slots, self);
    }

    pub fn getSizeInMemory(self: *Map) usize {
        return switch (self.getMapType()) {
            .Slots => self.asSlotsMap().getSizeInMemory(),
        };
    }
};

// NOTE: properties comes *before* the slot count in the struct
//       definition, but comes *after* the slot count in the actual bit
//       definitions.
const SlotsMap = packed struct {
    map: Map,
    /// Slots map properties.
    /// The first byte is the amount of assignable slots the map has.
    /// The other bytes are currently reserved for future use.
    /// The last two bits are zero.
    properties: u32,
    /// The amount of slots. The slots begin after the end of this
    /// field.
    slot_count: u32,

    /// Create a new slots map. Takes the amount of slots this object will have.
    ///
    /// IMPORTANT: All slots *must* be initialized right after creation like this:
    ///
    /// ```
    /// var slots_map = Object.Map.Slots.create(heap, 2);
    /// slots_map.getSlots()[0].initConstant(...);
    /// slots_map.getSlots()[1].initMutable(...);
    /// ```
    pub fn create(heap: *Heap, slot_count: u32) !*SlotsMap {
        const size = requiredSizeForAllocation(slot_count);
        const map_map = try getMapMap(heap);

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*SlotsMap, memory_area);
        self.init(slot_count, map_map);

        return self;
    }

    fn init(self: *SlotsMap, slot_count: u32, map_map: Value) void {
        self.map.init(.Slots, map_map);
        self.properties = 0;
        self.slot_count = slot_count;
    }

    fn getSlotsSlice(self: *SlotsMap) []u8 {
        const total_object_size = @sizeOf(SlotsMap) + self.slot_count * @sizeOf(Slot);
        const map_memory = @ptrCast([*]u8, self);
        return map_memory[@sizeOf(SlotsMap)..total_object_size];
    }

    pub fn getSlots(self: *SlotsMap) []Slot {
        return std.mem.bytesAsSlice(Slot, self.getSlotsSlice());
    }

    pub fn getSizeInMemory(self: *SlotsMap) usize {
        return requiredSizeForAllocation(self.slot_count);
    }

    pub fn asValue(self: *SlotsMap) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    /// Return the amount of assignable slots that this slot map
    /// contains.
    pub fn getAssignableSlotCount(self: *SlotsMap) u8 {
        // 255 assignable slots ought to be enough for everybody.
        return @intCast(u8, self.properties >> 24);
    }

    pub fn setAssignableSlotCount(self: *SlotsMap, count: u8) void {
        self.properties = (self.properties & @as(u32, 0x00FFFFFF)) | (@as(u32, count) << 24);
    }

    pub fn getSlotByHash(self: *SlotsMap, hash_value: u32) ?*Slot {
        for (self.getSlots()) |*slot| {
            if (slot.hash == hash_value) {
                return slot;
            }
        }

        return null;
    }

    pub fn getSlotByName(self: *SlotsMap, string: []const u8) ?*Slot {
        const hash_value = hash.stringHash(string);
        return self.getSlotByHash(hash_value);
    }

    /// Return the size required for the whole map with the given slot
    /// count.
    fn requiredSizeForAllocation(slot_count: u32) usize {
        return @sizeOf(SlotsMap) + slot_count * @sizeOf(Slot);
    }
};
