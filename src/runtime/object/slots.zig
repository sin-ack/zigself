// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Map = Object.Map;
const hash = @import("../../utility/hash.zig");
const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");

/// A slots object. A slots object does not contain all the slots that are
/// actually in the object; for that, the map must be consulted. The slot
/// object begins with a header followed by Values for each of the
/// assignable slots.
pub const Slots = packed struct {
    header: Object.Header,

    pub fn create(heap: *Heap, map: *Map.Slots, assignable_slot_values: []Value) !*Slots {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Slots, memory_area);
        self.init(map.asValue(), assignable_slot_values);

        return self;
    }

    fn init(self: *Slots, map: Value, assignable_slot_values: []Value) void {
        self.header.init(.Slots, map);
        std.mem.copy(Value, self.getAssignableSlotValues(), assignable_slot_values);
    }

    pub fn getMap(self: *Slots) *Map.Slots {
        return self.header.getMap().asSlotsMap();
    }

    pub fn getSizeInMemory(self: *Slots) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn asValue(self: *Slots) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    /// Returns a slice of `Value`s for the assignable slots that are after the
    /// Slots object header.
    pub fn getAssignableSlotValues(self: *Slots) []Value {
        const slots_header_size = @sizeOf(Slots);
        const object_memory = @ptrCast([*]u8, self);
        const assignable_slot_count = self.getMap().getAssignableSlotCount();

        return std.mem.bytesAsSlice(
            Value,
            object_memory[slots_header_size .. slots_header_size + assignable_slot_count * @sizeOf(Value)],
        );
    }

    /// Attempts to find the pointer to the `Value` for the assignable slot
    /// for the given hash. If the assignable slot does not exist, null is
    /// returned.
    pub fn getAssignableSlotValueByHash(self: *Slots, hash_value: u32) ?*Value {
        var values = self.getAssignableSlotValues();
        var cursor: usize = 0;

        for (self.getMap().getSlots()) |slot| {
            if (slot.isMutable()) {
                if (slot.hash == hash_value) {
                    return &values[cursor];
                } else {
                    cursor += 1;
                }
            }
        }

        return null;
    }

    /// Attempts to find the pointer to the `Value` for the assignable slot
    /// with the given name. If the assignable slot does not exist, null is
    /// returned.
    pub fn getAssignableSlotValueByName(self: *Slots, name: []const u8) ?*Value {
        const name_hash = hash.stringHash(name);
        return self.getAssignableSlotValueByHash(name_hash);
    }

    fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return @sizeOf(Object.Header) + assignable_slot_count * @sizeOf(Value);
    }
};

/// An activation object, which is just a slots object but with an extra
/// "receiver" value on its map that is the actual value on which a message was
/// activated.
pub const Activation = packed struct {
    slots: Slots,

    pub fn create(heap: *Heap, map: *Map.Activation, assignable_slot_values: []Value) !*Activation {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Activation, memory_area);
        self.init(map.asValue(), assignable_slot_values);

        return self;
    }

    fn init(self: *Activation, map: Value, assignable_slot_values: []Value) void {
        self.slots.init(map, assignable_slot_values);
        self.slots.header.init(.Slots, map);
    }

    pub fn getMap(self: *Activation) *Map.Activation {
        return self.slots.header.getMap().asActivationMap();
    }

    pub fn getSizeInMemory(self: *Activation) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn asValue(self: *Activation) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getAssignableSlotValues(self: *Activation) []Value {
        return self.slots.getAssignableSlotValues();
    }

    pub fn getAssignableSlotValueByHash(self: *Activation, hash_value: u32) ?*Value {
        return self.slots.getAssignableSlotValueByHash(hash_value);
    }

    pub fn getAssignableSlotValueByName(self: *Activation, name: []const u8) ?*Value {
        return self.slots.getAssignableSlotValueByName(name);
    }

    pub fn findActivationReceiver(self: *Activation) Value {
        var object = self.asValue().asObject();
        while (object.isActivationObject()) {
            const receiver = object.asActivationObject().getMap().receiver;
            if (receiver.isObjectReference()) {
                object = receiver.asObject();
            }
        }

        std.debug.assert(@ptrToInt(object.getAddress()) != @ptrToInt(self));
        return object.asValue();
    }

    fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return Slots.requiredSizeForAllocation(assignable_slot_count);
    }
};

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = packed struct {
    slots: Slots,

    pub fn create(heap: *Heap, map: *Map.Method, assignable_slot_values: []Value) !*Method {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Method, memory_area);
        self.init(map.asValue(), assignable_slot_values);

        return self;
    }

    fn init(self: *Method, map: Value, assignable_slot_values: []Value) void {
        self.slots.init(map, assignable_slot_values);
        self.slots.header.init(.Method, map);
    }

    pub fn getMap(self: *Method) *Map.Method {
        return self.slots.header.getMap().asMethodMap();
    }

    pub fn getSizeInMemory(self: *Method) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn asValue(self: *Method) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getArgumentSlotValues(self: *Method) []Value {
        return self.slots.getAssignableSlotValues()[0..self.getMap().getArgumentSlotCount()];
    }

    pub fn getRegularAssignableSlotValues(self: *Method) []Value {
        return self.slots.getAssignableSlotValues()[self.getMap().getArgumentSlotCount()..];
    }

    pub fn getAssignableSlotValueByHash(self: *Method, hash_value: u32) ?*Value {
        return self.slots.getAssignableSlotValueByHash(hash_value);
    }

    pub fn getAssignableSlotValueByName(self: *Method, name: []const u8) ?*Value {
        return self.slots.getAssignableSlotValueByName(name);
    }

    fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return Slots.requiredSizeForAllocation(assignable_slot_count);
    }
};

/// A block object. A block object is a slots object with a block map as its
/// parent.
pub const Block = packed struct {
    slots: Slots,

    pub fn create(heap: *Heap, map: *Map.Block, assignable_slot_values: []Value) !*Block {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Block, memory_area);
        self.init(map.asValue(), assignable_slot_values);

        return self;
    }

    fn init(self: *Block, map: Value, assignable_slot_values: []Value) void {
        self.slots.init(map, assignable_slot_values);
        self.slots.header.init(.Block, map);
    }

    pub fn getMap(self: *Block) *Map.Block {
        return self.slots.header.getMap().asBlockMap();
    }

    pub fn getSizeInMemory(self: *Block) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn asValue(self: *Block) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getArgumentSlotValues(self: *Block) []Value {
        return self.slots.getAssignableSlotValues()[0..self.getMap().getArgumentSlotCount()];
    }

    pub fn getRegularAssignableSlotValues(self: *Block) []Value {
        return self.slots.getAssignableSlotValues()[self.getMap().getArgumentSlotCount()..];
    }

    pub fn getAssignableSlotValueByHash(self: *Block, hash_value: u32) ?*Value {
        return self.slots.getAssignableSlotValueByHash(hash_value);
    }

    pub fn getAssignableSlotValueByName(self: *Block, name: []const u8) ?*Value {
        return self.slots.getAssignableSlotValueByName(name);
    }

    fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return Slots.requiredSizeForAllocation(assignable_slot_count);
    }
};
