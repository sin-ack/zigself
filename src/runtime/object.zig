// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const slots_objects = @import("./object/slots.zig");
const byte_vector_object = @import("./object/byte_vector.zig");
const vector_object = @import("./object/vector.zig");
const map_objects = @import("./object/map.zig");

const Self = @This();

pub const Slots = slots_objects.Slots;
pub const Activation = slots_objects.Activation;
pub const Method = slots_objects.Method;
pub const Block = slots_objects.Block;
pub const ByteVector = byte_vector_object.ByteVectorObject;
pub const Vector = vector_object.VectorObject;
pub const Map = map_objects.Map;

pub usingnamespace @import("./object/lookup.zig");

const ObjectTypeShift = 2;
const ObjectTypeMask: u64 = 0b111 << ObjectTypeShift;

/// The object types that are available in the system.
const ObjectType = enum(u64) {
    ForwardingReference = 0b000 << ObjectTypeShift,
    Slots = 0b001 << ObjectTypeShift,
    Activation = 0b010 << ObjectTypeShift,
    Method = 0b011 << ObjectTypeShift,
    Block = 0b100 << ObjectTypeShift,
    ByteVector = 0b101 << ObjectTypeShift,
    Vector = 0b110 << ObjectTypeShift,
    Map = 0b111 << ObjectTypeShift,
};

header: *align(@alignOf(u64)) Header,

pub fn addressIsObject(address: [*]u64) bool {
    return address[0] & Value.ValueMarkerMask == @enumToInt(Value.ValueType.ObjectMarker);
}

pub fn fromAddress(address: [*]u64) Self {
    if (!addressIsObject(address)) {
        std.debug.panic("!!! Object.fromAddress got an address that does NOT contain an object marker!", .{});
    }

    return .{ .header = @ptrCast(*align(@alignOf(u64)) Header, address) };
}

pub fn getAddress(self: Self) [*]u64 {
    return @ptrCast([*]u64, self.header);
}

pub fn getSizeInMemory(self: Self) usize {
    return switch (self.header.getObjectType()) {
        .ForwardingReference => unreachable,
        .Slots => self.asSlotsObject().getSizeInMemory(),
        .Activation => self.asActivationObject().getSizeInMemory(),
        .Method => self.asMethodObject().getSizeInMemory(),
        .Block => self.asBlockObject().getSizeInMemory(),
        .ByteVector => self.asByteVectorObject().getSizeInMemory(),
        .Vector => self.asVectorObject().getSizeInMemory(),
        .Map => self.asMap().getSizeInMemory(),
    };
}

pub fn format(self: Self) std.fmt.Formatter(formatObject) {
    return .{ .data = self.header };
}

fn formatObject(
    header: *align(@alignOf(u64)) Header,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;

    try writer.writeAll("Object(");
    switch (header.getObjectType()) {
        .ForwardingReference => {
            try writer.writeAll("ForwardingReference){ .forward_address = 0x");
            try std.fmt.formatInt(@ptrToInt((Self{ .header = header }).getForwardAddress()), 16, .lower, options, writer);
            try writer.writeAll(" }");
        },
        .Slots => {
            try writer.writeAll("Slots){ .assignable_slots = ");
            try std.fmt.formatInt(@ptrCast(*Slots, header).getMap().getAssignableSlotCount(), 10, .lower, options, writer);
            try writer.writeAll(" }");
        },
        .Activation => {
            try writer.writeAll("Activation){ .assignable_slots = ");
            try std.fmt.formatInt(@ptrCast(*Activation, header).getMap().getAssignableSlotCount(), 10, .lower, options, writer);
            try writer.writeAll(" }");
        },
        .Map => {
            var map = @ptrCast(*Map, header);
            try writer.writeAll("Map(");

            switch (map.getMapType()) {
                .Slots => {
                    try writer.writeAll("Slots)) { .slot_count = ");
                    try std.fmt.formatInt(map.asSlotsMap().slot_count, 10, .lower, options, writer);
                    try writer.writeAll(", .assignable_slots = ");
                    try std.fmt.formatInt(map.asSlotsMap().getAssignableSlotCount(), 10, .lower, options, writer);
                    try writer.writeAll(" }");
                },
                .Activation => {
                    try writer.writeAll("Activation)) { .slot_count = ");
                    try std.fmt.formatInt(map.asActivationMap().slots_map.slot_count, 10, .lower, options, writer);
                    try writer.writeAll(", .assignable_slots = ");
                    try std.fmt.formatInt(map.asActivationMap().getAssignableSlotCount(), 10, .lower, options, writer);
                    try writer.writeAll(" }");
                },
            }
        },
    }
}

pub fn finalize(self: Self, allocator: Allocator) void {
    switch (self.header.getObjectType()) {
        .ForwardingReference, .Slots, .Activation, .Method, .Block, .Vector, .ByteVector => unreachable,
        .Map => self.asMap().finalize(allocator),
    }
}

pub fn asValue(self: Self) Value {
    return Value.fromObjectAddress(self.getAddress());
}

pub fn clone(self: Self, heap: *Heap) !Self {
    return switch (self.header.getObjectType()) {
        .ForwardingReference, .Activation, .Method, .Map => unreachable,
        .Slots => fromAddress((try self.asSlotsObject().clone(heap)).asObjectAddress()),
        .Block => fromAddress((try self.asBlockObject().clone(heap)).asObjectAddress()),
        .ByteVector => fromAddress((try self.asByteVectorObject().clone(heap)).asObjectAddress()),
        .Vector => fromAddress((try self.asVectorObject().clone(heap)).asObjectAddress()),
    };
}

// Slots objects

pub fn isSlotsObject(self: Self) bool {
    return self.header.getObjectType() == .Slots;
}

fn mustBeSlotsObject(self: Self) void {
    if (!self.isSlotsObject()) {
        std.debug.panic("Expected the object at {*} to be a slots object", .{self.header});
    }
}

pub fn asSlotsObject(self: Self) *Slots {
    self.mustBeSlotsObject();
    return @ptrCast(*Slots, self.header);
}

// Activation objects

pub fn isActivationObject(self: Self) bool {
    return self.header.getObjectType() == .Activation;
}

fn mustBeActivationObject(self: Self) void {
    if (!self.isActivationObject()) {
        std.debug.panic("Expected the object at {*} to be a activation object", .{self.header});
    }
}

pub fn asActivationObject(self: Self) *Activation {
    self.mustBeActivationObject();
    return @ptrCast(*Activation, self.header);
}

// Method objects

pub fn isMethodObject(self: Self) bool {
    return self.header.getObjectType() == .Method;
}

fn mustBeMethodObject(self: Self) void {
    if (!self.isMethodObject()) {
        std.debug.panic("Expected the object at {*} to be a method object", .{self.header});
    }
}

pub fn asMethodObject(self: Self) *Method {
    self.mustBeMethodObject();
    return @ptrCast(*Method, self.header);
}

// Block objects

pub fn isBlockObject(self: Self) bool {
    return self.header.getObjectType() == .Block;
}

fn mustBeBlockObject(self: Self) void {
    if (!self.isBlockObject()) {
        std.debug.panic("Expected the object at {*} to be a block object", .{self.header});
    }
}

pub fn asBlockObject(self: Self) *Block {
    self.mustBeBlockObject();
    return @ptrCast(*Block, self.header);
}

// Byte vector objects

pub fn isByteVectorObject(self: Self) bool {
    return self.header.getObjectType() == .ByteVector;
}

fn mustBeByteVectorObject(self: Self) void {
    if (!self.isByteVectorObject()) {
        std.debug.panic("Expected the object at {*} to be a byte vector object", .{self.header});
    }
}

pub fn asByteVectorObject(self: Self) *ByteVector {
    self.mustBeByteVectorObject();
    return @ptrCast(*ByteVector, self.header);
}

// Vector objects

pub fn isVectorObject(self: Self) bool {
    return self.header.getObjectType() == .Vector;
}

fn mustBeVectorObject(self: Self) void {
    if (!self.isVectorObject()) {
        std.debug.panic("Expected the object at {*} to be a vector object", .{self.header});
    }
}

pub fn asVectorObject(self: Self) *Vector {
    self.mustBeVectorObject();
    return @ptrCast(*Vector, self.header);
}

// Map objects

pub fn isMap(self: Self) bool {
    return self.header.getObjectType() == .Map;
}

fn mustBeMap(self: Self) void {
    if (!self.isMap()) {
        std.debug.panic("Expected the object at {*} to be a map", .{self.header});
    }
}

pub fn asMap(self: Self) *Map {
    self.mustBeMap();
    return @ptrCast(*Map, self.header);
}

// Forwarding references

pub fn isForwardingReference(self: Self) bool {
    return self.header.getObjectType() == .ForwardingReference;
}

fn mustBeForwardingReference(self: Self) void {
    if (!self.isForwardingReference()) {
        std.debug.panic("Expected the object at {*} to be a forwarding reference", .{self.header});
    }
}

pub fn getForwardAddress(self: Self) [*]u64 {
    self.mustBeForwardingReference();
    return self.header.map_pointer.asObjectAddress();
}

/// Set the given address as the forwarding address of this object.
/// **Overwrites the current object type.**
pub fn setForwardAddress(self: Self, address: [*]u64) void {
    std.debug.assert(!self.isForwardingReference());

    self.header.setObjectType(.ForwardingReference);
    self.header.map_pointer = Value.fromObjectAddress(address);
}

pub const Header = packed struct {
    object_information: u64,
    map_pointer: Value,

    pub fn init(self: *Header, object_type: ObjectType, map: Value) void {
        self.object_information = @enumToInt(Value.ValueType.ObjectMarker);
        self.setObjectType(object_type);

        self.map_pointer = map;
    }

    pub fn setObjectType(self: *Header, object_type: ObjectType) void {
        self.object_information = (self.object_information & ~ObjectTypeMask) | @enumToInt(object_type);
    }

    pub fn getObjectType(self: *Header) ObjectType {
        // FIXME: Check whether the current object type is a valid one, and
        //        don't let Zig crash us here if it's not.
        return @intToEnum(ObjectType, self.object_information & ObjectTypeMask);
    }

    pub fn getMap(self: *Header) *Map {
        return self.map_pointer.asObject().asMap();
    }
};
