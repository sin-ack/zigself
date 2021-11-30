// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Value = @import("./value.zig").Value;
pub const Slots = @import("./object/slots.zig").Slots;
pub const Map = @import("./object/map.zig").Map;

const Self = @This();

const ObjectTypeShift = 2;
const ObjectTypeMask: u64 = 0b111 << ObjectTypeShift;

/// The object types that are available in the system.
const ObjectType = enum(u64) {
    ForwardingReference = 0b000 << ObjectTypeShift,
    Slots = 0b001 << ObjectTypeShift,
    Map = 0b010 << ObjectTypeShift,
};

header: *align(@alignOf(u64)) Header,

pub fn addressIsObject(address: [*]u64) bool {
    return address[0] & Value.ValueMarkerMask == Value.ObjectMarker;
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
            }
        },
    }
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
        self.object_information = Value.ObjectMarker;
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
