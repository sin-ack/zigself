// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./Heap.zig");
const Value = @import("./value.zig").Value;
const slots_objects = @import("./object/slots.zig");
const byte_array_object = @import("./object/byte_array.zig");
const array_object = @import("./object/array.zig");
const managed_object = @import("./object/managed.zig");
const map_objects = @import("./object/map.zig");
const actor_objects = @import("./object/actor.zig");

const Self = @This();

pub const Slots = slots_objects.Slots;
pub const Activation = slots_objects.Activation;
pub const Method = slots_objects.Method;
pub const Block = slots_objects.Block;
pub const ByteArray = byte_array_object.ByteArrayObject;
pub const Array = array_object.ArrayObject;
pub const Managed = managed_object.ManagedObject;
pub const Map = map_objects.Map;
pub const Actor = actor_objects.ActorObject;
pub const ActorProxy = actor_objects.ActorProxyObject;

pub usingnamespace @import("./object/lookup.zig");

pub const ObjectTypeShift = 2;
pub const ObjectTypeBits = 4;
const ObjectTypeMask: u64 = ((1 << ObjectTypeBits) - 1) << ObjectTypeShift;

/// The object types that are available in the system.
pub const ObjectType = enum(u64) {
    ForwardingReference = 0b0000 << ObjectTypeShift,
    Slots = 0b0001 << ObjectTypeShift,
    Activation = 0b0010 << ObjectTypeShift,
    Method = 0b0011 << ObjectTypeShift,
    Block = 0b0100 << ObjectTypeShift,
    ByteArray = 0b0101 << ObjectTypeShift,
    Array = 0b0110 << ObjectTypeShift,
    Managed = 0b0111 << ObjectTypeShift,
    Actor = 0b1000 << ObjectTypeShift,
    ActorProxy = 0b1001 << ObjectTypeShift,
    Map = 0b1111 << ObjectTypeShift,
};

header: *align(@alignOf(u64)) Header,

pub fn addressIsObject(address: [*]u64) bool {
    return address[0] & Value.ValueMarkerMask == @enumToInt(Value.ValueType.ObjectMarker);
}

pub fn fromAddress(address: [*]u64) Self {
    if (!addressIsObject(address)) {
        std.debug.panic("!!! Object.fromAddress got an address ({*}) that does NOT contain an object marker ({x})!", .{ address, address[0] });
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
        .ByteArray => self.asByteArrayObject().getSizeInMemory(),
        .Array => self.asArrayObject().getSizeInMemory(),
        .Managed => self.asManaged().getSizeInMemory(),
        .Map => self.asMap().getSizeInMemory(),
        .Actor => self.asActorObject().getSizeInMemory(),
        .ActorProxy => self.asActorProxyObject().getSizeInMemory(),
    };
}

pub fn shouldFinalize(self: Self) bool {
    return switch (self.header.getObjectType()) {
        .ForwardingReference, .Slots, .Activation, .Method, .Block, .Array, .ByteArray, .ActorProxy => false,
        .Managed, .Actor => true,
        .Map => self.asMap().shouldFinalize(),
    };
}

pub fn finalize(self: Self, allocator: Allocator) void {
    switch (self.header.getObjectType()) {
        .ForwardingReference, .Slots, .Activation, .Method, .Block, .Array, .ByteArray, .ActorProxy => unreachable,
        .Map => self.asMap().finalize(allocator),
        .Managed => self.asManaged().finalize(allocator),
        .Actor => self.asActorObject().finalize(allocator),
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
        .ByteArray => fromAddress((try self.asByteArrayObject().clone(heap)).asObjectAddress()),
        .Array => fromAddress((try self.asArrayObject().clone(heap)).asObjectAddress()),
        // Managed values are not clonable. Instead we return the same (immutable) reference.
        .Managed => self,
        .Actor => @panic("TODO Handle cloning of actor object"),
        .ActorProxy => fromAddress((try self.asActorProxyObject().clone(heap)).asObjectAddress()),
    };
}

// FIXME: This name really sucks. What would be a better name that doesn't clash
//        with ObjectType?
pub fn ObjectT(comptime object_type: ObjectType) type {
    return switch (object_type) {
        .ForwardingReference => unreachable,
        .Slots => Slots,
        .Activation => Activation,
        .Method => Method,
        .Block => Block,
        .ByteArray => ByteArray,
        .Array => Array,
        .Managed => Managed,
        .Actor => Actor,
        .ActorProxy => ActorProxy,
        .Map => Map,
    };
}

/// Return a human readable name for the given object type.
pub fn humanReadableNameFor(comptime object_type: ObjectType) []const u8 {
    return switch (object_type) {
        .ForwardingReference => unreachable,
        .Slots => "slots object",
        .Activation => "activation object",
        .Method => "method object",
        .Block => "block object",
        .ByteArray => "byte array object",
        .Array => "array object",
        .Managed => "managed object",
        .Actor => "actor object",
        .ActorProxy => "actor proxy object",
        .Map => "map",
    };
}

/// Return this object as the given object type, or return null if it cannot
/// be returned.
pub inline fn asType(self: Self, comptime object_type: ObjectType) ?*ObjectT(object_type) {
    if (builtin.mode == .Debug) {
        if (self.header.getObjectType() != object_type) return null;
    }

    return @ptrCast(*ObjectT(object_type), self.header);
}

// TODO: Deprecate and remove the functions below, and replace them with the
//       unified asType function across the codebase.

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
    if (builtin.mode == .Debug) self.mustBeSlotsObject();
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
    if (builtin.mode == .Debug) self.mustBeActivationObject();
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
    if (builtin.mode == .Debug) self.mustBeMethodObject();
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
    if (builtin.mode == .Debug) self.mustBeBlockObject();
    return @ptrCast(*Block, self.header);
}

// Byte array objects

pub fn isByteArrayObject(self: Self) bool {
    return self.header.getObjectType() == .ByteArray;
}

fn mustBeByteArrayObject(self: Self) void {
    if (!self.isByteArrayObject()) {
        std.debug.panic("Expected the object at {*} to be a byte array object", .{self.header});
    }
}

pub fn asByteArrayObject(self: Self) *ByteArray {
    if (builtin.mode == .Debug) self.mustBeByteArrayObject();
    return @ptrCast(*ByteArray, self.header);
}

// Array objects

pub fn isArrayObject(self: Self) bool {
    return self.header.getObjectType() == .Array;
}

fn mustBeArrayObject(self: Self) void {
    if (!self.isArrayObject()) {
        std.debug.panic("Expected the object at {*} to be a array object", .{self.header});
    }
}

pub fn asArrayObject(self: Self) *Array {
    if (builtin.mode == .Debug) self.mustBeArrayObject();
    return @ptrCast(*Array, self.header);
}

// Managed values

pub fn isManaged(self: Self) bool {
    return self.header.getObjectType() == .Managed;
}

fn mustBeManaged(self: Self) void {
    if (!self.isManaged()) {
        std.debug.panic("Expected the object at {*} to be a managed value", .{self.header});
    }
}

pub fn asManaged(self: Self) *Managed {
    if (builtin.mode == .Debug) self.mustBeManaged();
    return @ptrCast(*Managed, self.header);
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
    if (builtin.mode == .Debug) self.mustBeMap();
    return @ptrCast(*Map, self.header);
}

// Actor objects

pub fn isActorObject(self: Self) bool {
    return self.header.getObjectType() == .Actor;
}

fn mustBeActorObject(self: Self) void {
    if (!self.isActorObject()) {
        std.debug.panic("Expected the object at {*} to be an actor", .{self.header});
    }
}

pub fn asActorObject(self: Self) *Actor {
    if (builtin.mode == .Debug) self.mustBeActorObject();
    return @ptrCast(*Actor, self.header);
}

// Actor proxy objects

pub fn isActorProxyObject(self: Self) bool {
    return self.header.getObjectType() == .ActorProxy;
}

fn mustBeActorProxyObject(self: Self) void {
    if (!self.isActorProxyObject()) {
        std.debug.panic("Expected the object at {*} to be an actor proxy", .{self.header});
    }
}

pub fn asActorProxyObject(self: Self) *ActorProxy {
    if (builtin.mode == .Debug) self.mustBeActorProxyObject();
    return @ptrCast(*ActorProxy, self.header);
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
    if (builtin.mode == .Debug) self.mustBeForwardingReference();
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
        const object = self.map_pointer.asObject();

        // XXX: If we're currently in the middle of scavenging, then our map
        //      probably has already been scavenged into the target space and
        //      a forward reference has been placed in its location instead.
        //      Let's dereference that forward reference. I don't know whether
        //      this is the cleanest way or not, but let's do it for now anyway.
        return if (object.isForwardingReference()) {
            const forwarded_object = fromAddress(object.getForwardAddress());
            return forwarded_object.asMap();
        } else object.asMap();
    }
};
