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
const stage2_compat = @import("../utility/stage2_compat.zig");

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

pub fn clone(self: Self, token: *Heap.AllocationToken, actor_id: u31) !Self {
    return switch (self.header.getObjectType()) {
        .ForwardingReference, .Activation => unreachable,
        .Slots => fromAddress(self.asSlotsObject().clone(token, actor_id).asObjectAddress()),
        .Block => fromAddress(self.asBlockObject().clone(token, actor_id).asObjectAddress()),
        .ByteArray => fromAddress(self.asByteArrayObject().clone(token, actor_id).asObjectAddress()),
        .Array => fromAddress(self.asArrayObject().clone(token, actor_id).asObjectAddress()),
        .Method => fromAddress(self.asMethodObject().clone(token, actor_id).asObjectAddress()),
        .Map => fromAddress((try self.asMap().clone(token)).asObjectAddress()),
        // Managed values are not clonable. Instead we return the same (immutable) reference.
        .Managed => self,
        .Actor => @panic("TODO Handle cloning of actor object"),
        .ActorProxy => fromAddress(self.asActorProxyObject().clone(token, actor_id).asObjectAddress()),
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

/// Return this object as the given object type, or return null if the object is
/// not of the requested type.
pub inline fn asType(self: Self, comptime object_type: ObjectType) ?ObjectT(object_type).Ptr {
    if (self.header.getObjectType() != object_type) return null;
    return @ptrCast(ObjectT(object_type).Ptr, self.header);
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

pub fn asSlotsObject(self: Self) Slots.Ptr {
    if (builtin.mode == .Debug) self.mustBeSlotsObject();
    return @ptrCast(Slots.Ptr, self.header);
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

pub fn asActivationObject(self: Self) Activation.Ptr {
    if (builtin.mode == .Debug) self.mustBeActivationObject();
    return @ptrCast(Activation.Ptr, self.header);
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

pub fn asMethodObject(self: Self) Method.Ptr {
    if (builtin.mode == .Debug) self.mustBeMethodObject();
    return @ptrCast(Method.Ptr, self.header);
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

pub fn asBlockObject(self: Self) Block.Ptr {
    if (builtin.mode == .Debug) self.mustBeBlockObject();
    return @ptrCast(Block.Ptr, self.header);
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

pub fn asByteArrayObject(self: Self) ByteArray.Ptr {
    if (builtin.mode == .Debug) self.mustBeByteArrayObject();
    return @ptrCast(ByteArray.Ptr, self.header);
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

pub fn asArrayObject(self: Self) Array.Ptr {
    if (builtin.mode == .Debug) self.mustBeArrayObject();
    return @ptrCast(Array.Ptr, self.header);
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

pub fn asManaged(self: Self) Managed.Ptr {
    if (builtin.mode == .Debug) self.mustBeManaged();
    return @ptrCast(Managed.Ptr, self.header);
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

pub fn asMap(self: Self) Map.Ptr {
    if (builtin.mode == .Debug) self.mustBeMap();
    return @ptrCast(Map.Ptr, self.header);
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

pub fn asActorObject(self: Self) Actor.Ptr {
    if (builtin.mode == .Debug) self.mustBeActorObject();
    return @ptrCast(Actor.Ptr, self.header);
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

pub fn asActorProxyObject(self: Self) ActorProxy.Ptr {
    if (builtin.mode == .Debug) self.mustBeActorProxyObject();
    return @ptrCast(ActorProxy.Ptr, self.header);
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

pub const Header = extern struct {
    // FIXME: Turn this into a packed struct once Zig's packed structs start
    //        making sense.
    object_information: u64,
    map_pointer: Value,

    pub const Ptr = stage2_compat.HeapPtr(Header, .Mutable);

    const actor_id_shift = 32;
    const actor_id_bits = 31;
    const actor_id_mask: u64 = ((1 << actor_id_bits) - 1) << actor_id_shift;

    const globally_reachable_shift = 63;
    const globally_reachable_bits = 1;
    const globally_reachable_mask: u64 = ((1 << globally_reachable_bits) - 1) << globally_reachable_shift;

    pub fn init(self: Header.Ptr, object_type: ObjectType, actor_id: u31, map: Value) void {
        self.object_information = @enumToInt(Value.ValueType.ObjectMarker);
        self.setObjectType(object_type);
        self.setActorID(actor_id);

        self.map_pointer = map;
    }

    pub fn getObjectType(self: Header.Ptr) ObjectType {
        // FIXME: Check whether the current object type is a valid one, and
        //        don't let Zig crash us here if it's not.
        return @intToEnum(ObjectType, self.object_information & ObjectTypeMask);
    }

    pub fn setObjectType(self: Header.Ptr, object_type: ObjectType) void {
        self.object_information = (self.object_information & ~ObjectTypeMask) | @enumToInt(object_type);
    }

    pub fn getActorID(self: Header.Ptr) u31 {
        return @intCast(u31, (self.object_information & actor_id_mask) >> actor_id_shift);
    }

    pub fn setActorID(self: Header.Ptr, actor_id: u31) void {
        self.object_information = (self.object_information & ~actor_id_mask) | (@as(u64, actor_id) << actor_id_shift);
    }

    pub fn isGloballyReachable(self: Header.Ptr) bool {
        return (self.object_information & globally_reachable_mask) != 0;
    }

    pub fn setGloballyReachable(self: Header.Ptr, reachable: bool) void {
        self.object_information = (self.object_information & ~globally_reachable_mask) | (@as(u64, @boolToInt(reachable)) << globally_reachable_shift);
    }

    pub fn getMap(self: Header.Ptr) Map.Ptr {
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
