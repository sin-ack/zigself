// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Actor = @import("Actor.zig");
const debug = @import("../debug.zig");
const Value = @import("value.zig").Value;
const pointer = @import("../utility/pointer.zig");
const Selector = @import("Selector.zig");
const Reference = @import("value.zig").Reference;
const BaseObject = @import("base_object.zig").BaseObject;
const ObjectLike = @import("value.zig").ObjectLike;
const heap_import = @import("Heap.zig");
const scoped_bits = @import("../utility/scoped_bits.zig");
const object_lookup = @import("object_lookup.zig");
const VirtualMachine = @import("VirtualMachine.zig");

const STRICT_CAST_CHECKING = debug.STRICT_CAST_CHECKING;

// TODO: Unify ObjectType and ObjectRegistry once Zig stops treating it as a dependency loop.
pub const ObjectType = enum(u5) {
    Slots,
    Method,
    Block,
    Activation,
    Array,
    ByteArray,
    Managed,
    Actor,
    ActorProxy,
    Float,
    // Intrinsic objects
    AddrInfo,
};

/// A registry of object types. Objects within this array will be encoded into
/// the object information word by their index.
pub const ObjectRegistry = union(ObjectType) {
    Slots: @import("objects/slots.zig").Slots,
    Method: @import("objects/method.zig").Method,
    Block: @import("objects/block.zig").Block,
    Activation: @import("objects/activation.zig").Activation,
    Array: @import("objects/array.zig").Array,
    ByteArray: @import("objects/byte_array.zig").ByteArray,
    Managed: @import("objects/managed.zig").Managed,
    Actor: @import("objects/actor.zig").Actor,
    ActorProxy: @import("objects/actor_proxy.zig").ActorProxy,
    Float: @import("objects/float.zig").Float,
    // Intrinsic objects
    AddrInfo: @import("objects/intrinsic/addrinfo.zig").AddrInfo,
};

pub fn ObjectT(comptime object_type: ObjectType) type {
    const object_type_name = @tagName(object_type);
    inline for (@typeInfo(ObjectRegistry).@"union".fields) |field| {
        if (std.mem.eql(u8, object_type_name, field.name))
            return field.type;
    }

    @compileError("Unknown object type " ++ @tagName(object_type));
}

// Comptime object checks
comptime {
    for (@typeInfo(ObjectRegistry).@"union".fields) |field| {
        // Ensure that all objects contain the BaseObject type as their first
        // word, either directly or via another parent object.
        var object_first_field = @typeInfo(field.type).@"struct".fields[0].type;
        while (@typeInfo(object_first_field) == .@"struct") {
            if (object_first_field == BaseObject)
                break;
            object_first_field = @typeInfo(object_first_field).@"struct".fields[0].type;
        } else {
            @compileError("!!! Object " ++ @typeName(field.type) ++ " does not contain an object header!");
        }
    }
}

/// Object is the root of zigSelf. The object struct provides convenience
/// methods for asking the specialized object type questions and safely
/// casting.
pub const Object = extern struct {
    base_object: BaseObject align(@alignOf(u64)),

    /// The BaseObject metadata is cast to this type in order to utilize the
    /// unused bits.
    pub const Metadata = packed struct(u64) {
        value_type: Value.Type = .Object,
        object_like_type: ObjectLike.Type = .Object,
        base_type: BaseObject.Type = .Object,
        type: ObjectType,

        /// Unused bits that can be utilized for extra metadata on specialized
        /// objects. Reserve bits from the closest ExtraBits in the inheritance
        /// chain to use it.
        extra: u24 = 0,

        /// The actor ID determines which actor this object belongs to. In zigSelf,
        /// each object is associated with an actor, and is unreachable outside that
        /// actor (with the exception of globally reachable objects which are
        /// readable but not writable).
        actor_id: Actor.ActorID,
        /// The reachability of the object.
        reachability: BaseObject.Reachability = .Local,
    };

    pub const Ptr = pointer.HeapPtr(Object, .Mutable);

    /// Reserve bits from this ScopedBits object in your downstream objects to
    /// use Metadata.extra.
    pub const ExtraBits = scoped_bits.ScopedBitsOffsetLimit(Metadata, 8, 32);

    pub fn getMetadata(self: Object.Ptr) *Metadata {
        return @ptrCast(&self.base_object.metadata);
    }

    /// Initialize the object metadata. Should be called by specialized objects
    /// only.
    pub fn init(self: Object.Ptr, object_type: ObjectType, actor_id: Actor.ActorID) void {
        self.getMetadata().* = .{
            .type = object_type,
            .actor_id = actor_id,
        };
    }

    /// Get an object pointer from its address.
    pub fn fromAddress(address: [*]u64) Object.Ptr {
        const object: Object.Ptr = @ptrCast(address);
        std.debug.assert(object.getMetadata().value_type == .Object);
        std.debug.assert(object.getMetadata().object_like_type == .Object);
        std.debug.assert(object.getMetadata().base_type == .Object);
        return object;
    }

    // --- Casting ---

    /// If the object is of the given type, return it as that type. Otherwise return null.
    pub fn asType(self: Object.Ptr, comptime object_type: ObjectType) ?ObjectT(object_type).Ptr {
        if (self.getMetadata().type == object_type) return self.unsafeAsType(object_type);
        return null;
    }

    /// Force-cast this object to the given type. This is unsafe and should only
    /// be used when it is absolutely certain that the object is of the given
    /// type. Most code should use `asType` instead.
    pub fn unsafeAsType(self: Object.Ptr, comptime object_type: ObjectType) ObjectT(object_type).Ptr {
        if (STRICT_CAST_CHECKING) std.debug.assert(self.getMetadata().type == object_type);
        return @ptrCast(self);
    }

    /// Return the address of this object.
    pub fn getAddress(self: Object.Ptr) [*]u64 {
        return @ptrCast(self);
    }

    /// Return this object as a Value.
    pub fn asValue(self: Object.Ptr) Value {
        return Value.fromObjectAddress(self.getAddress());
    }

    // --- Common operations ---

    /// Perform a dynamic dispatch based on the current object type.
    inline fn dispatch(self: Object.Ptr, comptime ReturnType: type, comptime name: []const u8, args: anytype) ReturnType {
        return switch (self.getMetadata().type) {
            inline else => |t| {
                if (!@hasDecl(ObjectT(t), name)) {
                    @panic("!!! " ++ @tagName(t) ++ " does not implement `" ++ name ++ "`!");
                }

                const self_ptr: ObjectT(t).Ptr = @ptrCast(self);
                return @call(.always_inline, @field(ObjectT(t), name), .{self_ptr} ++ args);
            },
        };
    }

    /// Get the size of this object (NOT its dependencies) in memory.
    pub fn getSizeInMemory(self: Object.Ptr) usize {
        return self.dispatch(usize, "getSizeInMemory", .{});
    }

    /// Get the size required for cloning this object (including all private
    /// dependencies it has).
    pub fn getSizeForCloning(self: Object.Ptr) usize {
        return self.dispatch(usize, "getSizeForCloning", .{});
    }

    /// Finalize this object. Skip if the object does not support finalization.
    pub fn finalize(self: Object.Ptr, allocator: Allocator) void {
        // FIXME: This forces the finalize method to be defined on objects that
        //        don't support finalization. Find a way that can avoid forcing
        //        the definition of the useless finalize method.
        self.dispatch(void, "finalize", .{allocator});
    }

    /// Visit all the edges of this object.
    pub fn visitEdges(self: Object.Ptr, visitor: anytype) !void {
        const Error = blk: {
            comptime var Visitor = @TypeOf(visitor);
            if (@typeInfo(Visitor) == .pointer) {
                Visitor = @typeInfo(Visitor).pointer.child;
            }

            const visit_info = @typeInfo(@TypeOf(Visitor.visit));
            break :blk @typeInfo(visit_info.@"fn".return_type.?).error_union.error_set;
        };

        return self.dispatch(Error!void, "visitEdges", .{visitor});
    }

    /// Create a new shallow copy of this object. The map is not affected.
    pub fn clone(self: Object.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) Allocator.Error!Ptr {
        // NOTE: Inlining the delegation here because we need to cast the result into the generic object type.
        return switch (self.getMetadata().type) {
            inline else => |t| {
                if (!@hasDecl(ObjectT(t), "clone")) unreachable;

                const self_ptr: ObjectT(t).Ptr = @ptrCast(self);
                const result_or_error = self_ptr.clone(allocator, heap, token, actor_id);
                const result = if (@typeInfo(@TypeOf(result_or_error)) == .error_union) try result_or_error else result_or_error;
                return @ptrCast(result);
            },
        };
    }

    /// Lookup a selector on this object.
    pub fn lookup(
        self: Object.Ptr,
        selector: Selector,
        previously_visited: ?*const Selector.VisitedValueLink,
    ) object_lookup.LookupResult {
        return self.dispatch(object_lookup.LookupResult, "lookup", .{ selector, previously_visited });
    }
};

/// An object with an associated map reference. Some object types (notably Slots
/// and thus anything slots-based) require maps in order to work.
pub const MapObject = extern struct {
    object: Object align(@alignOf(u64)),
    map: Value align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(MapObject, .Mutable);

    /// Initialize the fields of the MapObject base.
    pub fn init(self: MapObject.Ptr, object_type: ObjectType, actor_id: Actor.ActorID, map: Value) void {
        self.object.init(object_type, actor_id);
        self.map = map;
    }

    pub fn getMetadata(self: MapObject.Ptr) *Object.Metadata {
        return self.object.getMetadata();
    }

    /// Visit edges of this object using the given visitor. Users of `MapObject`
    /// must call this method in their own `visitEdges` implementation.
    pub fn visitEdges(self: MapObject.Ptr, visitor: anytype) !void {
        try visitor.visit(&self.map, @ptrCast(self));
    }

    pub fn getMap(self: MapObject.Ptr) Map.Ptr {
        const reference = self.map.unsafeAsReference();

        // XXX: If we're currently in the middle of scavenging, then our map
        //      probably has already been scavenged into the target space and
        //      a forward reference has been placed in its location instead.
        //      We should get the forwarded object.
        if (Reference.tryFromForwarding2(reference)) |forwarded_reference| {
            return forwarded_reference.unsafeAsMap();
        }

        return reference.unsafeAsMap();
    }
};
