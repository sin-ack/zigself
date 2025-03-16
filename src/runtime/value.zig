// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Actor = @import("./Actor.zig");
const debug = @import("../debug.zig");
const Object = @import("object.zig").Object;
const RefPtr = @import("../utility/ref_counted.zig").RefPtr;
const context = @import("context.zig");
const Selector = @import("Selector.zig");
const ByteArray = @import("./ByteArray.zig");
const BaseObject = @import("base_object.zig").BaseObject;
const heap_import = @import("./Heap.zig");
const LookupResult = object_lookup.LookupResult;
const object_lookup = @import("object_lookup.zig");
const VirtualMachine = @import("VirtualMachine.zig");

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

pub const Value = packed struct(u64) {
    type: Type,
    data: Data,

    pub const Data = u63;
    pub const SignedData = i63;
    pub const Type = enum(u1) {
        Integer = 0,
        Object = 1,
    };

    pub const TypeBits = 1;
    pub const TypeMask: u64 = 0b1;
    pub const DataBits = 63;

    // --- Creation ---

    /// Create a new Value object from the given object address.
    pub inline fn fromObjectAddress(address: [*]u64) Value {
        return @bitCast(Reference.createRegular(address));
    }

    /// Create a new Value object from an integer literal.
    pub inline fn fromInteger(integer: SignedData) Value {
        return .{ .data = @bitCast(integer), .type = .Integer };
    }

    /// Create a new Value object from an unsigned integer literal.
    pub inline fn fromUnsignedInteger(integer: Data) Value {
        return .{ .data = @intCast(integer), .type = .Integer };
    }

    // --- Casting ---

    /// Try to cast this value to an integer. Returns null if this value is not
    /// an integer.
    pub inline fn asInteger(self: Value) ?Value.SignedData {
        if (self.type == .Integer) return @bitCast(self.data);
        return null;
    }

    /// Try to cast this value to an unsigned integer. Returns null if this
    /// value is not an unsigned integer.
    pub inline fn asUnsignedInteger(self: Value) ?Value.Data {
        if (self.type == .Integer) return self.data;
        return null;
    }

    /// Try to cast this value to an object-like. Returns null if this value is
    /// not an object-like.
    pub inline fn asObjectLike(self: Value) ?ObjectLike {
        if (self.type == .Object) return @bitCast(self);
        return null;
    }

    /// Try to cast this value to an object reference. Returns null if this
    /// value is not an object reference.
    pub inline fn asReference(self: Value) ?Reference {
        if (self.asObjectLike()) |object_like| return object_like.asReference();
        return null;
    }

    /// Try to cast this value to a base object pointer. Returns null if this
    /// value is not an object reference.
    pub inline fn asBaseObject(self: Value) ?BaseObject.Ptr {
        if (self.asObjectLike()) |object_like| return object_like.asBaseObject();
        return null;
    }

    /// Try to cast this value to an object pointer. Returns null if this value
    /// is not an object reference.
    pub inline fn asObject(self: Value) ?Object.Ptr {
        if (self.asBaseObject()) |base_object| return base_object.asObject();
        return null;
    }

    /// Try to cast this value to a map pointer. Returns null if this value is
    /// not an object reference.
    pub inline fn asMap(self: Value) ?Map.Ptr {
        if (self.asBaseObject()) |base_object| return base_object.asMap();
        return null;
    }

    /// Try to cast this value to a byte array pointer. Returns null if this
    /// value is not an object reference.
    pub inline fn asByteArray(self: Value) ?ByteArray {
        if (self.asObjectLike()) |object_like| return object_like.asByteArray();
        return null;
    }

    /// Perform a lookup on this value by a selector.
    pub fn lookup(
        self: Value,
        selector: Selector,
    ) LookupResult {
        if (LOOKUP_DEBUG) std.debug.print("Value.lookup: Looking up {} on {}\n", .{ selector, self });
        if (selector.equals(Selector.well_known.self)) {
            return .{ .Regular = self };
        }

        const vm = context.getVM();
        return switch (self.type) {
            .Object => selector.lookupObject(self.asObject().?),
            .Integer => {
                if (LOOKUP_DEBUG) std.debug.print("Value.lookup: Looking up on traits integer\n", .{});
                const integer_traits = vm.integer_traits.get();
                if (selector.equals(Selector.well_known.parent))
                    return LookupResult{ .Regular = integer_traits };

                return integer_traits.lookup(selector);
            },
        };
    }

    /// Clone this value on the heap and return a reference to the new copy.
    pub fn clone(self: Value, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) Value {
        return switch (self.type) {
            .Integer => self,
            // NOTE: The only error condition that can happen here is during method and block map cloning.
            //       Since user code is unable to do this, there is no reason to propagate a try here.
            .Object => (self.asObject().?.clone(allocator, heap, token, actor_id) catch unreachable).asValue(),
        };
    }
};

/// ObjectLike is a value that's not an integer. This can be an actual object
/// header, or an object reference.
pub const ObjectLike = packed struct(u64) {
    value_type: Value.Type = .Object,
    type: Type,
    data: Data,

    pub const Data = u62;
    pub const Type = enum(u1) {
        Reference = 0,
        Object = 1,
    };

    pub inline fn asReference(self: ObjectLike) ?Reference {
        if (self.type == .Reference) return @bitCast(self);
        return null;
    }

    pub inline fn asBaseObject(self: ObjectLike) ?BaseObject.Ptr {
        if (self.asReference()) |reference| return reference.asBaseObject();
        return null;
    }

    pub inline fn asByteArray(self: ObjectLike) ?ByteArray {
        if (self.asReference()) |reference| return ByteArray.fromAddress(reference.getAddress());
        return null;
    }

    // NOTE: Since we don't manipulate object headers here, we don't supply
    //       a method for it.
};

/// A reference to an object in the heap. This can be a regular or forwarding
/// reference.
pub const Reference = packed struct(u64) {
    value_type: Value.Type = .Object,
    object_like_type: ObjectLike.Type = .Reference,
    type: Type,
    data: Data,

    pub const Data = u61;
    pub const Type = enum(u1) {
        Regular = 0,
        Forwarding = 1,
    };

    const Mask: u64 = 0b111;

    fn create(comptime reference_type: Type, address: [*]u64) Reference {
        const address_as_int: u64 = @intFromPtr(address);

        // Must be 8-byte aligned in order to utilize all the bottom bits.
        std.debug.assert(address_as_int & 0b111 == 0);

        var new_reference: Reference = @bitCast(address_as_int);
        new_reference.value_type = .Object;
        new_reference.object_like_type = .Reference;
        new_reference.type = reference_type;

        return new_reference;
    }

    /// Create a new non-forwarding reference.
    pub inline fn createRegular(address: [*]u64) Reference {
        return create(.Regular, address);
    }

    /// Create a new forwarding reference.
    pub inline fn createForwarding(address: [*]u64) Reference {
        return create(.Forwarding, address);
    }

    pub inline fn isForwarding(self: Reference) bool {
        return self.type == .Forwarding;
    }

    /// Attempt to convert the given address on the heap to a forwarding
    /// reference, returning null if it doesn't look like one.
    pub inline fn tryFromForwarding(address: [*]u64) ?Reference {
        const reference: Reference = @bitCast(address[0]);
        if (reference.value_type != .Object) return null;
        if (reference.object_like_type != .Reference) return null;
        if (reference.type != .Forwarding) return null;

        return reference;
    }

    /// Attempt to convert the given heap reference to a forwarding
    /// reference, returning null if it doesn't look like one.
    pub inline fn tryFromForwarding2(reference: Reference) ?Reference {
        const target_reference: Reference = @bitCast(reference.getAddress()[0]);
        if (target_reference.value_type != .Object) return null;
        if (target_reference.object_like_type != .Reference) return null;
        if (target_reference.type != .Forwarding) return null;

        return target_reference;
    }

    /// Return this reference as a Value.
    pub inline fn asValue(self: Reference) Value {
        return @bitCast(self);
    }

    pub inline fn getAddress(self: Reference) [*]u64 {
        const raw_value: u64 = @bitCast(self);
        return @ptrFromInt(raw_value & ~Mask);
    }

    /// Return a pointer to the object this reference points to.
    pub inline fn asBaseObject(self: Reference) BaseObject.Ptr {
        return @ptrCast(self.getAddress());
    }

    pub inline fn asObject(self: Reference) ?Object.Ptr {
        return self.asBaseObject().asObject();
    }

    pub inline fn asMap(self: Reference) ?Map.Ptr {
        return self.asBaseObject().asMap();
    }
};

/// A value which stores a pointer to off-heap memory.
pub fn PointerValue(comptime T: type) type {
    return PointerValueAlignment(T, null);
}

pub fn PointerValueAlignment(comptime T: type, comptime alignment: ?u29) type {
    const PointerT = if (alignment) |a| *align(a) T else *T;

    return extern struct {
        value: Value align(@alignOf(u64)),

        const Self = @This();

        pub fn init(value: PointerT) Self {
            return .{ .value = Value.fromUnsignedInteger(@intCast(@intFromPtr(value))) };
        }

        pub fn get(self: Self) PointerT {
            const self_int: usize = @intCast(self.value.asUnsignedInteger().?);
            return @ptrFromInt(self_int);
        }
    };
}

/// A value which stores a pointer to off-heap ref-counted memory.
pub fn RefCountedValue(comptime T: type) type {
    const RefT = RefPtr(T);
    const PointerT = PointerValue(T);

    return extern struct {
        value: PointerT align(@alignOf(u64)),

        const Self = @This();

        pub fn init(ref_counted_value: RefT) Self {
            ref_counted_value.ref();
            return .{ .value = PointerT.init(ref_counted_value.value) };
        }

        /// Use if deinit requires no arguments.
        pub fn deinit(self: Self) void {
            self.get().unref();
        }

        /// Use if deinit requires the allocator.
        pub fn deinitWithAllocator(self: Self, allocator: Allocator) void {
            self.get().unrefWithAllocator(allocator);
        }

        pub fn get(self: Self) RefT {
            return RefT{ .value = self.value.get() };
        }
    };
}

pub const IntegerValueSignedness = enum { Signed, Unsigned };
/// A value which is known to be an integer.
pub fn IntegerValue(comptime signedness: IntegerValueSignedness) type {
    const IntegerT = switch (signedness) {
        .Signed => Value.SignedData,
        .Unsigned => Value.Data,
    };
    const init_function = switch (signedness) {
        .Signed => Value.fromInteger,
        .Unsigned => Value.fromUnsignedInteger,
    };
    const conversion_function = switch (signedness) {
        .Signed => Value.asInteger,
        .Unsigned => Value.asUnsignedInteger,
    };

    return extern struct {
        value: Value align(@alignOf(u64)),

        const Self = @This();

        pub fn init(value: IntegerT) Self {
            return .{ .value = init_function(value) };
        }

        pub fn get(self: Self) IntegerT {
            if (builtin.mode == .Debug) {
                if (self.value.type != .Integer) {
                    @panic("!!! IntegerValue does not contain an integer!");
                }
            }

            return conversion_function(self.value).?;
        }
    };
}

/// A value which is of a known object type. Attempting to get the value as an
/// object when something else is stored is a runtime panic in debug, and
/// undefined behavior in release mode.
pub fn ObjectValue(comptime ObjectT: type) type {
    return extern struct {
        value: Value align(@alignOf(u64)),

        const Self = @This();

        pub fn init(ptr: ObjectT.Ptr) Self {
            return .{ .value = ptr.asValue() };
        }

        pub fn get(self: Self) ObjectT.Ptr {
            return self.value.asObject().?.asType(ObjectT.Type).?;
        }
    };
}
