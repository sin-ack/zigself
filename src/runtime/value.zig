// Copyright (c) 2021-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const hash = @import("../utility/hash.zig");
const Heap = @import("./Heap.zig");
const Actor = @import("./Actor.zig");
const debug = @import("../debug.zig");
const Object = @import("object.zig").Object;
const RefPtr = @import("../utility/ref_counted.zig").RefPtr;
const context = @import("context.zig");
const ByteArray = @import("./ByteArray.zig");
const BaseObject = @import("base_object.zig").BaseObject;
const LookupResult = object_lookup.LookupResult;
const SelectorHash = object_lookup.SelectorHash;
const object_lookup = @import("object_lookup.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const InterpreterContext = @import("./interpreter.zig").InterpreterContext;

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

pub const Value = packed struct {
    data: u64,

    pub const ValueMarkerMask: u64 = 0b11;

    pub const ValueType = enum(u2) {
        Integer = 0b00,
        ObjectReference = 0b01,
        FloatingPoint = 0b10,
        ObjectMarker = 0b11,
    };

    /// Create a new Value object from the given object address.
    pub inline fn fromObjectAddress(address: [*]u64) Value {
        // Must be 8-byte aligned
        std.debug.assert((@intFromPtr(address) & 0b111) == 0);
        return .{ .data = @intFromPtr(address) | @intFromEnum(ValueType.ObjectReference) };
    }

    /// Create a new Value object from an integer literal.
    pub inline fn fromInteger(integer: i64) Value {
        std.debug.assert((@as(i64, -1) << 62) < integer and integer < (@as(i64, 1) << 62));
        const data: u64 = @bitCast(integer << 2);
        return .{ .data = data | @intFromEnum(ValueType.Integer) };
    }

    /// Create a new Value object from an unsigned integer literal.
    pub inline fn fromUnsignedInteger(integer: u64) Value {
        std.debug.assert(integer < (@as(u64, 1) << 62));
        return .{ .data = (integer << 2) | @intFromEnum(ValueType.Integer) };
    }

    pub inline fn fromFloatingPoint(floating_point: f64) Value {
        const floating_point_as_int: u64 = @bitCast(floating_point);
        return .{ .data = (floating_point_as_int & ~ValueMarkerMask) | @intFromEnum(ValueType.FloatingPoint) };
    }

    /// Return the type of this value.
    pub inline fn getType(self: Value) ValueType {
        return @enumFromInt(self.data & ValueMarkerMask);
    }

    /// Return whether this value is an integer.
    pub inline fn isInteger(self: Value) bool {
        return self.getType() == .Integer;
    }

    /// Return whether this value is an object reference.
    pub inline fn isObjectReference(self: Value) bool {
        return self.getType() == .ObjectReference;
    }

    /// Return whether this value is a floating point number.
    pub inline fn isFloatingPoint(self: Value) bool {
        return self.getType() == .FloatingPoint;
    }

    /// Return this value as an integer.
    pub inline fn asInteger(self: Value) i64 {
        std.debug.assert(self.isInteger());
        const data: i64 = @bitCast(self.data);
        return data >> 2;
    }

    /// Return this value as an unsigned integer.
    pub inline fn asUnsignedInteger(self: Value) u64 {
        std.debug.assert(self.isInteger());
        return self.data >> 2;
    }

    /// Return this value as a floating point number.
    pub inline fn asFloatingPoint(self: Value) f64 {
        std.debug.assert(self.isFloatingPoint());
        return @bitCast(self.data & ~ValueMarkerMask);
    }

    /// Return the object address stored in this value as a pointer.
    pub inline fn asObjectAddress(self: Value) [*]u64 {
        std.debug.assert(self.isObjectReference());
        const address: usize = @intCast(self.data & ~ValueMarkerMask);
        return @ptrFromInt(address);
    }

    /// Return the base object the address of which is stored in this value.
    pub inline fn asBaseObject(self: Value) BaseObject.Ptr {
        return BaseObject.fromAddress(self.asObjectAddress());
    }

    /// Return the object the address of which is stored in this value.
    pub inline fn asObject(self: Value) Object.Ptr {
        return self.asBaseObject().asObject().?;
    }

    /// Return the map the address of which is stored in this value.
    pub inline fn asMap(self: Value) Map.Ptr {
        return self.asBaseObject().asMap().?;
    }

    /// Return the byte vector the address of which is stored in this value.
    pub inline fn asByteArray(self: Value) ByteArray {
        return ByteArray.fromAddress(self.asObjectAddress());
    }

    pub fn lookup(
        self: Value,
        selector: []const u8,
    ) LookupResult {
        const selector_hash = SelectorHash.init(selector);
        if (LOOKUP_DEBUG) std.debug.print("Value.lookup: Looking up \"{s}\" (hash: {x}) on {}\n", .{ selector, selector_hash.regular, self });

        return self.lookupByHash(selector_hash);
    }

    pub fn lookupByHash(
        self: Value,
        selector_hash: SelectorHash,
    ) LookupResult {
        if (selector_hash.regular == object_lookup.self_hash) {
            return .{ .Regular = self };
        }

        const vm = context.getVM();
        return switch (self.getType()) {
            .ObjectMarker => unreachable,
            .ObjectReference => selector_hash.lookupObject(self.asObject()),

            .Integer => {
                if (LOOKUP_DEBUG) std.debug.print("Value.lookupByHash: Looking up on traits integer\n", .{});
                const integer_traits = vm.integer_traits.getValue();
                if (selector_hash.regular == object_lookup.parent_hash)
                    return LookupResult{ .Regular = integer_traits };

                return integer_traits.lookupByHash(selector_hash);
            },
            .FloatingPoint => {
                if (LOOKUP_DEBUG) std.debug.print("Value.lookupByHash: Looking up on traits float\n", .{});
                const float_traits = vm.float_traits.getValue();
                if (selector_hash.regular == object_lookup.parent_hash)
                    return LookupResult{ .Regular = float_traits };

                return float_traits.lookupByHash(selector_hash);
            },
        };
    }

    /// Clones this value and returns a copy of it.
    pub fn clone(self: Value, token: *Heap.AllocationToken, actor_id: Actor.ActorID) Value {
        return switch (self.getType()) {
            .ObjectMarker => unreachable,
            .Integer, .FloatingPoint => Value{ .data = self.data },
            // NOTE: The only error condition that can happen here is during method and block map cloning.
            //       Since user code is unable to do this, there is no reason to propagate a try here.
            .ObjectReference => (self.asObject().clone(token, actor_id) catch unreachable).asValue(),
        };
    }
};

/// A value which stores a pointer to off-heap memory.
pub fn PointerValue(comptime T: type) type {
    return PointerValueAlignment(T, null);
}

pub fn PointerValueAlignment(comptime T: type, comptime alignment: ?u29) type {
    const PointerT = if (alignment) |a| *align(a) T else *T;

    return packed struct {
        value: Value,

        const Self = @This();

        pub fn init(value: PointerT) Self {
            return .{ .value = Value.fromUnsignedInteger(@intFromPtr(value)) };
        }

        pub fn get(self: Self) PointerT {
            const self_int: usize = @intCast(self.value.asUnsignedInteger());
            return @ptrFromInt(self_int);
        }
    };
}

/// A value which stores a pointer to off-heap ref-counted memory.
pub fn RefCountedValue(comptime T: type) type {
    const RefT = RefPtr(T);
    const PointerT = PointerValue(T);

    return packed struct {
        value: PointerT,

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
        .Signed => i64,
        .Unsigned => u64,
    };
    const init_function = switch (signedness) {
        .Signed => Value.fromInteger,
        .Unsigned => Value.fromUnsignedInteger,
    };
    const conversion_function = switch (signedness) {
        .Signed => Value.asInteger,
        .Unsigned => Value.asUnsignedInteger,
    };

    return packed struct {
        value: Value,

        const Self = @This();

        pub fn init(value: IntegerT) Self {
            return .{ .value = init_function(value) };
        }

        pub fn get(self: Self) IntegerT {
            if (builtin.mode == .Debug) {
                if (!self.value.isInteger()) {
                    @panic("!!! IntegerValue does not contain an integer!");
                }
            }

            return conversion_function(self.value);
        }
    };
}

/// A value which is of a known object type. Attempting to get the value as an
/// object when something else is stored is a runtime panic in debug, and
/// undefined behavior in release mode.
pub fn ObjectValue(comptime ObjectT: type) type {
    return packed struct {
        value: Value,

        const Self = @This();

        pub fn init(ptr: ObjectT.Ptr) Self {
            return .{ .value = ptr.asValue() };
        }

        pub fn get(self: Self) ObjectT.Ptr {
            return self.value.asObject().asType(ObjectT.Type).?;
        }
    };
}
