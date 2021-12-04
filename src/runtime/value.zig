// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Object = @import("./object.zig");
const ByteVector = @import("./byte_vector.zig");

pub const Value = packed struct {
    data: u64,

    pub const ValueMarkerMask: u64 = 0b11;

    // Marks an integer. The number is shifted left by two bits for storage, and
    // shifted right by two bits for getting the value.
    pub const IntegerMarker: u64 = 0b00;
    // Marks an object reference.
    pub const ObjectReferenceMarker: u64 = 0b01;
    // Marks a floating point number. The lower bits are zero-ed before
    // returning.
    pub const FloatingPointMarker: u64 = 0b10;
    // Marks the beginning of an object in the heap.
    pub const ObjectMarker: u64 = 0b11;

    /// Create a new Value object from the given object address.
    pub fn fromObjectAddress(address: [*]u64) Value {
        // Must be 8-byte aligned
        std.debug.assert((@ptrToInt(address) & 0b111) == 0);
        return .{ .data = @ptrToInt(address) | ObjectReferenceMarker };
    }

    /// Create a new Value object from an integer literal.
    pub fn fromInteger(integer: i64) Value {
        std.debug.assert((@as(i64, -1) << 62) < integer and integer < (@as(i64, 1) << 62));
        return .{ .data = @bitCast(u64, integer << 2) | IntegerMarker };
    }

    /// Create a new Value object from an unsigned integer literal.
    pub fn fromUnsignedInteger(integer: u64) Value {
        std.debug.assert(integer < (@as(u64, 1) << 62));
        return .{ .data = (integer << 2) | IntegerMarker };
    }

    pub fn fromFloatingPoint(floating_point: f64) Value {
        return .{ .data = (@bitCast(u64, floating_point) & ~ValueMarkerMask) | FloatingPointMarker };
    }

    /// Return whether this value is an integer.
    pub fn isInteger(self: Value) bool {
        return (self.data & ValueMarkerMask) == IntegerMarker;
    }

    /// Return whether this value is an object reference.
    pub fn isObjectReference(self: Value) bool {
        return (self.data & ValueMarkerMask) == ObjectReferenceMarker;
    }

    /// Return whether this value is a floating point number.
    pub fn isFloatingPoint(self: Value) bool {
        return (self.data & ValueMarkerMask) == FloatingPointMarker;
    }

    /// Return this value as an integer.
    pub fn asInteger(self: Value) i64 {
        std.debug.assert(self.isInteger());
        return @bitCast(i64, self.data) >> 2;
    }

    /// Return this value as an unsigned integer.
    pub fn asUnsignedInteger(self: Value) u64 {
        std.debug.assert(self.isInteger());
        return self.data >> 2;
    }

    /// Return this value as a floating point number.
    pub fn asFloatingPoint(self: Value) f64 {
        std.debug.assert(self.isFloatingPoint());
        return @bitCast(f64, self.data & ~ValueMarkerMask);
    }

    /// Return the object address stored in this object as a pointer.
    pub fn asObjectAddress(self: Value) [*]u64 {
        std.debug.assert(self.isObjectReference());
        return @intToPtr([*]u64, self.data & ~ValueMarkerMask);
    }

    /// Return the object the address of which is stored in this value.
    pub fn asObject(self: Value) Object {
        std.debug.assert(self.isObjectReference());
        return Object.fromAddress(self.asObjectAddress());
    }

    /// Return the byte vector the address of which is stored in this value.
    pub fn asByteVector(self: Value) ByteVector {
        std.debug.assert(self.isObjectReference());
        return ByteVector.fromAddress(self.asObjectAddress());
    }
};
