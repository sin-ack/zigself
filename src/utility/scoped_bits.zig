// Copyright (c) 2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

/// ScopedBits lets the user define a "scoped" packed struct within a bitfield.
/// Objects in ZigSelf that "inherit" from others can avoid clobbering each
/// others' fields by using ScopedBits.
///
/// Example:
///
/// ```zig
/// // A base "slots object"
/// const Slots = extern struct {
///     // Object header
///     object: Object,
///
///     const bits = ScopedBits(u62).reserve(SlotsBits);
///
///     pub fn init(self: Slots.Ptr, ...) void {
///         Slots.bits.write(self.object.getMetadata(), .{ .mutable = ..., .globally_reachable = ... });
///     }
/// };
///
/// // Block "inherits" a slots object, so needs to initialize its bits while using some of its own
/// const Block = extern struct {
///     base: Slots,
///
///     // Each "reserve" call creates a new type and deducts however many bits the type has, tracking the offset and size
///     const bits = Slots.bits.reserve(BlockBits);
///
///     fn init(self: Block.Ptr, ...) void {
///         // Initialize the inner slots object
///         self.base.init(...);
///         // Write your own bits
///         Block.bits.write(self.base.object.getMetadata(), .{ ... });
///     }
///
///     // Read only the relevant bits
///     pub fn getBlockInfo(self: Block.Ptr) BlockBits {
///         return Block.bits.read(self.base.object.getMetadata().*);
///     }
/// };
/// ```
pub fn ScopedBits(comptime Source: type) type {
    return ScopedBitsOffset(Source, 0);
}

pub fn ScopedBitsOffset(comptime Source: type, comptime offset_bits: comptime_int) type {
    const SourceBackingInt = BackingIntType(Source, "source");
    return ScopedBitsOffsetLimit(Source, offset_bits, @typeInfo(SourceBackingInt).Int.bits);
}

pub fn ScopedBitsOffsetLimit(comptime Source: type, comptime offset_bits: comptime_int, comptime limit_bits: comptime_int) type {
    return ScopedBitsDetail(Source, offset_bits, limit_bits, u0);
}

fn BackingIntType(comptime T: type, comptime name: []const u8) type {
    const type_info = @typeInfo(T);
    const BackingInt = switch (type_info) {
        .Int => T,
        .Struct => |s| blk: {
            if (s.layout != .@"packed") {
                @compileError("ScopedBits requires the backing type of the " ++ name ++ " to be packed");
            }

            break :blk s.backing_integer.?;
        },
        .Enum => |e| e.tag_type,
        else => @compileError("ScopedBits only works with unsigned integers, packed structs, and enums"),
    };

    const backing_type_info = @typeInfo(BackingInt);

    if (backing_type_info != .Int) {
        @compileError("ScopedBits requires the backing type of the " ++ name ++ " to be an integer");
    }

    if (backing_type_info.Int.signedness == .signed) {
        @compileError("ScopedBits requires the backing type of the " ++ name ++ " to be an unsigned integer");
    }

    return BackingInt;
}

fn ScopedBitsDetail(comptime Source: type, comptime offset_bits: comptime_int, comptime limit_bits: comptime_int, comptime Target: type) type {
    const SourceBackingInt = BackingIntType(Source, "source");
    const TargetBackingInt = BackingIntType(Target, "target");

    const target_backing_type_info = @typeInfo(TargetBackingInt);

    const reserved_bits = target_backing_type_info.Int.bits;
    const remaining_bits = limit_bits - offset_bits - reserved_bits;

    return struct {
        const Self = @This();
        const Mask: SourceBackingInt = ((1 << reserved_bits) - 1) << offset_bits;

        pub fn reserve(comptime NewTarget: type) type {
            const NewBackingInt = BackingIntType(NewTarget, "target");

            const new_backing_type_info = @typeInfo(NewBackingInt);
            const new_bits = new_backing_type_info.Int.bits;

            if (new_bits > remaining_bits) {
                @compileError(std.fmt.comptimePrint("ScopedBits could not reserve {} bits for {} ({} bits remaining)", .{ new_bits, @typeName(NewTarget), remaining_bits }));
            }

            return ScopedBitsDetail(Source, offset_bits + reserved_bits, limit_bits, NewTarget);
        }

        fn castToBackingType(comptime T: type, comptime BackingT: type, value: T) BackingT {
            return switch (@typeInfo(T)) {
                .Int => @intCast(value),
                .Struct => @bitCast(value),
                .Enum => @intFromEnum(value),
                else => unreachable,
            };
        }

        fn castFromBackingType(comptime T: type, comptime BackingT: type, value: BackingT) T {
            return switch (@typeInfo(T)) {
                .Int => @intCast(value),
                .Struct => @bitCast(value),
                .Enum => @enumFromInt(value),
                else => unreachable,
            };
        }

        pub fn read(source: Source) Target {
            const value: TargetBackingInt = @intCast((castToBackingType(Source, SourceBackingInt, source) & Mask) >> offset_bits);
            return castFromBackingType(Target, TargetBackingInt, value);
        }

        pub fn write(source: *Source, new_value: Target) void {
            const value_as_integer: SourceBackingInt = castToBackingType(Target, TargetBackingInt, new_value);
            const source_as_integer = castToBackingType(Source, SourceBackingInt, source.*);

            source.* = castFromBackingType(Source, SourceBackingInt, (source_as_integer & ~Mask) | (value_as_integer << offset_bits));
        }
    };
}

test ScopedBits {
    // Create a ScopedBits
    const Bits = ScopedBits(u32).reserve(u8);

    var value: u32 = 0xAAAAAAAA;
    Bits.write(&value, 0x55);
    try std.testing.expectEqual(Bits.read(value), 0x55);
    try std.testing.expectEqual(value, 0xAAAAAA55);

    // Reserve more bits
    const Bits2 = Bits.reserve(u16);
    Bits2.write(&value, 0x3344);
    try std.testing.expectEqual(Bits.read(value), 0x55);
    try std.testing.expectEqual(Bits2.read(value), 0x3344);
    try std.testing.expectEqual(value, 0xAA334455);

    // Reserve a packed struct
    const Packed = packed struct(u3) { a: u1, b: u2 };

    value = 0xBBBBAABB;
    const Bits3 = Bits.reserve(Packed);
    Bits3.write(&value, .{ .a = 1, .b = 3 });
    try std.testing.expectEqual(Bits3.read(value), Packed{ .a = 1, .b = 3 });
    try std.testing.expectEqual(Bits.read(value), 0xBB);
    try std.testing.expectEqual(value, 0xBBBBAFBB);
}
