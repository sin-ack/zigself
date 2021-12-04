// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Value = @import("./value.zig").Value;
const Heap = @import("./heap.zig");

const Self = @This();

header: *align(@alignOf(u64)) Header,

pub fn createFromString(heap: *Heap, string: []const u8) !Self {
    var memory_area = try heap.allocateInByteVectorSegment(requiredSize(string.len));
    var header = @ptrCast(*Header, memory_area);

    header.init(string.len);

    var self = Self{ .header = header };
    std.mem.copy(u8, self.getValues(), string);

    return self;
}

pub fn fromAddress(address: [*]u64) Self {
    return .{ .header = @ptrCast(*align(@alignOf(u64)) Header, address) };
}

pub fn getValues(self: Self) []u8 {
    const header_size = @sizeOf(Header);
    const total_length = self.header.length.asUnsignedInteger();

    return @ptrCast([*]u8, self.header)[header_size..total_length];
}

pub fn asValue(self: Self) Value {
    return Value.fromObjectAddress(@ptrCast([*]u64, self.header));
}

pub fn getSizeInMemory(self: Self) usize {
    return requiredSize(self.header.length.asUnsignedInteger() - @sizeOf(Header));
}

/// Return the size required for the byte vector with `length` amount of
/// bytes to be stored inside.
fn requiredSize(length: u64) usize {
    // A basic ceil
    const required_words = if (length == 0) 0 else ((length - 1) / @sizeOf(u64)) + 1;

    return @sizeOf(Header) + required_words * @sizeOf(u64);
}

pub const Header = packed struct {
    /// The length of the bytevector object, including the size of the
    /// header.
    length: Value,

    pub fn init(self: *Header, byte_vector_length: u64) void {
        self.length = Value.fromUnsignedInteger(@sizeOf(Header) + byte_vector_length);
    }

    pub fn asByteVector(self: *Header) Self {
        return .{ .header = @alignCast(@alignOf(u64), self) };
    }
};
