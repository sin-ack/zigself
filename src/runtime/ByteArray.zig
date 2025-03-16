// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./Heap.zig");
const value = @import("./value.zig");
const Value = value.Value;
const IntegerValue = value.IntegerValue;
const pointer = @import("../utility/pointer.zig");

const ByteArray = @This();

header: Header.Ptr,

pub fn createFromString(allocator: Allocator, string: []const u8) !ByteArray {
    var self = try createUninitialized(allocator, string.len);
    @memcpy(self.getValues(), string);
    return self;
}

pub fn createUninitialized(allocator: Allocator, size: usize) !ByteArray {
    const memory_area = try allocator.alignedAlloc(u8, .of(u64), requiredSizeForAllocation(size));
    var header: Header.Ptr = @ptrCast(memory_area);

    header.init(size);

    return ByteArray{ .header = header };
}

pub fn deinit(self: ByteArray, allocator: Allocator) void {
    const memory: [*]align(@alignOf(u64)) u8 = @ptrCast(self.header);
    allocator.free(memory[0..self.header.length.get()]);
}

pub inline fn fromAddress(address: [*]u64) ByteArray {
    return .{ .header = @ptrCast(address) };
}

pub fn getValues(self: ByteArray) []u8 {
    const header_size = @sizeOf(Header);
    const total_length: usize = @intCast(self.header.length.get());

    const header: [*]u8 = @ptrCast(self.header);
    return header[header_size..total_length];
}

pub fn getLength(self: ByteArray) usize {
    return @intCast(self.header.length.get() - @sizeOf(Header));
}

/// Return the size required for the byte vector with `length` amount of
/// bytes to be stored inside.
fn requiredSizeForAllocation(length: usize) usize {
    return @sizeOf(Header) + length;
}

pub const Header = extern struct {
    /// The length of the bytevector object, including the size of the
    /// header.
    length: IntegerValue(.Unsigned) align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Header, .Mutable);

    pub fn init(self: Header.Ptr, byte_array_length: u64) void {
        self.length = IntegerValue(.Unsigned).init(@intCast(@sizeOf(Header) + byte_array_length));
    }

    pub fn asByteArray(self: Header.Ptr) ByteArray {
        return ByteArray{ .header = self };
    }
};
