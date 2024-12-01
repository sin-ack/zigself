// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const Heap = @import("./Heap.zig");
const value = @import("./value.zig");
const Value = value.Value;
const IntegerValue = value.IntegerValue;
const pointer = @import("../utility/pointer.zig");

const ByteArray = @This();

header: Header.Ptr,

pub fn createFromString(token: *Heap.AllocationToken, string: []const u8) ByteArray {
    var self = createUninitialized(token, string.len);
    @memcpy(self.getValues(), string);
    return self;
}

pub fn createUninitialized(token: *Heap.AllocationToken, size: usize) ByteArray {
    const memory_area = token.allocate(.ByteArray, requiredSizeForAllocation(size));
    var header: Header.Ptr = @ptrCast(memory_area);

    header.init(size);

    return ByteArray{ .header = header };
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

pub fn asValue(self: ByteArray) Value {
    return Value.fromObjectAddress(@ptrCast(@alignCast(self.header)));
}

pub fn getSizeInMemory(self: ByteArray) usize {
    return requiredSizeForAllocation(self.getLength());
}

/// Return the size required for the byte vector with `length` amount of
/// bytes to be stored inside.
pub fn requiredSizeForAllocation(length: usize) usize {
    // A basic ceil
    const required_words = if (length == 0) 0 else ((length - 1) / @sizeOf(u64)) + 1;

    return @sizeOf(Header) + required_words * @sizeOf(u64);
}

pub const Header = extern struct {
    /// The length of the bytevector object, including the size of the
    /// header.
    length: IntegerValue(.Unsigned) align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Header, .Mutable);

    pub fn init(self: Header.Ptr, byte_array_length: u64) void {
        self.length = IntegerValue(.Unsigned).init(@intCast(@sizeOf(Header) + byte_array_length));
    }

    pub fn asByteVector(self: Header.Ptr) ByteArray {
        return .{ .header = @alignCast(self) };
    }
};
