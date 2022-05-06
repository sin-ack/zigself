// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

/// A typed stack object.
pub fn Stack(comptime T: type) type {
    return struct {
        stack: std.ArrayListUnmanaged(T) = .{},

        const Self = @This();

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.stack.deinit(allocator);
        }

        /// Push a new item on the stack.
        pub fn push(self: *Self, allocator: Allocator, value: T) !void {
            try self.stack.append(allocator, value);
        }

        /// Return a slice of the last N items on the stack.
        pub fn lastNItems(self: Self, n: u32) []const T {
            const item_count = self.stack.items.len;
            return self.stack.items[item_count - n .. item_count];
        }

        /// Pop the last N items off the stack.
        pub fn popNItems(self: *Self, n: u32) void {
            self.restoreTo(self.height() - n);
        }

        /// Return all the items that are currently on the stack.
        pub fn allItems(self: Self) []T {
            return self.stack.items;
        }

        /// Return the current height of the stack.
        pub fn height(self: Self) usize {
            return self.allItems().len;
        }

        /// Restore the stack to the given height h, popping all items after it.
        pub fn restoreTo(self: *Self, h: usize) void {
            self.stack.shrinkRetainingCapacity(h);
        }
    };
}
