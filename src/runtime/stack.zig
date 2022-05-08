// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../debug.zig");

const STACK_DEBUG = debug.STACK_DEBUG;

/// A typed stack object.
pub fn Stack(comptime T: type, comptime debug_name: [*:0]const u8, comptime sentinel: ?T) type {
    return struct {
        stack: std.ArrayListUnmanaged(T) = .{},

        const Self = @This();

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.stack.deinit(allocator);
        }

        /// Push a new item on the stack.
        pub fn push(self: *Self, allocator: Allocator, value: T) !void {
            try self.stack.append(allocator, value);
            if (STACK_DEBUG) std.debug.print(debug_name ++ ": Pushed, now have {} items\n", .{self.height()});
        }

        pub fn pop(self: *Self) T {
            const value = self.popImpl();
            if (STACK_DEBUG) std.debug.print(debug_name ++ ": Popped, now have {} items\n", .{self.height()});
            return value;
        }

        fn popImpl(self: *Self) T {
            const value = self.lastNItems(1)[0];
            self.popNItemsImpl(1);
            return value;
        }

        pub usingnamespace if (sentinel) |st| struct {
            /// Pushes a sentinel value on the stack which can later be checked with
            /// `verifySentinel`.
            pub fn pushSentinel(self: *Self, allocator: Allocator) !void {
                try self.stack.append(allocator, st);
                if (STACK_DEBUG) std.debug.print(debug_name ++ ": Pushed (sentinel), now have {} items\n", .{self.height()});
            }

            pub fn verifySentinel(self: *Self) void {
                const popped_value = self.popImpl();
                if (!std.meta.eql(popped_value, st))
                    std.debug.panic("!!! Failed to verify sentinel! Expected {}, got {}", .{ popped_value, st });
                if (STACK_DEBUG) std.debug.print(debug_name ++ ": Popped (sentinel), now have {} items\n", .{self.height()});
            }
        } else struct {};

        /// Return a slice of the last N items on the stack.
        pub fn lastNItems(self: Self, n: u32) []const T {
            const item_count = self.stack.items.len;
            return self.stack.items[item_count - n .. item_count];
        }

        /// Pop the last N items off the stack.
        pub fn popNItems(self: *Self, n: u32) void {
            self.popNItemsImpl(n);
            if (STACK_DEBUG) std.debug.print(debug_name ++ ": Popped {} items, now have {} items\n", .{ n, self.height() });
        }

        fn popNItemsImpl(self: *Self, n: u32) void {
            self.restoreToImpl(self.height() - n);
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
            self.restoreToImpl(h);
            if (STACK_DEBUG) std.debug.print(debug_name ++ ": Restored to height {}\n", .{h});
        }

        fn restoreToImpl(self: *Self, h: usize) void {
            self.stack.shrinkRetainingCapacity(h);
        }
    };
}
