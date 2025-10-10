// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../debug.zig");

const STACK_DEBUG = debug.STACK_DEBUG;

const SentinelEntry = struct {
    height: usize,
    value: usize,
};

/// A typed stack object.
pub fn Stack(comptime T: type, comptime debug_name: []const u8, comptime sentinel: bool) type {
    return struct {
        stack: std.ArrayList(T) = .empty,
        sentinels: if (sentinel) std.ArrayList(SentinelEntry) else void = if (sentinel) .empty else {},

        const Self = @This();
        pub const SentinelIndex = if (sentinel) usize else void;

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.stack.deinit(allocator);
            if (sentinel) self.sentinels.deinit(allocator);
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

        const sentinel_methods = if (sentinel) struct {
            /// Pushes a sentinel value on the stack which can later be checked with
            /// `verifySentinel`.
            pub fn pushSentinel(self: *Self, allocator: Allocator, value: usize) !void {
                try self.sentinels.append(allocator, .{ .height = self.height(), .value = value });
                if (STACK_DEBUG) std.debug.print(debug_name ++ ": Pushed (sentinel: {}), now have {} items\n", .{ value, self.height() });
            }

            pub fn verifySentinel(self: *Self, expected: usize) void {
                const actual = self.sentinels.pop().?;
                if (actual.height != self.height())
                    std.debug.panic("!!! Failed to verify sentinel! Expected height {}, got {}", .{ actual.height, self.height() });
                if (actual.value != expected)
                    std.debug.panic("!!! Failed to verify sentinel! Expected value {}, got {}", .{ actual.value, expected });
                if (STACK_DEBUG) std.debug.print(debug_name ++ ": Popped (sentinel: {}), now have {} items\n", .{ expected, self.height() });
            }

            pub fn sentinelHeight(self: *const Self) SentinelIndex {
                return self.sentinels.items.len;
            }
        } else struct {
            pub fn pushSentinel(self: *Self, allocator: Allocator, value: usize) !void {
                _ = self;
                _ = allocator;
                _ = value;

                @panic("Stack does not support sentinel values");
            }

            pub fn verifySentinel(self: *Self, expected: usize) void {
                _ = self;
                _ = expected;

                @panic("Stack does not support sentinel values");
            }

            pub fn sentinelHeight(self: *const Self) SentinelIndex {
                _ = self;
                return;
            }
        };

        pub const pushSentinel = sentinel_methods.pushSentinel;
        pub const verifySentinel = sentinel_methods.verifySentinel;
        pub const sentinelHeight = sentinel_methods.sentinelHeight;

        /// Return a slice of the last N items on the stack.
        pub fn lastNItems(self: Self, n: u32) []const T {
            const item_count = self.stack.items.len;

            const first_item = item_count - n;
            // If sentinels are enabled, last N items must not cross a sentinel boundary.
            if (sentinel) {
                if (self.sentinels.getLastOrNull()) |entry| {
                    if (entry.height > first_item) {
                        std.debug.panic("!!! Attempted to access last {} items, but crossed sentinel boundary at index {} (value {})", .{ n, entry.height, entry.value });
                    }
                }
            }

            return self.stack.items[first_item..item_count];
        }

        /// Pop the last N items off the stack.
        pub fn popNItems(self: *Self, n: u32) void {
            self.popNItemsImpl(n);
            if (STACK_DEBUG) std.debug.print(debug_name ++ ": Popped {} items, now have {} items\n", .{ n, self.height() });
        }

        fn popNItemsImpl(self: *Self, n: u32) void {
            const target_height = self.height() - n;

            // If sentinels are enabled, popping N items must not cross a sentinel boundary.
            // Restores *are* allowed to cross sentinel boundaries, so we can't check it there.
            if (sentinel) {
                if (self.sentinels.getLastOrNull()) |entry| {
                    if (entry.height > target_height) {
                        std.debug.panic("!!! Attempted to pop last {} items, but crossed sentinel boundary at index {} (value {})", .{ n, entry.height, entry.value });
                    }
                }
            }

            self.restoreToImpl(target_height, self.sentinelHeight());
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
        pub fn restoreTo(self: *Self, h: usize, sentinel_index: SentinelIndex) void {
            self.restoreToImpl(h, sentinel_index);
            if (STACK_DEBUG) std.debug.print(debug_name ++ ": Restored to height {}\n", .{h});
        }

        fn restoreToImpl(self: *Self, h: usize, sentinel_index: SentinelIndex) void {
            self.stack.shrinkRetainingCapacity(h);
            if (sentinel) {
                self.sentinels.shrinkRetainingCapacity(sentinel_index);
            }
        }
    };
}
