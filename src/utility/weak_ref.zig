// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const ref_counted = @import("./ref_counted.zig");

fn WeakHandle(comptime T: type) type {
    return struct {
        allocator: Allocator,
        ptr: ?*T,
        ref: ref_counted.RefCount,

        const Self = @This();
        pub fn destroy(self: *Self) void {
            self.allocator.destroy(self);
        }
    };
}

/// The struct which should be placed on objects that will be weakly referenced.
/// When the object is initialized, the `init` method should be called; when
/// the object is deinitialized, the `deinit` method should be called.
pub fn WeakPtrBlock(comptime T: type) type {
    const Handle = WeakHandle(T);
    const Ref = ref_counted.RefPtr(Handle);

    return struct {
        handle: Ref,

        const Self = @This();

        pub fn init(allocator: Allocator, ptr: *T) !Self {
            const raw_handle_object = try allocator.create(Handle);
            raw_handle_object.allocator = allocator;
            raw_handle_object.ptr = ptr;
            raw_handle_object.ref = .{};

            return Self{ .handle = Ref.adopt(raw_handle_object) };
        }

        pub fn deinit(self: *Self) void {
            self.handle.value.ptr = null;
            self.handle.unref();
        }
    };
}

/// A weakly-referencing pointer type. The pointer within can be attempted to be
/// obtained by calling `getPointer`. If the object has already been
/// deallocated, then the method will return null. Initialize with `init` and
/// deinitialize with `deinit`.
///
/// The passed type must have a `WeakPtrBlock(T)` object as its `weak` member.
pub fn WeakPtr(comptime T: type) type {
    const HandleRef = ref_counted.RefPtr(WeakHandle(T));

    return struct {
        handle: HandleRef,

        const Self = @This();

        pub fn init(object: *T) Self {
            std.debug.assert(object.weak.handle.value.ptr != null);
            object.weak.handle.ref();
            return Self{ .handle = object.weak.handle };
        }

        pub fn deinit(self: Self) void {
            self.handle.unref();
        }

        pub fn getPointer(self: Self) ?*T {
            return self.handle.value.ptr;
        }
    };
}
