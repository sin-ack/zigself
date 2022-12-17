// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! An allocator that can allocate with an alignment greater than the platform's
//! page size. Different OSes expose different ways of doing so, but it seems to
//! be mostly up to "POSIX vs Windows".

const builtin = @import("builtin");
const std = @import("std");

const AlignedAllocator = @This();

pub fn allocate(n: usize, comptime alignment: usize) !Allocation(alignment) {
    // Implementation adapted from:
    // https://github.com/ziglang/zig/blob/deda6b514691c3a7ffc7931469886d0e7be2f67e/lib/std/heap.zig
    // Copyright (c) 2015-2022, Zig contributors. License: MIT

    // The requested memory size, aligned to at least a page.
    const page_aligned_length = std.mem.alignForward(n, std.mem.page_size);

    // The maximum amount that we can unmap.
    const max_drop_len = alignment - @min(alignment, std.mem.page_size);
    // The actual amount to allocate. If the amount we can drop is less than the extra
    // padding that was added as a result of forward aligning the requested size to a page,
    // then we simply use the aligned length, because that requires max_drop_len to be a
    // maximum of page_size - 1, so we will be using all the pages we get. Otherwise, we
    // allocate all the pages needed to store the memory, plus the maximum drop length,
    // in order to ensure that we find an address that's both large enough to fit the requested
    // length and is aligned to the requested alignment.
    const alloc_len = if (max_drop_len <= page_aligned_length - n)
        page_aligned_length
    else
        std.mem.alignForward(page_aligned_length + max_drop_len, std.mem.page_size);

    var slice = try std.heap.page_allocator.alignedAlloc(u8, std.mem.page_size, alloc_len);
    std.debug.assert(std.mem.isAligned(@ptrToInt(slice.ptr), std.mem.page_size));

    const result_ptr = std.mem.alignPointer(slice.ptr, alignment) orelse
        return error.OutOfMemory;

    // NOTE: The original Allocator implementation used to drop the bytes between the first
    //       correctly aligned address and the start of the given slice, but since the
    //       Allocator interface doesn't let us do that, we just leak that memory here in
    //       order to keep things simple.

    // Try to unmap extra pages at the end
    const extra_start_len = @ptrToInt(result_ptr) - @ptrToInt(slice.ptr);
    const aligned_buffer_len = alloc_len - extra_start_len;
    if (aligned_buffer_len > page_aligned_length) {
        const new_size = extra_start_len + page_aligned_length;
        if (std.heap.page_allocator.resize(slice, new_size)) {
            slice.len = new_size;
        }
    }

    // NOTE: The original Zig implementation used std.heap.alignPageAllocLen here, but
    //       it doesn't seem to be particularly necessary here.
    return Allocation(alignment){
        .original_slice = slice,
        .slice = @alignCast(alignment, result_ptr[0..n]),
    };
}

pub fn Allocation(comptime alignment: usize) type {
    return struct {
        // You can store original_slice somewhere safe and pass it to destroy below,
        // or just store the Allocation.
        original_slice: []align(std.mem.page_size) u8,
        slice: []align(alignment) u8,

        pub fn get(self: @This()) []align(alignment) u8 {
            return self.slice;
        }

        pub fn destroy(self: @This()) void {
            AlignedAllocator.destroy(self.original_slice);
        }
    };
}

pub fn destroy(original_slice: []align(std.mem.page_size) u8) void {
    std.heap.page_allocator.free(original_slice);
}
