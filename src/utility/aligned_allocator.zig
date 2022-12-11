// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! An allocator that can allocate with an alignment greater than the platform's
//! page size. Different OSes expose different ways of doing so, but it seems to
//! be mostly up to "POSIX vs Windows".

const builtin = @import("builtin");
const std = @import("std");

pub fn allocate(n: usize, comptime alignment: usize) ![]align(alignment) u8 {
    switch (builtin.os.tag) {
        .windows => @compileError("TODO: Implement aligned allocation for Windows"),
        else => {
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

            // FIXME: The original Zig implementation used an "next mmap address
            //        hint" here. This could potentially reduce memory fragmentation.
            const slice = std.os.mmap(
                null,
                alloc_len,
                std.os.PROT.READ | std.os.PROT.WRITE,
                std.os.MAP.PRIVATE | std.os.MAP.ANONYMOUS,
                -1,
                0,
            ) catch return error.OutOfMemory;
            std.debug.assert(std.mem.isAligned(@ptrToInt(slice.ptr), std.mem.page_size));

            const result_ptr = std.mem.alignPointer(slice.ptr, alignment) orelse
                return error.OutOfMemory;

            // Unmap the extra bytes that were only requested in order to guarantee
            // that the range of memory we were provided had a proper alignment in
            // it somewhere. The extra bytes could be at the beginning, or end, or both.
            const drop_len = @ptrToInt(result_ptr) - @ptrToInt(slice.ptr);
            if (drop_len != 0) {
                std.os.munmap(slice[0..drop_len]);
            }

            // Unmap extra pages
            const aligned_buffer_len = alloc_len - drop_len;
            if (aligned_buffer_len > page_aligned_length) {
                std.os.munmap(@alignCast(std.mem.page_size, result_ptr[page_aligned_length..aligned_buffer_len]));
            }

            // NOTE: The original Zig implementation used std.heap.alignPageAllocLen here, but
            //       it doesn't seem to be particularly necessary here.
            return @alignCast(alignment, result_ptr[0..n]);
        },
    }
}

pub fn destroy(memory: anytype) void {
    const Slice = @typeInfo(@TypeOf(memory)).Pointer;
    const bytes = std.mem.sliceAsBytes(memory);
    const bytes_len = bytes.len + if (Slice.sentinel != null) @sizeOf(Slice.child) else 0;
    const non_const_ptr = @intToPtr([*]u8, @ptrToInt(bytes.ptr));

    const buf_unaligned = non_const_ptr[0..bytes_len];

    switch (builtin.os.tag) {
        .windows => @compileError("TODO: Implement aligned allocation for Windows"),
        else => {
            const buf_aligned_len = std.mem.alignForward(buf_unaligned.len, std.mem.page_size);
            const ptr = @alignCast(std.mem.page_size, buf_unaligned.ptr);
            std.os.munmap(ptr[0..buf_aligned_len]);
        },
    }
}
