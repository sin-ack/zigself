// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! This is an arena allocator which allows chunks to be reclaimed. It is used
//! to allocate individual handles so that they can be used throughout the VM.
//!
//! Each chunk is 1MB. The chunks are self-aligned, and an area at the start
//! of each chunk is reserved for metadata. This allows easy access to the
//! chunk's metadata area so that it can be manipulated when a handle is
//! reclaimed by the allocator.
//!
//! Each chunk maintains how many handles have been allocated from it. When a
//! chunk has no handles allocated on it and is not the last chunk, it is moved
//! to a pool of unused chunks (which is bounded by MaximumUnusedChunks).
//!
//! NOTE: The handle area does not use the Zig Allocator interface to allocate its
//!       chunks, because it doesn't allow allocating with an alignment greater than
//!       the platform's page size.

const std = @import("std");
const aligned_allocator = @import("../utility/aligned_allocator.zig");

first_chunk: *Chunk,
latest_chunk: *Chunk,
/// The chunks which are currently unused. They are maintained as a singly
/// linked list. If there are already MaximumUnusedChunks unused chunks, then
/// the newly unused chunk is simply destroyed instead of being added to this
/// list.
unused_chunks: ?*Chunk = null,
unused_chunk_count: usize = 0,

const Self = @This();
const HandleType = *[*]u64;

/// Size (and alignment) of the chunk.
const ChunkSize: usize = 1024 * 1024;
/// Magic to verify that we're pointing at a chunk.
const ChunkMagic: u64 = 0xDEADBEEFBEEFDEAD;
/// The maximum amount of unused chunks that will be kept around.
const MaximumUnusedChunks: usize = 8;
/// The maximum amount of handles that can be allocated in a single chunk.
const MaximumHandlesInChunk = @divExact(ChunkSize - @sizeOf(Chunk), @sizeOf(HandleType));

/// A chunk, starting with the metadata. Each chunk is exactly ChunkSize bytes.
const Chunk = extern struct {
    magic: u64 align(@alignOf(u64)) = ChunkMagic,

    // These two become 8 bytes on 32-bit and 16 bytes on 64-bit.
    original_allocation_ptr: [*]align(std.mem.page_size) u8 align(@alignOf(usize)),
    original_allocation_len: usize align(@alignOf(usize)),

    previous: ?*Chunk align(@alignOf(usize)) = null,
    next: ?*Chunk align(@alignOf(usize)) = null,

    count: u64 align(@alignOf(u64)) = 0,
    /// The topmost handle we allocated (in @sizeOf(HandleType)s from the start
    /// of the chunk allocation area).
    high_water_mark: usize align(@alignOf(u64)) = 0,

    pub fn create() !*Chunk {
        const allocation = try aligned_allocator.allocate(ChunkSize, ChunkSize);
        const self = @ptrCast(*Chunk, allocation.get());
        self.* = .{
            .original_allocation_ptr = allocation.original_slice.ptr,
            .original_allocation_len = allocation.original_slice.len,
        };

        return self;
    }

    pub fn fromHandle(handle: HandleType) *Chunk {
        const handle_address = @ptrToInt(handle);
        const chunk_address = handle_address & ~(ChunkSize - 1);

        const self = @intToPtr(*Chunk, chunk_address);
        if (self.magic != ChunkMagic) {
            std.debug.panic("!!! Got invalid magic {x} when attempting to get chunk from handle!", .{self.magic});
        }

        return self;
    }

    pub fn destroy(self: *Chunk) void {
        aligned_allocator.destroy(self.original_allocation_ptr[0..self.original_allocation_len]);
    }

    pub fn insertAfterMe(self: *Chunk, new_chunk: *Chunk) void {
        std.debug.assert(new_chunk.previous == null);
        std.debug.assert(new_chunk.next == null);

        new_chunk.next = self.next;
        new_chunk.previous = self;

        if (self.next) |next| next.previous = new_chunk;
        self.next = new_chunk;
    }

    pub fn insertBeforeMe(self: *Chunk, new_chunk: *Chunk) void {
        std.debug.assert(new_chunk.previous == null);
        std.debug.assert(new_chunk.next == null);

        new_chunk.previous = self.previous;
        new_chunk.next = self;

        if (self.previous) |previous| previous.next = new_chunk;
        self.previous = new_chunk;
    }

    pub fn remove(self: *Chunk) void {
        std.debug.assert(self.count == 0);
        // Wipe my high water mark too, so that I start fresh when I'm used again.
        self.high_water_mark = 0;

        if (self.previous) |previous| previous.next = self.next;
        if (self.next) |next| next.previous = self.previous;

        self.previous = null;
        self.next = null;
    }

    pub fn isFull(self: Chunk) bool {
        return self.high_water_mark == MaximumHandlesInChunk;
    }

    pub fn allocate(self: *Chunk) !HandleType {
        if (self.isFull()) {
            return error.OutOfMemory;
        }

        const memory_area = self.getMemoryArea();
        const new_handle = @ptrCast(HandleType, &memory_area[self.high_water_mark]);

        self.high_water_mark += 1;
        self.count += 1;
        return new_handle;
    }

    pub fn free(self: *Chunk, handle: HandleType) bool {
        if (!self.handleInChunk(handle)) {
            @panic("Attempting to free handle from chunk which doesn't own it");
        }

        // If the handle we are given is the handle we just allocated, then
        // simply "rewind" the HWM and pretend we never allocated it.
        if (self.high_water_mark > 0) {
            const handle_address = @ptrToInt(handle);
            const previous_allocation_address = @ptrToInt(&self.getMemoryArea()[self.high_water_mark - 1]);
            if (handle_address == previous_allocation_address)
                self.high_water_mark -= 1;
        }

        self.count -= 1;
        return self.count == 0;
    }

    fn getMemoryArea(self: *Chunk) [*]u64 {
        return @intToPtr([*]u64, @ptrToInt(self) + @sizeOf(Chunk));
    }

    fn handleInChunk(self: *Chunk, handle: HandleType) bool {
        const start_of_memory = @ptrToInt(self.getMemoryArea());
        const end_of_memory = start_of_memory + MaximumHandlesInChunk * @sizeOf(HandleType);
        const handle_address = @ptrToInt(handle);

        return handle_address >= start_of_memory and handle_address < end_of_memory;
    }
};

comptime {
    if (@sizeOf(Chunk) % @alignOf([*]u64) != 0)
        @compileError("!!! Chunk header size is not aligned to [*]u64!");
}

pub fn create() !Self {
    const initial_chunk = try Chunk.create();

    return Self{
        .first_chunk = initial_chunk,
        .latest_chunk = initial_chunk,
    };
}

pub fn destroy(self: *Self) void {
    var chunk_it: ?*Chunk = self.first_chunk;
    while (chunk_it) |chunk| {
        var next_chunk = chunk.next;
        chunk.destroy();
        chunk_it = next_chunk;
    }

    chunk_it = self.unused_chunks;
    while (chunk_it) |chunk| {
        var next_chunk = chunk.next;
        chunk.destroy();
        chunk_it = next_chunk;
    }
}

pub fn allocHandle(self: *Self) !HandleType {
    return self.latest_chunk.allocate() catch |err| switch (err) {
        error.OutOfMemory => {
            try self.allocateNewChunk();
            return self.latest_chunk.allocate() catch |second_err| switch (second_err) {
                error.OutOfMemory => @panic("!!! Could not allocate a handle even after allocating a new chunk!"),
            };
        },
    };
}

pub fn freeHandle(self: *Self, handle: HandleType) void {
    const chunk = Chunk.fromHandle(handle);
    if (chunk.free(handle)) {
        if (chunk != self.latest_chunk) {
            self.moveChunkIntoUnusedPool(chunk);
        } else {
            // It is possible (has happened in practice!) that we free every
            // single handle in the latest chunk, and hit the high water mark
            // when doing so. In that case we simply need to reset the HWM of
            // the latest chunk; otherwise, the next allocHandle call will cause
            // a new chunk to be allocated unnecessarily.
            if (chunk.isFull())
                chunk.high_water_mark = 0;
        }
    }
}

fn allocateNewChunk(self: *Self) !void {
    var new_chunk = if (self.hasUnusedChunks())
        self.getFirstUnusedChunk()
    else
        try Chunk.create();

    self.latest_chunk.insertAfterMe(new_chunk);
    self.latest_chunk = new_chunk;
}

fn moveChunkIntoUnusedPool(self: *Self, chunk: *Chunk) void {
    std.debug.assert(chunk != self.latest_chunk);
    std.debug.assert(chunk.count == 0);

    if (chunk == self.first_chunk) {
        const next_chunk = chunk.next.?;
        chunk.remove();
        self.first_chunk = next_chunk;
    } else {
        chunk.remove();
    }

    if (self.unused_chunk_count < MaximumUnusedChunks) {
        if (self.unused_chunks) |first_unused_chunk| {
            first_unused_chunk.insertBeforeMe(chunk);
        }

        self.unused_chunks = chunk;
        self.unused_chunk_count += 1;
    } else {
        chunk.destroy();
    }
}

fn hasUnusedChunks(self: *Self) bool {
    return self.unused_chunk_count > 0;
}

fn getFirstUnusedChunk(self: *Self) *Chunk {
    std.debug.assert(self.hasUnusedChunks());

    const first_unused_chunk = self.unused_chunks.?;
    const next_unused_chunk = first_unused_chunk.next;

    first_unused_chunk.remove();
    self.unused_chunks = next_unused_chunk;

    self.unused_chunk_count -= 1;
    return first_unused_chunk;
}
