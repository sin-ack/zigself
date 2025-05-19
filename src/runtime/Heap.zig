// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

// TODO: Investigate if a conservative stack scanning approach would be
//       faster. The Whippet garbage collector uses a hybrid approach where
//       the stack is scanned conservatively and the heap is scanned
//       precisely, and it seems to be faster thanks to not needing manual
//       rooting or side tables.
//
//       To implement this, we would need to have a concept of pinned objects,
//       which is fundamentally incompatible with an eden that must be cleared
//       after every minor collection.

// TODO: Investigate alternative strategies for tenuring surviving objects. Most
//       Java garbage collectors use an age-based tenuring strategy where objects
//       that survive a certain number of garbage collection cycles are tenured.
//       We currently have 24 extra bits in the object header that go unused. Seems
//       like most garbage collectors tenure objects after 15 or so cycles, so we
//       could use 4 bits for the age.
//
//       For now, we just tenure as soon as the past survivor space is full beyond
//       a certain threshold at the start of each minor collection cycle. This has
//       the major downside of tenuring some very short-lived objects that have only
//       survived a couple of cycles.
//
//       Idea from kprotty: Instead of preemptively copying aged objects, only copy
//       when age == 2^N-1 AND referenced by something. Fixes edge case where object
//       has survived many cycles but died on the exact cycle it would be tenured.

// TODO: Investigate whether region memory can be allocated through performing
//       a large mmap(PROT_NONE) and then mprotect'ing chunks of it. This would
//       allow region memory to be contiguous which would simplify a lot of
//       the old generation code. (Idea from kprotty)

// TODO: During the heap rewrite, I removed byte arrays (strings etc.) from
//       being allocated directly in the heap. See if this is better or worse
//       than the old approach. The idea is that it's easier to intern byte
//       arrays when they're off-heap, but we could just as easily hold
//       references through the VM directly and just CoW when necessary.

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../debug.zig");
const Value = value_import.Value;
const Reference = value_import.Reference;
const BaseObject = @import("base_object.zig").BaseObject;
const value_import = @import("value.zig");
const VirtualMachine = @import("VirtualMachine.zig");

const GC_DEBUG = debug.GC_DEBUG;
const GC_SPAMMY_DEBUG = debug.GC_SPAMMY_DEBUG;
const REMEMBERED_SET_DEBUG = debug.REMEMBERED_SET_DEBUG;
const HEAP_HANDLE_MISS_DEBUG = debug.HEAP_HANDLE_MISS_DEBUG;
const GC_TOKEN_ALLOCATION_DEBUG = debug.GC_TOKEN_ALLOCATION_DEBUG;
const CRASH_ON_OUT_OF_ORDER_HANDLE_FREES = debug.CRASH_ON_OUT_OF_ORDER_HANDLE_FREES;
// TODO: Restore GC_TOKEN_DEBUG
// TODO: Restore GC_TRACK_SOURCE_DEBUG

/// The size of the new generation in bytes. This is further split between eden
/// and half-spaces, controlled by SURVIVOR_SPACE_RATIO below.
const NEW_GENERATION_SIZE = 1 * 1024 * 1024;
/// The size of one region in the old generation in bytes. The old generation is
/// split into regions to allow it to grow dynamically.
const OLD_GENERATION_REGION_SIZE = 8 * 1024 * 1024;
/// The ratio of the size of the survivor space to the size of eden. The value
/// here means how many times larger eden is compared to the survivor space.
/// For example, the value 3 would mean a new generation of 1MB would have an
/// eden space of 750KB and two survivor spaces of 125KB each.
const SURVIVOR_SPACE_RATIO = 6;
/// The percentage of the past survivor space that must be full before objects
/// are tenured to the old generation.
const TENURE_THRESHOLD_PERCENT = 80;
/// How many handles should be allocated per heap.
// 2x the empirically determined maximum handles used during zig build test.
const HANDLES_PER_HEAP = 32;
/// The initial value of the old generation's high water mark. When the number of
/// active regions reaches this value * MAJOR_GC_HWM_MULTIPLIER, a major
/// collection cycle is triggered.
const INITIAL_HIGH_WATER_MARK = 5;
/// The multiplier for the high water mark to trigger a major collection cycle.
const COLLECTION_HWM_MULTIPLIER = 2;
/// The maximum number of free regions to keep around for reuse. When the number
/// of free regions exceeds this value, any extra regions are freed.
const MAX_RECYCLED_REGIONS = 3;
/// The maximum number of handles that can be present in a single handle set.
/// This directly affects how much stack space is used in VM calls, so be
/// careful with this value.
const MAX_HANDLES = 20;

comptime {
    if (NEW_GENERATION_SIZE % @sizeOf(u64) != 0) {
        @compileError("!!! NEW_GENERATION_SIZE must be a multiple of the machine word size.");
    }

    if (OLD_GENERATION_REGION_SIZE % @sizeOf(u64) != 0) {
        @compileError("!!! OLD_GENERATION_REGION_SIZE must be a multiple of the machine word size.");
    }

    if (NEW_GENERATION_SIZE % (SURVIVOR_SPACE_RATIO + 2) != 0) {
        @compileError("!!! NEW_GENERATION_SIZE must be evenly divisible by SURVIVOR_SPACE_RATIO + 2. (SURVIVOR_SPACE_RATIO * eden + 2 * survivor-semi-space)");
    }

    if (!std.math.isPowerOfTwo(HANDLES_PER_HEAP)) {
        @compileError("!!! HANDLES_PER_HEAP must be a power of two.");
    }
}

/// The zigSelf heap.
///
/// This is a generational scavenging garbage-collected heap. Unlike the
/// original Self heap, byte arrays are allocated outside the heap itself. This
/// is so that byte arrays can be interned and shared between objects.
///
/// Like with Self, there are two generations: New and old. The new generation
/// is split between eden and the survivor half-spaces. The old generation is
/// collected using tri-color mark-and-compact. The new generation collects when
/// eden is full; the old generation is dynamically sized, and collects based on
/// a heuristic.
///
/// This function takes a `Root` type that contains a method with the following
/// signature:
///
///     fn visitEdges(self: *Root, visitor: anytype) !void
///
/// where `visitor` is a structure containing a method with the following
/// signature:
///
///     fn visit(self: *@TypeOf(visitor), value: *Value, object: ?BaseObject.Ptr) !void
///
/// The `visitEdges` method should iterate over all the heap references in the
/// root and call the `visit` method on the `visitor` for each reference (and, if applicable,
/// the object containing the reference).
pub fn Heap(comptime Root: type) type {
    return struct {
        allocator: Allocator,
        new_generation: *NewGeneration,
        old_generation: *OldGeneration,

        root: *Root,

        /// The first (technically last) handle set in use. Handle sets on the
        /// stack form a linked list which can be scanned during garbage
        /// collection.
        first_handle_set: ?*Handles,

        const Self = @This();
        const FinalizationSet = std.AutoArrayHashMapUnmanaged([*]u64, void);

        /// Initialize a new heap.
        pub fn init(allocator: Allocator, root: *Root) !Self {
            const old_generation: *OldGeneration = try .create(allocator);
            errdefer old_generation.destroy(allocator);

            const new_generation: *NewGeneration = try .create(allocator);
            errdefer new_generation.destroy(allocator);

            return .{
                .allocator = allocator,
                .new_generation = new_generation,
                .old_generation = old_generation,

                .root = root,

                .first_handle_set = null,
            };
        }

        /// Deinitialize this heap.
        pub fn deinit(self: *Self) void {
            self.new_generation.destroy(self.allocator);
            self.old_generation.destroy(self.allocator);
        }

        // --- Allocation/Collection ---

        /// Allocate the given number of bytes and return it in an allocation token.
        /// This ensures that the given amount of bytes can be allocated without
        /// garbage collection occurring.
        pub fn allocate(self: *Self, bytes: usize) !AllocationToken {
            const memory = try self.allocateRaw(bytes);
            return .{ .slice = memory, .used = 0 };
        }

        pub const CollectionFlag = enum { Disable, Enable, Force };
        pub const GarbageCollectionOptions = struct {
            minor: CollectionFlag = .Enable,
            major: CollectionFlag = .Enable,
        };

        /// Collect garbage. A minor cycle will be triggered unless the old space doesn't
        /// have enough memory for the worst-case scenario for a minor cycle, in which case
        /// a major cycle will be triggered first.
        pub fn collect(self: *Self, options: GarbageCollectionOptions) !GarbageCollectionStats {
            var stats: GarbageCollectionStats = .{
                .freed_bytes = 0,
                .existing_survived_bytes = 0,
                .new_survived_bytes = 0,
                .tenured_bytes = 0,
                .old_compacted_bytes = 0,
            };

            // Ask the old generation whether it should collect (based on
            // various heuristics), and if so, collect it first. This avoids
            // extra system allocation churn if the old generation is full.
            if (options.major != .Disable and (options.major == .Force or self.old_generation.shouldCollect())) {
                try self.old_generation.collect(self, &stats);
            }

            // TODO: Handle .minor == .Force
            if (options.minor != .Disable) {
                try self.new_generation.collect(self, &stats);
            }

            return stats;
        }

        /// Allocate the given number of bytes from the new generation for the given
        /// thread. If the allocation would exceed the remaining available space in
        /// the new generation, then a garbage collection cycle is triggered.
        fn allocateRaw(self: *Self, bytes: usize) ![]u64 {
            return self.new_generation.allocate(bytes) catch |err| switch (err) {
                error.OutOfMemory => {
                    // Trigger a garbage collection cycle.
                    _ = try self.collect(.{});
                    // Try allocating again.
                    return self.new_generation.allocate(bytes) catch @panic("!!! Out of memory after garbage collection!");
                },
            };
        }

        // --- Wololo ---

        /// Update all occurrences for the given reference.
        pub fn updateAllReferencesTo(self: *Self, from: Value, to: Value) !void {
            // Don't need to update non-references.
            const from_ref = from.asReference() orelse return;

            const Visitor = struct {
                from: Reference,
                to: Value,
                heap: *Self,

                pub fn visit(visitor: *const @This(), value: *Value, object: ?BaseObject.Ptr) !void {
                    const reference = value.asReference() orelse return;
                    if (reference.getAddress() == visitor.from.getAddress()) {
                        value.* = visitor.to;
                        if (object) |o| {
                            _ = try visitor.heap.rememberObjectReference(o.asValue(), visitor.to);
                        }
                    }
                }
            };
            const visitor: Visitor = .{
                .from = from_ref,
                .to = to,
                .heap = self,
            };

            try self.root.visitEdges(visitor);
            try self.new_generation.visitAllObjects(visitor);
            try self.old_generation.visitAllObjects(visitor);
        }

        // --- Remembering object references ---

        pub fn rememberObjectReference(self: *Self, source: Value, target: Value) !bool {
            // FIXME: If we add an assignable slot to traits integer for instance, this
            //        will cause the assignment code to explode. What can we do there?
            std.debug.assert(source.type == .Object);
            if (target.type != .Object) return false;

            const source_ref = source.asReference().?;
            const target_ref = target.asReference().?;

            if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Trying to create a reference {*} -> {*}\n", .{ source_ref.getAddress(), target_ref.getAddress() });

            if (!(self.old_generation.contains(self, source_ref) and self.new_generation.contains(target_ref))) {
                if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Referrer in same or newer space than target, not creating a reference.\n", .{});
                return false;
            }

            if (REMEMBERED_SET_DEBUG) std.debug.print("Heap.rememberObjectReference: Adding to old space remembered set\n", .{});
            return self.old_generation.rememberAddress(source_ref.getAddress());
        }

        // --- Finalization ---

        /// Put the given object in the finalization set of the new generation. This must
        /// be called by object initialization code if the object needs to be finalized.
        pub fn markAddressAsNeedingFinalization(self: *Self, object: [*]u64) !void {
            if (std.debug.runtime_safety) {
                if (self.new_generation.referenceLocation(Reference.createRegular(object)) != .Eden) {
                    @panic("!!! Attempted to put object in finalization set that is not in eden!");
                }
            }

            try self.new_generation.finalization_set.put(self.allocator, object, {});
        }

        /// The new generation. This is split between eden and the survivor half-spaces.
        /// The eden space is where new objects are allocated. When the eden space is
        /// exhausted, a minor garbage collection cycle is triggered. Objects that
        /// have references from roots, other alive eden objects, or objects in the
        /// old generation are moved to the future survivor space. Each cycle, the past
        /// survivor space is scanned as well and those objects are also moved to the
        /// future survivor space, and then the two half-spaces are swapped.
        ///
        /// If the surviving objects in the survivor space exceed a certain threshold,
        /// then the objects are tenured to the old generation.
        ///
        /// If within a single cycle the surviving objects in eden exceed the available
        /// space in the survivor space, the objects will be "spilled" to the old
        /// generation.
        const NewGeneration = struct {
            // The full memory space.
            memory: []align(@alignOf(u64)) u8,

            // NOTE: All the _used fields below are in bytes.
            /// The eden memory space. This is where new objects are allocated.
            eden: []align(@alignOf(u64)) u8,
            eden_used: usize,
            /// The past survivor space, containing objects that survived the last
            /// few garbage collection cycles.
            past_survivor: []align(@alignOf(u64)) u8,
            past_survivor_used: usize,
            /// The future survivor space. Empty while not in a garbage collection
            /// cycle. During garbage collection, this is where objects that are still
            /// alive are moved to.
            future_survivor: []align(@alignOf(u64)) u8,
            future_survivor_used: usize,

            finalization_set: FinalizationSet,

            // --- Initialization ---

            pub fn create(allocator: Allocator) !*NewGeneration {
                const self = try allocator.create(NewGeneration);
                errdefer allocator.destroy(self);

                const eden_size = NEW_GENERATION_SIZE / (SURVIVOR_SPACE_RATIO + 2) * SURVIVOR_SPACE_RATIO;
                const semispace_size = NEW_GENERATION_SIZE / (SURVIVOR_SPACE_RATIO + 2);

                const memory = try allocator.alignedAlloc(u8, .of(u64), NEW_GENERATION_SIZE);
                const eden = memory[0..eden_size];
                const past_survivor = memory[eden_size..(eden_size + semispace_size)];
                const future_survivor = memory[(eden_size + semispace_size)..];

                self.* = .{
                    .memory = memory,

                    .eden = eden,
                    .eden_used = 0,

                    .past_survivor = past_survivor,
                    .past_survivor_used = 0,

                    .future_survivor = future_survivor,
                    .future_survivor_used = 0,

                    .finalization_set = .empty,
                };

                return self;
            }

            pub fn destroy(self: *NewGeneration, allocator: Allocator) void {
                // Finalize everything in the finalization set.
                for (self.finalization_set.keys()) |address| {
                    const reference = Reference.createRegular(address);
                    const object = reference.asBaseObject();
                    std.debug.assert(object.canFinalize());
                    object.finalize(allocator);
                }

                self.finalization_set.deinit(allocator);
                allocator.free(self.memory);
                allocator.destroy(self);
            }

            // --- Heap Traversal ---

            /// Visit all objects on the heap. Not all objects may be alive, but all
            /// objects will be valid.
            pub fn visitAllObjects(self: *const NewGeneration, visitor: anytype) !void {
                var current_offset: usize = 0;
                while (current_offset < self.eden_used) {
                    const address: [*]u64 = @alignCast(@ptrCast(self.eden.ptr + current_offset));
                    const reference = Reference.createRegular(address);
                    const object = reference.asBaseObject();
                    // XXX: Dynamic dispatch
                    const object_size_bytes = object.getSizeInMemory();

                    try object.visitEdges(visitor);

                    current_offset += object_size_bytes;
                }

                current_offset = 0;
                while (current_offset < self.past_survivor_used) {
                    const address: [*]u64 = @alignCast(@ptrCast(self.past_survivor.ptr + current_offset));
                    const reference = Reference.createRegular(address);
                    const object = reference.asBaseObject();
                    // XXX: Dynamic dispatch
                    const object_size_bytes = object.getSizeInMemory();

                    try object.visitEdges(visitor);

                    current_offset += object_size_bytes;
                }
            }

            // --- Allocation ---

            /// Allocate the given number of bytes from eden.
            pub fn allocate(self: *NewGeneration, bytes: usize) ![]u64 {
                std.debug.assert(bytes % @sizeOf(u64) == 0);

                if (self.eden_used + bytes > self.eden.len) {
                    return error.OutOfMemory;
                }

                const memory: []align(@alignOf(u64)) u8 = @alignCast(self.eden[self.eden_used..][0..bytes]);
                self.eden_used += bytes;

                return std.mem.bytesAsSlice(u64, memory);
            }

            /// Return whether the new generation contains the given reference.
            pub fn contains(self: *NewGeneration, reference: Reference) bool {
                return switch (self.referenceLocation(reference)) {
                    .Eden, .PastSurvivor, .FutureSurvivor => true,
                    .Outside => false,
                };
            }

            // --- Garbage Collection ---

            const CopyTarget = enum { FutureSurvivorSpace, OldGeneration };

            /// Allocate the given number of bytes from the future survivor space.
            /// Must only be called during garbage collection.
            fn allocateInFutureSurvivor(self: *NewGeneration, bytes: usize) ![]u64 {
                std.debug.assert(bytes % @sizeOf(u64) == 0);

                if (self.future_survivor_used + bytes > self.future_survivor.len) {
                    return error.OutOfMemory;
                }

                const memory: []align(@alignOf(u64)) u8 = @alignCast(self.future_survivor[self.future_survivor_used..][0..bytes]);
                self.future_survivor_used += bytes;

                return std.mem.bytesAsSlice(u64, memory);
            }

            /// Where the given reference is located relative to the new generation.
            const ReferenceLocation = enum {
                Eden,
                PastSurvivor,
                FutureSurvivor,
                Outside,
            };

            /// Return the location of the given reference.
            pub fn referenceLocation(self: *NewGeneration, reference: Reference) ReferenceLocation {
                const address = @intFromPtr(reference.getAddress());
                const eden_base = @intFromPtr(self.eden.ptr);
                const past_survivor_base = @intFromPtr(self.past_survivor.ptr);
                const future_survivor_base = @intFromPtr(self.future_survivor.ptr);

                if (address >= eden_base and address < eden_base + self.eden.len)
                    return .Eden;
                if (address >= past_survivor_base and address < past_survivor_base + self.past_survivor.len)
                    return .PastSurvivor;
                if (address >= future_survivor_base and address < future_survivor_base + self.future_survivor.len)
                    return .FutureSurvivor;

                return .Outside;
            }

            /// Copy the given reference to the target space if it's in the new
            /// generation, leaving a forwarding reference in its place. If
            /// the target is the future survivor space and the future survivor
            /// space is full, this object (and any future objects copied) will
            /// be tenured to the old generation instead.
            fn copyReference(self: *NewGeneration, heap: *Self, target: *CopyTarget, stats: *GarbageCollectionStats, reference: Reference) !Reference {
                var survived_bytes_stat = &stats.new_survived_bytes;

                switch (self.referenceLocation(reference)) {
                    .Eden => {},
                    .PastSurvivor => {
                        survived_bytes_stat = &stats.existing_survived_bytes;
                    },
                    .FutureSurvivor => @panic("!!! Attempting to copy object in future survivor space!"),
                    .Outside => return reference,
                }

                // If what we're pointing to looks like a forwarding reference, then
                // just return the address it points to. Note that we need to look at
                // what the address is *pointing to*, rather than what the address *is*,
                // unlike in the non-forwarding case.
                if (Reference.tryFromForwarding2(reference)) |forwarding_reference| {
                    if (GC_SPAMMY_DEBUG) std.debug.print("NewGeneration.copyObject: Found forwarding reference {*}\n", .{forwarding_reference.getAddress()});
                    const new_address = forwarding_reference.getAddress();
                    return Reference.createRegular(new_address);
                }

                const object = reference.asBaseObject();
                // XXX: Dynamic dispatch happens here. If there is a heap corruption
                //      bug, it will likely start here.
                const object_size_bytes = object.getSizeInMemory();

                survived_bytes_stat.* += object_size_bytes;
                stats.freed_bytes -= object_size_bytes;

                const old_memory = object.getAddress();
                const new_memory = switch (target.*) {
                    .FutureSurvivorSpace => self.allocateInFutureSurvivor(object_size_bytes) catch |err| switch (err) {
                        error.OutOfMemory => blk: {
                            // Future survivor space has no more room. We unfortunately have to
                            // tenure this object to the old generation.
                            if (GC_DEBUG) std.debug.print("NewGeneration.copyObject: Switching target to OldGeneration because future survivor space is full\n", .{});

                            target.* = .OldGeneration;
                            stats.tenured_bytes += object_size_bytes;
                            break :blk heap.old_generation.allocate(heap.allocator, object_size_bytes) catch
                                @panic("!!! Out of memory in old generation! (This should never happen since old generation grows dynamically)");
                        },
                    },
                    .OldGeneration => blk: {
                        stats.tenured_bytes += object_size_bytes;
                        break :blk heap.old_generation.allocate(heap.allocator, object_size_bytes) catch
                            @panic("!!! Out of memory in old generation! (This should never happen since old generation grows dynamically)");
                    },
                };

                @memcpy(new_memory, old_memory);

                // If the object was in our finalization set, update or move the entry.
                if (self.finalization_set.swapRemove(old_memory)) {
                    switch (target.*) {
                        .FutureSurvivorSpace => {
                            self.finalization_set.putAssumeCapacity(new_memory.ptr, {});
                        },
                        .OldGeneration => {
                            try heap.old_generation.finalization_set.put(heap.allocator, new_memory.ptr, {});
                        },
                    }
                }

                old_memory[0] = @bitCast(Reference.createForwarding(new_memory.ptr));
                return Reference.createRegular(new_memory.ptr);
            }

            /// Collect garbage in the new generation, copying objects from eden and the
            /// past survivor space to the future survivor space.
            pub fn collect(self: *NewGeneration, heap: *Self, stats: *GarbageCollectionStats) Allocator.Error!void {
                var copy_target = CopyTarget.FutureSurvivorSpace;

                // Assume that all the memory in the past survivor space is garbage at the
                // start of the cycle. We'll decrement this as we copy objects over.
                stats.freed_bytes += self.eden_used + self.past_survivor_used;

                const future_survivor_space_initial_offset = self.future_survivor_used;
                const old_generation_initial_latest_region = heap.old_generation.latest_region;
                const old_generation_initial_latest_region_offset = if (old_generation_initial_latest_region) |region| region.used else 0;

                // First, if necessary, tenure objects from the past survivor space to the
                // old generation.
                if (self.past_survivor_used >= (self.past_survivor.len * TENURE_THRESHOLD_PERCENT) / 100) {
                    if (GC_DEBUG) std.debug.print("NewGeneration.collect: Tenuring past survivor space to old generation\n", .{});

                    const Visitor = struct {
                        heap: *Self,
                        new_generation: *NewGeneration,
                        stats: *GarbageCollectionStats,

                        pub fn visit(visitor: *const @This(), value: *Value, object: ?BaseObject.Ptr) !void {
                            _ = object;

                            const reference = value.asReference() orelse return;
                            var past_survivor_copy_target = CopyTarget.OldGeneration;
                            const new_reference = try visitor.new_generation.copyReference(visitor.heap, &past_survivor_copy_target, visitor.stats, reference);
                            value.* = new_reference.asValue();
                        }
                    };
                    const visitor: Visitor = .{
                        .heap = heap,
                        .new_generation = self,
                        .stats = stats,
                    };

                    var current_offset: usize = 0;
                    while (current_offset < self.past_survivor_used) {
                        const address: [*]u64 = @alignCast(@ptrCast(self.past_survivor.ptr + current_offset));
                        const reference = Reference.createRegular(address);

                        // If this is a forwarding reference, then the object being referenced has
                        // already been copied, so the only thing we need to do is move forward.
                        if (Reference.tryFromForwarding2(reference)) |forwarding_reference| {
                            if (GC_SPAMMY_DEBUG) std.debug.print("NewGeneration.collect: Found forwarding reference {*} when tenuring to past survivor space\n", .{forwarding_reference.getAddress()});

                            const new_object = forwarding_reference.asBaseObject();
                            current_offset += new_object.getSizeInMemory();
                        } else {
                            const object = reference.asBaseObject();
                            const object_size_bytes = object.getSizeInMemory();

                            try object.visitEdges(visitor);
                            // Since we're force-copying without using any roots, we don't care about the
                            // new value we get back.
                            var value = reference.asValue();
                            try visitor.visit(&value, object);

                            current_offset += object_size_bytes;
                        }
                    }
                }

                const Visitor = struct {
                    heap: *Self,
                    new_generation: *NewGeneration,
                    copy_target: *CopyTarget,
                    stats: *GarbageCollectionStats,

                    pub fn visit(visitor: *const @This(), value: *Value, object: ?BaseObject.Ptr) !void {
                        _ = object;

                        const reference = value.asReference() orelse return;
                        const new_reference = try visitor.new_generation.copyReference(visitor.heap, visitor.copy_target, visitor.stats, reference);
                        value.* = new_reference.asValue();
                    }
                };
                const visitor: Visitor = .{
                    .heap = heap,
                    .new_generation = self,
                    .copy_target = &copy_target,
                    .stats = stats,
                };

                // Kick off the process by visiting roots.
                try heap.root.visitEdges(visitor);
                if (heap.first_handle_set) |handle_set| {
                    try handle_set.visitEdges(visitor);
                }

                // Iterate through the old generation's regions and check each
                // card in the region's remembered set to see if there are any
                // objects that are pointing to the new region.
                var current_region = heap.old_generation.first_region;
                while (current_region) |region| {
                    current_region = region.next;

                    for (region.remembered_set.cards, 0..) |card, index| {
                        if (card == .Empty) continue;

                        const card_start = index << RememberedSet.CARD_INDEX_SHIFT;
                        const card_end = @min(region.used, (index + 1) << RememberedSet.CARD_INDEX_SHIFT);
                        const first_offset = card.wordOffset() * Card.WORD_SIZE;

                        // We hit the end of the region, so no point in going further.
                        if (card_end < card_start) break;

                        var offset = card_start + first_offset;
                        while (offset < card_end) {
                            const address: [*]u64 = @alignCast(@ptrCast(region.memory.ptr + offset));
                            const reference = Reference.createRegular(address);

                            const object = reference.asBaseObject();
                            const object_size_bytes = object.getSizeInMemory();

                            try object.visitEdges(visitor);

                            offset += object_size_bytes;
                        }
                    }
                }

                // Cheney on the MTA: Try to catch up to the end of the future survivor
                // space by scanning every object and copying over references to eden
                // or the past survivor space.
                var target_current_offset = future_survivor_space_initial_offset;
                while (target_current_offset < self.future_survivor_used) {
                    const address: [*]u64 = @alignCast(@ptrCast(self.future_survivor.ptr + target_current_offset));
                    const reference = Reference.createRegular(address);

                    const object = reference.asBaseObject();
                    const object_size_bytes = object.getSizeInMemory();

                    try object.visitEdges(visitor);

                    target_current_offset += object_size_bytes;
                }

                // If our target switched to the old generation at any point, we need to
                // also scan the objects in the old generation that have been added since
                // the start of the garbage collection cycle.
                if (copy_target == .OldGeneration) {
                    target_current_offset = old_generation_initial_latest_region_offset;
                    var current_scanned_region = old_generation_initial_latest_region;
                    while (current_scanned_region) |region| {
                        while (target_current_offset < region.used) {
                            const address: [*]u64 = @alignCast(@ptrCast(region.memory.ptr + target_current_offset));
                            const reference = Reference.createRegular(address);

                            const object = reference.asBaseObject();
                            const object_size_bytes = object.getSizeInMemory();

                            try object.visitEdges(visitor);

                            target_current_offset += object_size_bytes;
                        }

                        current_scanned_region = region.next;
                        target_current_offset = 0;
                    }
                }

                // Finalize any objects still in the finalization set and is part of eden
                // or the past survivor space.
                var new_finalization_set: FinalizationSet = .empty;
                for (self.finalization_set.keys()) |address| {
                    const reference = Reference.createRegular(address);
                    switch (self.referenceLocation(reference)) {
                        .Eden, .PastSurvivor => {
                            const object = reference.asBaseObject();
                            std.debug.assert(object.canFinalize());
                            object.finalize(heap.allocator);
                        },
                        .FutureSurvivor => {
                            try new_finalization_set.put(heap.allocator, address, {});
                        },
                        .Outside => @panic("!!! Finalization set contains object outside of new generation!"),
                    }
                }

                self.finalization_set.deinit(heap.allocator);
                self.finalization_set = new_finalization_set;

                // We have now copied all the objects we need to, and can assume that the
                // eden and past survivor spaces are completely empty.
                self.eden_used = 0;
                self.past_survivor_used = 0;

                // Swap the past and future survivor spaces.
                std.mem.swap([]align(@alignOf(u64)) u8, &self.past_survivor, &self.future_survivor);
                std.mem.swap(usize, &self.past_survivor_used, &self.future_survivor_used);
            }
        };

        const Card = enum(u8) {
            Empty = 0xFF,
            _,

            pub const RANGE = 0x80;
            pub const WORD_SIZE = @sizeOf(u64);
            pub const MAXIMUM_WORD_OFFSET = RANGE - 1;
            pub const MAXIMUM_BYTE_OFFSET = WORD_SIZE * MAXIMUM_WORD_OFFSET;

            /// Create a card from the given offset (in bytes).
            pub fn fromOffset(offset: u16) Card {
                std.debug.assert(offset <= MAXIMUM_BYTE_OFFSET);
                std.debug.assert(offset % WORD_SIZE == 0);

                return @enumFromInt(@divExact(offset, WORD_SIZE));
            }

            /// Return the word offset for this card.
            pub fn wordOffset(self: Card) u8 {
                std.debug.assert(self != .Empty);
                return @intFromEnum(self);
            }
        };

        const RememberedSet = struct {
            cards: []Card,

            const BYTES_PER_CARD = Card.RANGE * Card.WORD_SIZE;
            const CARD_INDEX_SHIFT = std.math.log2_int(usize, BYTES_PER_CARD);

            comptime {
                std.debug.assert(std.math.isPowerOfTwo(BYTES_PER_CARD));
            }

            /// Create a new remembered set based on the size (in bytes) of the
            /// memory region that cards will be maintained for.
            pub fn createFromSize(allocator: Allocator, size: usize) !RememberedSet {
                std.debug.assert(size % BYTES_PER_CARD == 0);

                const size_in_cards = @divExact(size, BYTES_PER_CARD);
                const cards = try allocator.alloc(Card, size_in_cards);
                @memset(cards, .Empty);

                return .{ .cards = cards };
            }

            /// Destroy this remembered set.
            pub fn destroy(self: *RememberedSet, allocator: Allocator) void {
                allocator.free(self.cards);
            }

            /// Get the index in the cards array for this offset (in bytes)
            /// inside the memory region.
            fn cardIndex(offset: usize) usize {
                return offset >> CARD_INDEX_SHIFT;
            }

            /// Mark the card corresponding to the given offset (in bytes).
            pub fn mark(self: *RememberedSet, offset: usize) void {
                const index = cardIndex(offset);
                std.debug.assert(index < self.cards.len);

                const card_offset: u16 = @intCast(offset % BYTES_PER_CARD);
                const new_card = Card.fromOffset(card_offset);
                // NOTE: The new generation will scan until the next card, so
                //       we have to always point to the *first* object in the
                //       region that must be remembered.
                if (self.cards[index] == .Empty or
                    self.cards[index].wordOffset() > new_card.wordOffset())
                {
                    self.cards[index] = new_card;
                }
            }

            /// Reset the remembered set.
            pub fn reset(self: *RememberedSet) void {
                @memset(self.cards, .Empty);
            }
        };

        /// A single region within the old generation.
        const Region = struct {
            memory: []align(@alignOf(u64)) u8,

            /// The next region in the list.
            next: ?*Region,
            /// How many bytes are currently used within this region.
            used: usize,

            remembered_set: RememberedSet,

            pub fn create(allocator: Allocator) !*Region {
                // TODO: Instead of doing two disjoint allocations, allocate
                //       OLD_GENERATION_REGION_SIZE + @sizeOf(Region) and then
                //       split it up into the region and the memory.
                const self = try allocator.create(Region);
                errdefer allocator.destroy(self);

                const memory = try allocator.alignedAlloc(u8, .of(u64), OLD_GENERATION_REGION_SIZE);
                errdefer allocator.free(memory);

                const remembered_set = try RememberedSet.createFromSize(allocator, OLD_GENERATION_REGION_SIZE);

                self.* = .{
                    .memory = memory,
                    .next = null,
                    .used = 0,
                    .remembered_set = remembered_set,
                };

                return self;
            }

            pub fn destroy(self: *Region, allocator: Allocator) void {
                self.remembered_set.destroy(allocator);
                allocator.free(self.memory);
                allocator.destroy(self);
            }

            /// Reset this region's state. All contents will be assumed to be
            /// garbage.
            pub fn reset(self: *Region) void {
                self.used = 0;
                self.remembered_set.reset();
            }

            // --- Memory Stats ---

            /// Get the number of bytes available in this region.
            pub fn availableMemory(self: *Region) usize {
                return self.memory.len - self.used;
            }

            // --- Allocation ---

            /// Allocate the given number of bytes from this region.
            fn allocate(self: *Region, bytes: usize) ![]u64 {
                std.debug.assert(bytes % @sizeOf(u64) == 0);

                if (self.used + bytes > self.memory.len) {
                    return error.OutOfMemory;
                }

                const memory: []align(@alignOf(u64)) u8 = @alignCast(self.memory[self.used..][0..bytes]);
                self.used += bytes;

                return std.mem.bytesAsSlice(u64, memory);
            }

            // --- Remembered Set Handling ---

            /// Remember the given absolute memory address by marking it down
            /// in the remembered set.
            /// Returns true if the address was recorded, or false if the
            /// address doesn't belong to this region.
            pub fn rememberAddress(self: *Region, address: [*]u64) bool {
                const address_value = @intFromPtr(address);
                const region_start = @intFromPtr(self.memory.ptr);
                const region_end = @intFromPtr(self.memory.ptr + self.memory.len);

                if (!(address_value >= region_start and address_value < region_end))
                    return false;

                self.remembered_set.mark(address_value - region_start);
                return true;
            }
        };

        /// The old generation. This is a mark-compact garbage collector. The
        /// memory is split into regions to allow it to grow dynamically.
        const OldGeneration = struct {
            /// The first region in the list of regions. The regions are linked
            /// together in a singly-linked list.
            first_region: ?*Region,
            /// The latest region in the list of regions. This is used to
            /// allocate new memory.
            latest_region: ?*Region,

            /// Singly-linked list of regions that can be reused. When regions
            /// are freed, they are added to this list (up to a certain limit).
            first_free_region: ?*Region,

            /// The number of regions that remained allocated the last time a
            /// major garbage collection cycle was performed. This is used to
            /// determine whether we should collect or not.
            high_water_mark: u32,
            /// The number of currently-allocated regions (does not include regions
            /// that are in the free list).
            active_regions: u32,
            /// The number of free regions that are currently available.
            free_regions: u32,

            /// The total number of bytes used in the old generation.
            used: usize,

            finalization_set: FinalizationSet,

            /// Initialize the old generation.
            pub fn create(allocator: Allocator) !*OldGeneration {
                const self = try allocator.create(OldGeneration);
                errdefer allocator.destroy(self);

                self.* = .{
                    .first_region = null,
                    .latest_region = null,
                    .first_free_region = null,
                    .high_water_mark = INITIAL_HIGH_WATER_MARK,
                    .active_regions = 0,
                    .free_regions = 0,
                    .used = 0,
                    .finalization_set = .empty,
                };

                return self;
            }

            /// Deinitialize the old generation.
            pub fn destroy(self: *OldGeneration, allocator: Allocator) void {
                var region = self.first_free_region;
                while (region) |r| {
                    const next = r.next;
                    r.destroy(allocator);
                    region = next;
                }

                region = self.first_region;
                while (region) |r| {
                    const next = r.next;
                    r.destroy(allocator);
                    region = next;
                }

                // Finalize everything in the finalization set.
                for (self.finalization_set.keys()) |address| {
                    const reference = Reference.createRegular(address);
                    const object = reference.asBaseObject();
                    std.debug.assert(object.canFinalize());
                    object.finalize(allocator);
                }

                self.finalization_set.deinit(allocator);
                allocator.destroy(self);
            }

            // --- Heap Traversal ---

            /// Visit all objects on the heap. Not all objects may be alive, but all
            /// objects will be valid.
            pub fn visitAllObjects(self: *const OldGeneration, visitor: anytype) !void {
                var current_region = self.first_region;
                while (current_region) |region| {
                    current_region = region.next;

                    var current_offset: usize = 0;
                    while (current_offset < region.used) {
                        const address: [*]u64 = @alignCast(@ptrCast(region.memory.ptr + current_offset));
                        const reference = Reference.createRegular(address);
                        const object = reference.asBaseObject();
                        // XXX: Dynamic dispatch
                        const object_size_bytes = object.getSizeInMemory();

                        try object.visitEdges(visitor);

                        current_offset += object_size_bytes;
                    }
                }
            }

            // --- Allocation ---

            /// Allocate the given number of bytes from the old generation.
            pub fn allocate(self: *OldGeneration, allocator: Allocator, bytes: usize) ![]u64 {
                std.debug.assert(bytes % @sizeOf(u64) == 0);

                const latest_region = self.latest_region orelse {
                    // No regions have been allocated yet. Allocate a new one.
                    try self.makeFreeRegion(allocator);
                    return try self.allocate(allocator, bytes);
                };

                const memory = latest_region.allocate(bytes) catch |err| switch (err) {
                    error.OutOfMemory => {
                        // The latest region is full. Try to allocate a new one.
                        try self.makeFreeRegion(allocator);
                        return try self.allocate(allocator, bytes);
                    },
                };

                self.used += bytes;
                return memory;
            }

            /// Get the number of bytes available in the old generation. Any
            /// wasted space at the end of regions except the latest isn't
            /// counted.
            pub fn availableMemory(self: *OldGeneration) usize {
                return if (self.latest_region) |region| region.availableMemory() else 0;
            }

            /// Return whether the old generation contains the given reference.
            pub fn contains(self: *OldGeneration, heap: *Self, reference: Reference) bool {
                _ = self;
                return !heap.new_generation.contains(reference);
            }

            // --- Remembered Set Handling ---

            /// Remember the given absolute memory address by marking it down in
            /// the respective region's remembered set.
            /// Returns true if the address was recorded, or false if the address
            /// doesn't belong to the old generation.
            pub fn rememberAddress(self: *OldGeneration, address: [*]u64) bool {
                var current_region = self.first_region;
                while (current_region) |region| {
                    current_region = region.next;

                    if (region.rememberAddress(address)) return true;
                }

                return false;
            }

            // --- Region Management ---

            /// Try to find a free region, either through the free region list or by
            /// allocating a new one.
            fn makeFreeRegion(self: *OldGeneration, allocator: Allocator) !void {
                const new_region = new_region: {
                    if (self.first_free_region) |free_region| {
                        // Recycle a free region.
                        self.first_free_region = free_region.next;
                        free_region.next = null;
                        self.free_regions -= 1;

                        break :new_region free_region;
                    } else {
                        // No free regions available. Allocate a new one.
                        const region = try Region.create(allocator);
                        break :new_region region;
                    }
                };

                if (self.latest_region) |latest_region| {
                    latest_region.next = new_region;
                } else {
                    // If we don't have a latest region, then we don't have a first
                    // region either.
                    self.first_region = new_region;
                }

                self.latest_region = new_region;
                self.active_regions += 1;
            }

            // --- Garbage Collection ---

            pub fn shouldCollect(self: *OldGeneration) bool {
                // TODO: Currently we use the very simple heuristic of region
                //       count. In the future, we could use a more sophisticated
                //       approach like:
                //       1. Time-based major GC for programs with stable memory
                //          footprint.
                //       2. Increase or decrease collection multiplier based on
                //          allocation and garbage collection rate.

                return self.active_regions >= self.high_water_mark * COLLECTION_HWM_MULTIPLIER;
            }

            fn visitRootsAndNewGeneration(heap: *Self, visitor: anytype) !void {
                // Visit the heap roots (whatever they may be).
                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Visiting heap roots\n", .{});
                try heap.root.visitEdges(visitor);

                // Visit the heap handles.
                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Visiting handles\n", .{});
                if (heap.first_handle_set) |handle_set| {
                    try handle_set.visitEdges(visitor);
                }

                // Visit eden and the past survivor space, since they can point into
                // the old generation.
                var current_offset: usize = 0;

                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Visiting all eden objects\n", .{});
                while (current_offset < heap.new_generation.eden_used) {
                    const address: [*]u64 = @alignCast(@ptrCast(heap.new_generation.eden.ptr + current_offset));
                    const reference = Reference.createRegular(address);

                    const object = reference.asBaseObject();
                    try object.visitEdges(visitor);

                    current_offset += object.getSizeInMemory();
                }

                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Visiting all past survivor objects\n", .{});
                while (current_offset < heap.new_generation.past_survivor_used) {
                    const address: [*]u64 = @alignCast(@ptrCast(heap.new_generation.past_survivor.ptr + current_offset));
                    const reference = Reference.createRegular(address);

                    const object = reference.asBaseObject();
                    try object.visitEdges(visitor);

                    current_offset += object.getSizeInMemory();
                }
            }

            /// Collect garbage in the old generation using a mark-compact algorithm.
            pub fn collect(self: *OldGeneration, heap: *Self, stats: *GarbageCollectionStats) !void {
                if (GC_DEBUG) std.debug.print("OldGeneration.collect: Performing major collection\n", .{});

                const first_region = self.first_region orelse {
                    // No regions have been allocated yet! There's no garbage to collect.
                    if (GC_DEBUG) std.debug.print("OldGeneration.collect: No regions allocated, nothing to collect\n", .{});
                    return;
                };

                // Assume everything is garbage at the start of the cycle.
                const initial_used = self.used;
                stats.freed_bytes += self.used;

                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Initial used size: {} bytes\n", .{initial_used});

                var arena: std.heap.ArenaAllocator = .init(heap.allocator);
                defer arena.deinit();
                const allocator = arena.allocator();

                // NOTE: All allocations in the code below are done with the arena above,
                //       so deinitialization steps are intentionally omitted.

                var gray_queue: std.ArrayListUnmanaged([*]u64) = .empty;
                var black_set: std.AutoArrayHashMapUnmanaged([*]u64, void) = .empty;

                const MarkVisitor = struct {
                    allocator: Allocator,
                    new_generation: *NewGeneration,
                    gray_queue: *std.ArrayListUnmanaged([*]u64),
                    black_set: *std.AutoArrayHashMapUnmanaged([*]u64, void),

                    pub fn visit(visitor: *const @This(), value: *Value, object: ?BaseObject.Ptr) !void {
                        _ = object;

                        const reference = value.asReference() orelse return;
                        const address = reference.getAddress();

                        // If the object is already known to be alive, we don't need to visit it.
                        if (visitor.black_set.contains(address)) {
                            return;
                        }

                        // Make sure we only include objects that are actually in the old
                        // generation. If the heap is sound, this means that the object
                        // is *not* in the new generation (so we don't have to linearly
                        // check all of our regions for it).
                        if (visitor.new_generation.referenceLocation(reference) != .Outside) {
                            return;
                        }

                        if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Found object at {*} to mark\n", .{address});
                        try visitor.gray_queue.append(visitor.allocator, address);
                        try visitor.black_set.put(visitor.allocator, address, {});
                    }
                };
                const mark_visitor: MarkVisitor = .{
                    .allocator = allocator,
                    .new_generation = heap.new_generation,
                    .gray_queue = &gray_queue,
                    .black_set = &black_set,
                };

                // Kick off the marking process by visiting all the roots.
                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Performing initial mark\n", .{});
                try visitRootsAndNewGeneration(heap, mark_visitor);

                // Start popping from the gray queue, and visit all the objects reachable
                // from there.
                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Visiting gray queue\n", .{});
                while (gray_queue.pop()) |address| {
                    const reference = Reference.createRegular(address);

                    const object = reference.asBaseObject();
                    try object.visitEdges(mark_visitor);
                }

                // Now that we've marked all the live objects, we can compact the heap.
                // Note that unlike with the new generation, which is a moving collector,
                // we have to store the new addresses of the objects in an out-of-band table
                // since we will most likely overwrite the forwarding addresses.
                var remap_table: std.AutoArrayHashMapUnmanaged([*]u64, [*]u64) = .empty;
                var current_total_used: usize = 0;
                var first_empty_region: ?*Region = null;
                var new_active_regions: u32 = 0;

                // Go through every object and find the ones in the black set, and compact
                // them to the left. For all the ones that are not in the black set, finalize
                // them if necessary.
                //
                // NOTE: We can't just go through the black set, since this is an overwriting
                //       GC method, and we need to maintain a strict ordering of the
                //       objects to scan.
                {
                    var current_scanned_region: ?*Region = first_region;
                    var current_allocation_region = first_region;
                    while (current_scanned_region) |scanned_region| {
                        const previously_used = scanned_region.used;
                        var offset: usize = 0;

                        // NOTE: We conceptually create a "shadow copy" of the region here by
                        //       resetting it (which essentially means everything is treated
                        //       as garbage). Afterwards we will pick the objects that are
                        //       still alive and compact them to the left. This is guaranteed
                        //       to be safe since it's impossible to have more surviving
                        //       objects than the objects that were in the region initially.
                        scanned_region.reset();

                        while (offset < previously_used) {
                            const address: [*]u64 = @alignCast(@ptrCast(scanned_region.memory.ptr + offset));
                            const reference = Reference.createRegular(address);

                            const object = reference.asBaseObject();
                            // XXX: Dynamic dispatch happens here. If there is a heap corruption
                            //      bug, it will likely start here.
                            const object_size_bytes = object.getSizeInMemory();

                            if (black_set.contains(address)) {
                                const old_memory = address[0..@divExact(object_size_bytes, @sizeOf(u64))];
                                const new_memory = current_allocation_region.allocate(object_size_bytes) catch |err| new_address: switch (err) {
                                    error.OutOfMemory => {
                                        // Switch over to the next region to begin allocating.
                                        current_allocation_region = current_allocation_region.next orelse
                                            @panic("!!! Somehow tried to allocate more memory while garbage collecting than what was previously used?!");
                                        if (current_allocation_region.used != 0)
                                            @panic("!!! Allocation region is ahead of scan region?!");

                                        new_active_regions += 1;
                                        break :new_address current_allocation_region.allocate(object_size_bytes) catch
                                            @panic("!!! Failed to allocate even after switching allocation regions!");
                                    },
                                };

                                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Copying alive object {*} to {*}\n", .{ old_memory, new_memory });
                                try remap_table.put(allocator, address, new_memory.ptr);
                                std.mem.copyForwards(u64, new_memory, old_memory);
                                current_total_used += object_size_bytes;

                                if (self.finalization_set.swapRemove(address)) {
                                    // If this object was in the finalization set, we need to
                                    // update the entry to point to the new address.
                                    self.finalization_set.putAssumeCapacity(new_memory.ptr, {});
                                }
                            } else {
                                if (self.finalization_set.swapRemove(address)) {
                                    std.debug.assert(object.canFinalize());
                                    object.finalize(heap.allocator);
                                }
                            }

                            offset += object_size_bytes;
                        }

                        current_scanned_region = scanned_region.next;
                    }

                    // Ensure that we reset all the remaining regions beyond the
                    // current allocation, since anything beyond the current
                    // region is only dead objects.
                    //
                    // TODO: Support compacting away even the first region if
                    //       no objects survived.
                    first_empty_region = current_allocation_region.next;
                    current_allocation_region.next = null;
                    self.latest_region = current_allocation_region;
                    if (first_empty_region) |empty_region| {
                        var region: ?*Region = empty_region;
                        while (region) |r| {
                            r.reset();
                            region = r.next;
                        }
                    }
                }

                self.used = current_total_used;
                self.active_regions = new_active_regions;
                self.high_water_mark = new_active_regions;

                const RemapVisitor = struct {
                    remap_table: *std.AutoArrayHashMapUnmanaged([*]u64, [*]u64),

                    pub fn visit(visitor: *const @This(), value: *Value, object: ?BaseObject.Ptr) !void {
                        _ = object;

                        const reference = value.asReference() orelse return;
                        const address = reference.getAddress();

                        if (visitor.remap_table.get(address)) |new_address| {
                            value.* = Value.fromObjectAddress(new_address);
                        }
                    }
                };
                const remap_visitor: RemapVisitor = .{ .remap_table = &remap_table };

                // Go through all the existing objects and remap the references.
                if (GC_SPAMMY_DEBUG) std.debug.print("OldGeneration.collect: Remapping all external objects\n", .{});
                try visitRootsAndNewGeneration(heap, remap_visitor);

                // Do the same for all of our surviving objects in the old generation.
                {
                    var current_remap_region: ?*Region = first_region;
                    while (current_remap_region) |remap_region| {
                        var offset: usize = 0;
                        while (offset < remap_region.used) {
                            const address: [*]u64 = @alignCast(@ptrCast(remap_region.memory.ptr + offset));
                            const reference = Reference.createRegular(address);

                            const object = reference.asBaseObject();
                            try object.visitEdges(remap_visitor);

                            offset += object.getSizeInMemory();
                        }

                        current_remap_region = remap_region.next;
                    }
                }

                const RememberedSetVisitor = struct {
                    new_generation: *NewGeneration,
                    old_generation: *OldGeneration,
                    found: bool,

                    pub fn visit(visitor: *@This(), value: *Value, object: ?BaseObject.Ptr) !void {
                        _ = object;

                        const reference = value.asReference() orelse return;
                        const address = reference.getAddress();

                        switch (visitor.new_generation.referenceLocation(reference)) {
                            .Outside => return,
                            .FutureSurvivor => std.debug.panic("!!! Old generation object pointing to future survivor space address {*}?!", .{address}),
                            .Eden, .PastSurvivor => {},
                        }

                        // TODO: Find a way to short-circuit this.
                        visitor.found = true;
                    }
                };
                var remembered_set_visitor: RememberedSetVisitor = .{
                    .new_generation = heap.new_generation,
                    .old_generation = self,
                    .found = false,
                };

                // Go through all objects once more and reconstruct the
                // remembered sets for each region.
                {
                    var current_rs_region: ?*Region = first_region;
                    while (current_rs_region) |rs_region| {
                        var offset: usize = 0;
                        while (offset < rs_region.used) {
                            remembered_set_visitor.found = false;

                            const address: [*]u64 = @alignCast(@ptrCast(rs_region.memory.ptr + offset));
                            const reference = Reference.createRegular(address);

                            const object = reference.asBaseObject();
                            try object.visitEdges(&remembered_set_visitor);

                            if (remembered_set_visitor.found) {
                                const did_remember = rs_region.rememberAddress(reference.getAddress());
                                std.debug.assert(did_remember);
                            }

                            offset += object.getSizeInMemory();
                        }

                        current_rs_region = rs_region.next;
                    }
                }

                stats.old_compacted_bytes = initial_used - self.used;
                stats.freed_bytes -= self.used;

                if (GC_DEBUG) std.debug.print("OldGeneration.collect: Collected {} bytes, {} bytes remain in old generation\n", .{ stats.old_compacted_bytes, self.used });

                self.compactRegions(heap.allocator, first_empty_region);
            }

            /// Recycle or free all the regions starting from the given region.
            /// This will free the regions if the number of regions is greater
            /// than MAX_RECYCLED_REGIONS, otherwise it will recycle them.
            fn compactRegions(self: *OldGeneration, allocator: Allocator, first_region: ?*Region) void {
                var current_region = first_region;
                while (current_region) |r| {
                    current_region = r.next;

                    if (self.free_regions == MAX_RECYCLED_REGIONS) {
                        // Free the region.
                        r.destroy(allocator);
                    } else {
                        // Recycle the region.
                        r.next = self.first_free_region;
                        self.first_free_region = r;
                        self.free_regions += 1;
                    }
                }
            }
        };

        /// A set of handles that can be used within a local stack frame to hold
        /// references to objects which will be updated.
        ///
        /// Usage:
        ///
        ///     var handles: Heap.Handles = undefined;
        ///     handles.init(&heap);
        ///     defer handles.deinit(&heap);
        ///
        ///     // ...
        ///
        ///     var my_value = some_value;
        ///     handles.trackValue(&my_value);
        ///
        ///     // ...
        ///
        ///     var my_object = SomeObject.create(&token, ...);
        ///     handles.trackObject(@ptrCast(&my_object));
        ///
        /// The idea is taken from the legend:
        /// https://bernsteinbear.com/blog/scrapscript-baseline/#inside-the-runtime-handles
        pub const Handles = struct {
            handles: [MAX_HANDLES]Handle,
            len: usize,
            next: ?*Handles,

            // TODO: See whether this representation can be improved as we're
            //       only holding 1 bit per handle for info.
            const Handle = union(enum) {
                Value: *Value,
                Object: *BaseObject.Ptr,
            };

            /// Initialize the handle set.
            pub fn init(self: *Handles, heap: *Self) void {
                self.* = .{
                    .handles = undefined,
                    .len = 0,
                    .next = heap.first_handle_set,
                };
                heap.first_handle_set = self;
            }

            /// Deinitialize the handle set.
            pub fn deinit(self: *Handles, heap: *Self) void {
                heap.first_handle_set = self.next;
            }

            /// Store a handle to a Value. The Value that is being pointed to
            /// does not have to be a reference.
            pub fn trackValue(self: *Handles, value: *Value) void {
                std.debug.assert(self.len < MAX_HANDLES);

                self.handles[self.len] = .{ .Value = value };
                self.len += 1;
            }

            /// Store a reference to some object. The object must descend from
            /// (embed) BaseObject.
            pub fn trackObject(self: *Handles, object: *BaseObject.Ptr) void {
                std.debug.assert(self.len < MAX_HANDLES);

                self.handles[self.len] = .{ .Object = object };
                self.len += 1;
            }

            /// Visit all the values in this handle set and (if present) the
            /// next one.
            pub fn visitEdges(self: *const Handles, visitor: anytype) !void {
                for (0..self.len) |i| {
                    switch (self.handles[i]) {
                        .Value => |v| try visitor.visit(v, null),
                        .Object => |o| {
                            var value = o.*.asValue();
                            try visitor.visit(&value, null);
                            if (value.asBaseObject()) |base_object| {
                                @branchHint(.likely);
                                o.* = base_object;
                            } else {
                                @branchHint(.cold);
                                @panic("Object became non-object after visit?!");
                            }
                        },
                    }
                }

                if (self.next) |next_set| {
                    try next_set.visitEdges(visitor);
                }
            }
        };
    };
}

const GarbageCollectionStats = struct {
    /// The total number of bytes freed during this cycle, including both the
    /// new and old generations.
    freed_bytes: usize,
    /// The number of bytes that were in the past survivor space and survived
    /// this garbage collection cycle. This value is greater than 0 only if
    /// a minor garbage collection cycle was triggered.
    existing_survived_bytes: usize,
    /// The number of bytes that were allocated during this garbage collection
    /// cycle and survived. This value is greater than 0 only if a minor garbage
    /// collection cycle was triggered.
    new_survived_bytes: usize,
    /// The number of bytes that were tenured to the old generation during this
    /// garbage collection cycle. This value is greater than 0 only if a minor
    /// garbage collection cycle was triggered.
    tenured_bytes: usize,
    /// The number of bytes that were compacted in the old generation during this
    /// garbage collection cycle. This value is greater than 0 only if a major
    /// garbage collection cycle was triggered.
    old_compacted_bytes: usize,
};

/// An allocation token. This is used as a way to obtain "safepoints" for
/// allocations. Once a token is allocated, the VM code can be certain that no
/// garbage collection will occur for the given amount of bytes.
///
/// The allocation token is also useful for verifying that the correct amount of
/// bytes are allocated for each object. AllocationToken.deinit will check that
/// the token has been fully used.
pub const AllocationToken = struct {
    /// The slice of memory that this token represents. This will always be
    /// inside eden.
    slice: []u64,
    /// The number of bytes used so far.
    used: usize,

    /// Allocate the given number of bytes from this token. This will panic if
    /// the token is used up.
    pub fn allocate(self: *@This(), bytes: usize) [*]u64 {
        std.debug.assert(bytes % @sizeOf(u64) == 0);
        const size = self.slice.len * @sizeOf(u64);

        if (self.used + bytes > size) {
            std.debug.panic(
                "!!! Attempted to allocate {} bytes from {} byte-sized allocation token with {} bytes remaining!",
                .{ bytes, size, size - self.used },
            );
        }

        const address = self.slice.ptr + (self.used / @sizeOf(u64));
        self.used += bytes;
        if (GC_TOKEN_ALLOCATION_DEBUG) std.debug.print("AllocationToken.allocate: {}/{} bytes allocated at {*} (requested {})\n", .{ self.used, size, address, bytes });
        return address;
    }

    /// Deinitialize this allocation token. In safe modes, this will check that
    /// the token has been fully used up.
    pub fn deinit(self: @This()) void {
        if (std.debug.runtime_safety) {
            const size = self.slice.len * @sizeOf(u64);
            if (self.used != size) {
                std.debug.panic("!!! Only {} out of {} bytes consumed from the allocation token!", .{ self.used, size });
            }
        }
    }
};

// --- Integration Tests ---

const TestRoot = struct {
    pub fn visitEdges(self: *TestRoot, visitor: anytype) !void {
        _ = self;
        _ = visitor;

        // Do nothing.
    }
};

test Heap {
    std.testing.refAllDecls(Heap(TestRoot));
}

test "basic heap allocation" {
    const ArrayMap = @import("objects/array.zig").ArrayMap;
    const ArrayObject = @import("objects/array.zig").Array;
    const allocator = std.testing.allocator;

    var root: TestRoot = .{};

    var heap = try Heap(TestRoot).init(allocator, &root);
    defer heap.deinit();

    var handles: Heap(TestRoot).Handles = undefined;
    handles.init(&heap);
    defer handles.deinit(&heap);

    const required_memory = ArrayMap.requiredSizeForAllocation() + ArrayObject.requiredSizeForAllocation(0);
    var token = try heap.allocate(required_memory);
    defer token.deinit();

    const array_map = ArrayMap.create(&token, 0);
    var array = ArrayObject.createWithValues(&token, .Global, array_map, &.{}, null);
    const previous_array_address = array;
    // Root the object so it doesn't get collected.
    handles.trackObject(@ptrCast(&array));

    const stats = try heap.collect(.{});

    // Shouldn't have collected anything...
    try std.testing.expectEqual(0, stats.freed_bytes);
    // But should have still moved the object (including its transitive reference,
    // the slots map) to the survivor space.
    try std.testing.expectEqual(0, stats.existing_survived_bytes);
    try std.testing.expectEqual(required_memory, stats.new_survived_bytes);
    // Shouldn't have tenured anything because our survivor space isn't full
    // yet.
    try std.testing.expectEqual(0, stats.tenured_bytes);
    try std.testing.expectEqual(0, stats.old_compacted_bytes);

    // Array object should have moved to future (now past) survivor space.
    try std.testing.expect(array != previous_array_address);
    try std.testing.expectEqual(.PastSurvivor, heap.new_generation.referenceLocation(array.asValue().asReference().?));
}

test "spill to old space" {
    const ArrayMap = @import("objects/array.zig").ArrayMap;
    const ArrayObject = @import("objects/array.zig").Array;
    const ByteArray = @import("ByteArray.zig");
    const allocator = std.testing.allocator;

    var root: TestRoot = .{};

    var heap = try Heap(TestRoot).init(allocator, &root);
    defer heap.deinit();

    var handles: Heap(TestRoot).Handles = undefined;
    handles.init(&heap);
    defer handles.deinit(&heap);

    const map_required_memory = ArrayMap.requiredSizeForAllocation();
    const required_memory_per_object = ArrayObject.requiredSizeForAllocation(1);
    // Exactly one more object than can fit in the future survivor space.
    const object_count = @divFloor((heap.new_generation.future_survivor.len - map_required_memory), required_memory_per_object) + 1;

    var token = try heap.allocate(map_required_memory + (required_memory_per_object * object_count));
    defer token.deinit();

    const slot_name = try ByteArray.createFromString(allocator, "prev");
    defer slot_name.deinit(allocator);

    const array_map = ArrayMap.create(&token, 1);

    // Chain a whole bunch of objects together.

    var array = ArrayObject.createWithValues(&token, .Global, array_map, &.{Value.fromUnsignedInteger(0)}, null);
    array.getValues()[0] = array.asValue();

    for (1..object_count) |_| {
        const prev_array = array;
        array = ArrayObject.createWithValues(&token, .Global, array_map, &.{prev_array.asValue()}, null);
    }

    // Root the last object so the whole chain doesn't get collected.
    handles.trackObject(@ptrCast(&array));

    const stats = try heap.collect(.{});

    // Shouldn't have collected anything.
    try std.testing.expectEqual(0, stats.freed_bytes);
    // All objects must have survived.
    try std.testing.expectEqual(0, stats.existing_survived_bytes);
    try std.testing.expectEqual(map_required_memory + (required_memory_per_object * object_count), stats.new_survived_bytes);
    // Should have tenured exactly one slots object to the old generation.
    try std.testing.expectEqual(required_memory_per_object * 1, stats.tenured_bytes);
    try std.testing.expectEqual(0, stats.old_compacted_bytes);
}

test "past survivor space tenuring" {
    const ArrayMap = @import("objects/array.zig").ArrayMap;
    const ArrayObject = @import("objects/array.zig").Array;
    const allocator = std.testing.allocator;

    var root: TestRoot = .{};

    var heap = try Heap(TestRoot).init(allocator, &root);
    defer heap.deinit();

    var handles: Heap(TestRoot).Handles = undefined;
    handles.init(&heap);
    defer handles.deinit(&heap);

    // Fill up the heap with enough objects to trigger the tenuring threshold, but
    // not so much that we spill to the old generation.
    const map_required_memory = ArrayMap.requiredSizeForAllocation();
    const required_memory_per_object = ArrayObject.requiredSizeForAllocation(1);
    const object_count = @divFloor((((heap.new_generation.past_survivor.len - map_required_memory) * TENURE_THRESHOLD_PERCENT) / 100), required_memory_per_object) + 1;

    var token = try heap.allocate(map_required_memory + (required_memory_per_object * object_count));
    defer token.deinit();

    const array_map = ArrayMap.create(&token, 1);

    // Chain a whole bunch of objects together.
    var array = ArrayObject.createWithValues(&token, .Global, array_map, &.{Value.fromUnsignedInteger(0)}, null);

    for (1..object_count) |_| {
        const prev_array = array;
        array = ArrayObject.createWithValues(&token, .Global, array_map, &.{prev_array.asValue()}, null);
    }

    // Root the last object so the whole chain doesn't get collected.
    handles.trackObject(@ptrCast(&array));

    const stats = try heap.collect(.{});

    // For this first run, we shouldn't have freed or tenured anything; everything should
    // be in the past survivor space now.
    try std.testing.expectEqual(0, stats.freed_bytes);
    try std.testing.expectEqual(0, stats.existing_survived_bytes);
    try std.testing.expectEqual(map_required_memory + (required_memory_per_object * object_count), stats.new_survived_bytes);
    try std.testing.expectEqual(0, stats.tenured_bytes);
    try std.testing.expectEqual(0, stats.old_compacted_bytes);

    const stats2 = try heap.collect(.{});

    // Now, since we are above the tenuring threshold, we should have tenured all the objects
    // to the old generation.
    try std.testing.expectEqual(0, stats2.freed_bytes);
    try std.testing.expectEqual(map_required_memory + (required_memory_per_object * object_count), stats2.existing_survived_bytes);
    try std.testing.expectEqual(0, stats2.new_survived_bytes);
    try std.testing.expectEqual(map_required_memory + (required_memory_per_object * object_count), stats2.tenured_bytes);
    try std.testing.expectEqual(0, stats2.old_compacted_bytes);
}

test "finalization" {
    const managed = @import("objects/managed.zig");
    const ManagedObject = managed.Managed;
    const FileDescriptor = managed.FileDescriptor;
    const allocator = std.testing.allocator;

    var root: TestRoot = .{};

    var heap = try Heap(TestRoot).init(allocator, &root);
    defer heap.deinit();

    var handles: Heap(TestRoot).Handles = undefined;
    handles.init(&heap);
    // NOTE: deinit below

    // Just open some dummy file.
    // FIXME: This test will fail on non-POSIX. Find something cross-platform
    //        to open.
    const file = try std.posix.openZ("/dev/null", .{}, 0);
    const fd_object = FileDescriptor.adopt(file, .{ .close_during_finalization = true });

    const required_memory = ManagedObject.requiredSizeForAllocation();
    var token = try heap.allocate(required_memory);
    defer token.deinit();

    var object = try ManagedObject.create(&heap, &token, .Global, .FileDescriptor, fd_object.toValue());

    // Root the object so it doesn't get collected.
    handles.trackObject(@ptrCast(&object));

    // First, collect to make sure the object's finalizer isn't run while it's still
    // alive.
    const stats1 = try heap.collect(.{});

    try std.testing.expectEqual(0, stats1.freed_bytes);
    try std.testing.expectEqual(0, stats1.existing_survived_bytes);
    try std.testing.expectEqual(required_memory, stats1.new_survived_bytes);
    try std.testing.expectEqual(0, stats1.tenured_bytes);
    try std.testing.expectEqual(0, stats1.old_compacted_bytes);

    // Deinitialize the handle set so the object can be collected.
    handles.deinit(&heap);

    // Now, collect again to make sure the finalizer is run.
    const stats2 = try heap.collect(.{});

    try std.testing.expectEqual(required_memory, stats2.freed_bytes);
    try std.testing.expectEqual(0, stats2.existing_survived_bytes);
    try std.testing.expectEqual(0, stats2.new_survived_bytes);
    try std.testing.expectEqual(0, stats2.tenured_bytes);
    try std.testing.expectEqual(0, stats2.old_compacted_bytes);

    // The finalizer should have run, which means the file descriptor should have been closed.
    var buffer: [1]u8 = undefined;
    _ = std.posix.read(file, buffer[0..]) catch |err| {
        try std.testing.expectEqual(error.NotOpenForReading, err);
    };
}

test "major garbage collection" {
    const ArrayMap = @import("objects/array.zig").ArrayMap;
    const ArrayObject = @import("objects/array.zig").Array;
    const allocator = std.testing.allocator;

    var root: TestRoot = .{};

    var heap = try Heap(TestRoot).init(allocator, &root);
    defer heap.deinit();

    var handles: Heap(TestRoot).Handles = undefined;
    handles.init(&heap);
    // NOTE: deinit below

    const map_required_memory = ArrayMap.requiredSizeForAllocation();
    const array_required_memory = ArrayObject.requiredSizeForAllocation(0);

    // Allocate one object directly into the old generation.
    const map: ArrayMap.Ptr = @ptrCast(try heap.old_generation.allocate(heap.allocator, map_required_memory));
    map.init(0);

    // Allocate the array that uses the map into the new generation.
    var token = try heap.allocate(array_required_memory);
    var array = ArrayObject.createWithValues(&token, .Global, map, &.{}, null);

    // Hold onto the array.
    handles.trackObject(@ptrCast(&array));

    // Force a major+minor garbage collection. This should not clear anything.
    const stats1 = try heap.collect(.{ .major = .Force });
    try std.testing.expectEqual(0, stats1.freed_bytes);
    try std.testing.expectEqual(0, stats1.existing_survived_bytes);
    try std.testing.expectEqual(array_required_memory, stats1.new_survived_bytes);
    try std.testing.expectEqual(0, stats1.tenured_bytes);
    try std.testing.expectEqual(0, stats1.old_compacted_bytes);

    // Deinitialize the handle set so there are no more roots to the array.
    handles.deinit(&heap);

    // Force one *minor*, and one *major* collection. The former is to remove
    // the array->map reference, and the latter is to remove the map itself.
    const stats2 = try heap.collect(.{ .minor = .Force, .major = .Disable });
    try std.testing.expectEqual(array_required_memory, stats2.freed_bytes);
    try std.testing.expectEqual(0, stats2.new_survived_bytes);
    try std.testing.expectEqual(0, stats2.existing_survived_bytes);
    try std.testing.expectEqual(0, stats2.tenured_bytes);
    try std.testing.expectEqual(0, stats2.old_compacted_bytes);

    const stats3 = try heap.collect(.{ .minor = .Disable, .major = .Force });
    try std.testing.expectEqual(map_required_memory, stats3.freed_bytes);
    try std.testing.expectEqual(0, stats3.new_survived_bytes);
    try std.testing.expectEqual(0, stats3.existing_survived_bytes);
    try std.testing.expectEqual(0, stats3.tenured_bytes);
    try std.testing.expectEqual(map_required_memory, stats3.old_compacted_bytes);
}

test "remembered set" {
    const ArrayMap = @import("objects/array.zig").ArrayMap;
    const ArrayObject = @import("objects/array.zig").Array;
    const allocator = std.testing.allocator;

    var root: TestRoot = .{};

    var heap = try Heap(TestRoot).init(allocator, &root);
    defer heap.deinit();

    var handles: Heap(TestRoot).Handles = undefined;
    handles.init(&heap);
    defer handles.deinit(&heap);

    const map_required_memory = ArrayMap.requiredSizeForAllocation();
    const array_required_memory = ArrayObject.requiredSizeForAllocation(0);

    // Place a random object into the old generation (used for testing whether
    // remembered set reconstruction is successful). This will be cleared with
    // the first major GC.
    const unused_map: ArrayMap.Ptr = @ptrCast(try heap.old_generation.allocate(heap.allocator, map_required_memory));
    unused_map.init(0);

    // Place the actually used map in new generation. The array object will point
    // to it.
    const used_map: ArrayMap.Ptr = @ptrCast(try heap.new_generation.allocate(map_required_memory));
    used_map.init(0);

    // Place the array itself in the old generation.
    var array: ArrayObject.Ptr = @ptrCast(try heap.old_generation.allocate(heap.allocator, array_required_memory));
    array.init(.Global, used_map, &.{}, null);
    // Remember the address since it will point to the new generation.
    const did_remember = heap.old_generation.rememberAddress(array.asValue().asReference().?.getAddress());
    try std.testing.expect(did_remember);

    // First perform a minor collection. At the end of this collection, the
    // array map should still be alive because the array in the old generation
    // is pointing to it.
    const stats1 = try heap.collect(.{ .minor = .Force, .major = .Disable });
    try std.testing.expectEqual(0, stats1.freed_bytes);
    try std.testing.expectEqual(0, stats1.existing_survived_bytes);
    try std.testing.expectEqual(array_required_memory, stats1.new_survived_bytes);
    try std.testing.expectEqual(0, stats1.tenured_bytes);
    try std.testing.expectEqual(0, stats1.old_compacted_bytes);

    // XXX: Beyond this point used_map is invalid.

    // Check whether the map moved successfully to past survivor.
    const new_used_map = array.getMap();
    const new_used_map_ref = new_used_map.asValue().asReference().?;
    try std.testing.expectEqual(.PastSurvivor, heap.new_generation.referenceLocation(new_used_map_ref));

    // Ensure that the card offset for the first card is *not* 0 (since we
    // allocated an object before the array).
    var card = heap.old_generation.first_region.?.remembered_set.cards[0];
    try std.testing.expect(card != .Empty);
    try std.testing.expect(card.wordOffset() != 0);

    // Hold onto the array so that it doesn't go away.
    handles.trackObject(@ptrCast(&array));

    // Now perform a major GC. This will:
    // - Compact away unused_map.
    // - Move array to a new location.
    // - Update the remembered set so that the first card now has index 0.
    const stats2 = try heap.collect(.{ .minor = .Disable, .major = .Force });
    try std.testing.expectEqual(map_required_memory, stats2.freed_bytes);
    try std.testing.expectEqual(0, stats2.existing_survived_bytes); // 0 because no minor was performed
    try std.testing.expectEqual(0, stats2.new_survived_bytes);
    try std.testing.expectEqual(0, stats2.tenured_bytes);
    try std.testing.expectEqual(map_required_memory, stats2.old_compacted_bytes);

    // XXX: Beyond this point unused_map is dead and array is invalid.

    // Ensure that the card offset is now 0.
    card = heap.old_generation.first_region.?.remembered_set.cards[0];
    try std.testing.expect(card != .Empty);
    try std.testing.expectEqual(0, card.wordOffset());

    handles.deinit(&heap);

    // Now, perform one final minor GC. Nothing should be collected.
    const stats3 = try heap.collect(.{ .minor = .Force, .major = .Disable });
    try std.testing.expectEqual(0, stats3.freed_bytes);
    try std.testing.expectEqual(map_required_memory, stats3.existing_survived_bytes);
    try std.testing.expectEqual(0, stats3.new_survived_bytes);
    try std.testing.expectEqual(0, stats3.tenured_bytes);
    try std.testing.expectEqual(0, stats3.old_compacted_bytes);
}

test "handle set updates pointers correctly" {
    const ArrayMap = @import("objects/array.zig").ArrayMap;
    const allocator = std.testing.allocator;

    var root: TestRoot = .{};

    var heap = try Heap(TestRoot).init(allocator, &root);
    defer heap.deinit();

    var handles: Heap(TestRoot).Handles = undefined;
    handles.init(&heap);
    defer handles.deinit(&heap);

    const required_memory = ArrayMap.requiredSizeForAllocation();

    var token = try heap.allocate(required_memory);
    defer token.deinit();

    var array_map = ArrayMap.create(&token, 0);
    handles.trackObject(@ptrCast(&array_map));
    var array_map_value = array_map.asValue();
    handles.trackValue(&array_map_value);

    const array_map_original = array_map;
    const array_map_value_original = array_map_value;

    // Force a minor collection, which will move the array map to a new location.
    const stats = try heap.collect(.{ .minor = .Force, .major = .Disable });

    // Since we're holding onto the array map, it should not have been collected.
    try std.testing.expectEqual(0, stats.freed_bytes);
    try std.testing.expectEqual(0, stats.existing_survived_bytes);
    try std.testing.expectEqual(required_memory, stats.new_survived_bytes);
    try std.testing.expectEqual(0, stats.tenured_bytes);
    try std.testing.expectEqual(0, stats.old_compacted_bytes);

    // The array map should have moved to a new location.
    try std.testing.expect(array_map != array_map_original);
    try std.testing.expect(array_map_value.data != array_map_value_original.data);

    // Both the value and the object pointer should point to the same location.
    try std.testing.expect(array_map.asValue().asReference().?.getAddress() == array_map_value.asReference().?.getAddress());
}
