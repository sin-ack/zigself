// Copyright (c) 2021-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("../map.zig").Map;
const debug = @import("../../debug.zig");
const Value = value_import.Value;
const MapType = @import("../map.zig").MapType;
const bytecode = @import("../bytecode.zig");
const SlotsMap = @import("slots.zig").SlotsMap;
const ValueSlot = @import("../object_lookup.zig").ValueSlot;
const value_import = @import("../value.zig");
const LookupTarget = @import("../object_lookup.zig").LookupTarget;
const PointerValue = value_import.PointerValue;
const pointer = @import("../../utility/pointer.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const RefCountedValue = value_import.RefCountedValue;
const InlineCacheEntry = @import("../inline_cache.zig").InlineCacheEntry;

const INLINE_CACHE_DEBUG = debug.INLINE_CACHE_DEBUG;

/// An "executable map" is one that contains a reference to executable code.
/// They have a VM-intrinsic way to activate them; for instance, methods are
/// activated by sending the message with the same name to an object that has it
/// in its lookup chain, and blocks are activated by sending them the correct
/// `value:With:...` method.
pub const ExecutableMap = extern struct {
    slots: SlotsMap align(@alignOf(u64)),
    /// The address of the bytecode block. Owned by definition_executable_ref.
    block: PointerValue(bytecode.Block) align(@alignOf(u64)),
    /// The executable which this map was created from.
    definition_executable_ref: RefCountedValue(bytecode.Executable) align(@alignOf(u64)),
    /// A monomorphic inline cache, containing an entry for every send
    /// instruction in the bytecode block. Contains exactly `block.send_count`
    /// entries.
    inline_cache: [*]InlineCacheEntry align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(ExecutableMap, .Mutable);

    pub const ExecutableInformation = packed struct(u8) { argument_slot_count: u8 };
    pub const ExtraBits = Map.ExtraBits.reserve(ExecutableInformation);

    /// Refs `script`.
    pub fn init(
        self: ExecutableMap.Ptr,
        comptime map_type: MapType,
        argument_slot_count: u8,
        total_slot_count: u16,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
        inline_cache: []InlineCacheEntry,
    ) void {
        self.slots.init(total_slot_count);
        // FIXME: We're manually overriding the map type here. This is a hack
        //        because the initialization of SlotsMap's own fields and the
        //        base map initialization are currently entangled.
        self.slots.map.getMetadata().type = map_type;
        self.setArgumentSlotCount(argument_slot_count);

        self.block = PointerValue(bytecode.Block).init(block);
        self.definition_executable_ref = RefCountedValue(bytecode.Executable).init(executable);
        self.inline_cache = inline_cache.ptr;
    }

    /// Finalizes this object. All maps that have a SlotsAndBytecodeMap member
    /// must call this function in their finalize.
    pub fn finalize(self: ExecutableMap.Ptr, allocator: Allocator) void {
        allocator.free(self.inline_cache[0..self.block.get().send_count]);
        self.definition_executable_ref.deinit();
    }

    /// Visit the edges of this object.
    pub fn visitEdges(
        self: ExecutableMap.Ptr,
        visitor: anytype,
    ) !void {
        for (self.inline_cache[0..self.block.get().send_count]) |*entry| {
            try entry.visitEdges(visitor, @ptrCast(self));
        }
    }

    pub fn getArgumentSlotCount(self: ExecutableMap.Ptr) u8 {
        return ExecutableMap.ExtraBits.read(self.slots.map.getMetadata().*).argument_slot_count;
    }

    fn setArgumentSlotCount(self: ExecutableMap.Ptr, count: u8) void {
        ExecutableMap.ExtraBits.write(self.slots.map.getMetadata(), .{ .argument_slot_count = count });
    }

    // --- Inline cache operations ---

    /// Return a `ValueSlot` if the given `receiver` matches the inline cache
    /// entry at `send_index` for this executable map, or `null` otherwise.
    pub fn getCachedValueSlot(
        self: ExecutableMap.Ptr,
        vm: *const VirtualMachine,
        send_index: u32,
        receiver: Value,
    ) ?ValueSlot {
        const map = receiver.getMapForCaching(vm) orelse return null;
        const result = self.inline_cache[send_index].getValueSlot(map, receiver);
        if (INLINE_CACHE_DEBUG) std.debug.print("ExecutableMap.getCachedValueSlot: send_index={} map={*} hit? {}\n", .{ send_index, map, result != null });
        return result;
    }

    /// Cache the given `LookupTarget` on the inline cache entry at `send_index`
    /// for this executable map, using `receiver`'s map for caching.
    pub fn cacheLookupTarget(
        self: ExecutableMap.Ptr,
        vm: *const VirtualMachine,
        send_index: u32,
        receiver: Value,
        target: LookupTarget,
    ) void {
        const map = receiver.getMapForCaching(vm) orelse return;
        if (INLINE_CACHE_DEBUG) {
            std.debug.print("ExecutableMap.cacheLookupTarget: Storing lookup target {} for send_index={} map={*}\n", .{
                target,
                send_index,
                map,
            });
        }
        self.inline_cache[send_index].write(map, receiver, target);
    }
};
