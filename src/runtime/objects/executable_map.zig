// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("../Heap.zig");
const Value = value_import.Value;
const Object = @import("../object.zig").Object;
const MapType = @import("map.zig").MapType;
const bytecode = @import("../bytecode.zig");
const SlotsMap = @import("slots.zig").SlotsMap;
const ArrayMap = @import("./array.zig").ArrayMap;
const ArrayObject = @import("./array.zig").Array;
const MethodObject = @import("./method.zig").Method;
const value_import = @import("../value.zig");
const PointerValue = value_import.PointerValue;
const stage2_compat = @import("../../utility/stage2_compat.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const RefCountedValue = value_import.RefCountedValue;

/// An "executable map" is one that contains a reference to executable code.
/// They have a VM-intrinsic way to activate them; for instance, methods are
/// activated by sending the message with the same name to an object that has it
/// in its lookup chain, and blocks are activated by sending them the correct
/// `value:With:...` method.
pub const ExecutableMap = extern struct {
    slots: SlotsMap align(@alignOf(u64)),
    /// The address of the bytecode block. Owned by definition_executable_ref.
    block: PointerValue(bytecode.Block) align(@alignOf(u64)),
    /// An inline cache twice the length of `block`. The items form pairs of
    /// [receiver map reference, method reference]. When the receiver map
    /// reference matches what's in the cache, we directly use the method
    /// instead of performing a lookup.
    inline_cache: ArrayObject.Value,
    /// The executable which this map was created from.
    definition_executable_ref: RefCountedValue(bytecode.Executable) align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(ExecutableMap, .Mutable);

    pub const ExecutableInformation = packed struct(u16) {
        argument_slot_count: u8,
        padding: u8 = 0,
    };

    /// Refs `script`.
    pub fn allocateAndInit(
        self: ExecutableMap.Ptr,
        vm: *VirtualMachine,
        token: *Heap.AllocationToken,
        comptime map_type: MapType,
        argument_slot_count: u8,
        total_slot_count: u32,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) void {
        const map_map = vm.getMapMap();

        const inline_cache_size = block.getLength() * 2;
        const inline_cache_map = ArrayMap.create(map_map, token, inline_cache_size);
        // TODO: Use GlobalActorID!
        const inline_cache = ArrayObject.createWithValues(token, 0, inline_cache_map, &.{}, vm.nil());

        self.init(
            map_type,
            map_map,
            argument_slot_count,
            total_slot_count,
            block,
            ArrayObject.Value.init(inline_cache),
            executable,
        );
    }

    fn init(
        self: ExecutableMap.Ptr,
        comptime map_type: MapType,
        map_map: Map.Ptr,
        argument_slot_count: u8,
        total_slot_count: u32,
        block: *bytecode.Block,
        inline_cache: ArrayObject.Value,
        executable: bytecode.Executable.Ref,
    ) void {
        std.debug.assert(argument_slot_count <= total_slot_count);

        self.slots.init(total_slot_count, map_map);
        self.slots.map.init(map_type, map_map);
        self.setArgumentSlotCount(argument_slot_count);

        self.block = PointerValue(bytecode.Block).init(block);
        self.inline_cache = inline_cache;
        self.definition_executable_ref = RefCountedValue(bytecode.Executable).init(executable);
    }

    /// Finalizes this object. All maps that have a SlotsAndBytecodeMap member
    /// must call this function in their finalize.
    pub fn finalize(self: ExecutableMap.Ptr, allocator: Allocator) void {
        _ = allocator;
        self.definition_executable_ref.deinit();
    }

    pub fn getArgumentSlotCount(self: ExecutableMap.Ptr) u8 {
        return @ptrCast(*ExecutableInformation, &self.slots.information.extra).argument_slot_count;
    }

    fn setArgumentSlotCount(self: ExecutableMap.Ptr, count: u8) void {
        @ptrCast(*ExecutableInformation, &self.slots.information.extra).argument_slot_count = count;
    }

    pub fn requiredSizeForAllocation(block: *bytecode.Block) usize {
        // Since we will be allocating an array as well as its map, we need to
        // include both of those in our required size calculation.
        var required_size = ArrayMap.requiredSizeForAllocation();
        required_size += ArrayObject.requiredSizeForAllocation(block.getLength() * 2);
        return required_size;
    }

    // --- Inline cache operations ---

    pub fn writeIntoInlineCacheAtOffset(self: ExecutableMap.Ptr, offset: usize, object: Object.Ptr, method: MethodObject.Ptr) void {
        const inline_cache = self.inline_cache.get();
        std.debug.assert(offset < inline_cache.getSize() / 2);

        const inline_cache_array = self.inline_cache.get().getValues();
        inline_cache_array[offset * 2] = object.map;
        inline_cache_array[(offset * 2) + 1] = method.asValue();
    }
};
