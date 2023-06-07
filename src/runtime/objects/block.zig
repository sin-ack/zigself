// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Slot = @import("../slot.zig").Slot;
const Heap = @import("../Heap.zig");
const debug = @import("../../debug.zig");
const slots = @import("slots.zig");
const Value = value_import.Value;
const Object = @import("../object.zig").Object;
const bytecode = @import("../bytecode.zig");
const Activation = @import("../Activation.zig");
const SlotsObject = slots.Slots;
const SourceRange = @import("../SourceRange.zig");
const MethodObject = @import("method.zig").Method;
const value_import = @import("../value.zig");
const ExecutableMap = @import("executable_map.zig").ExecutableMap;
const stage2_compat = @import("../../utility/stage2_compat.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationObject = @import("activation.zig").Activation;
const SlotsLikeMapBase = slots.SlotsLikeMapBase;
const SlotsLikeObjectBase = slots.SlotsLikeObjectBase;
const AssignableSlotsMixin = slots.AssignableSlotsMixin;

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// A block object is an executable object which can be defined in a method and
/// then executed later. The block must be executed while the method in which it
/// is created is still on the activation stack.
pub const Block = extern struct {
    slots: SlotsObject align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(Block, .Mutable);

    pub usingnamespace SlotsLikeObjectBase(Block);
    pub usingnamespace AssignableSlotsMixin(Block);

    pub fn create(token: *Heap.AllocationToken, actor_id: u31, map: BlockMap.Ptr, assignable_slot_values: []const Value) Block.Ptr {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = Block.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(Block.Ptr, memory_area);
        self.init(actor_id, map);
        @memcpy(self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: Block.Ptr, actor_id: u31, map: BlockMap.Ptr) void {
        self.slots.object = .{
            .object_information = .{
                .object_type = .Block,
                .actor_id = actor_id,
            },
            .map = map.asValue(),
        };
    }

    pub fn getMap(self: Block.Ptr) BlockMap.Ptr {
        return self.slots.object.getMap().mustBeType(.Block);
    }

    pub fn getSlots(self: Block.Ptr) Slot.Slice {
        return self.getMap().getSlots();
    }

    pub fn canFinalize(self: Block.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: Block.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call Block.finalize");
    }

    pub fn lookup(self: Block.Ptr, vm: *VirtualMachine, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        // NOTE: executeMessage will handle the execution of the block itself.
        _ = self;
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("Block.lookup: Looking at traits block\n", .{});
        const block_traits = vm.block_traits.getValue();
        if (selector_hash.regular == object_lookup.parent_hash)
            return object_lookup.LookupResult{ .Regular = block_traits };

        return block_traits.lookupByHash(vm, selector_hash);
    }

    // --- Slot counts ---

    pub fn getArgumentSlotCount(self: Block.Ptr) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: Block.Ptr) u8 {
        return self.getMap().getAssignableSlotCount();
    }

    // --- Activation ---

    /// Returns whether the passed message name is the correct one for this block
    /// to be executed. The logic is:
    ///
    /// - For blocks with no arguments, `value`
    /// - For blocks with a single argument, `value:`
    /// - For blocks with more than one argument, `value:With:`, with as many
    ///   `With:`s as needed (number of colons should match number of arguments)
    pub fn isCorrectMessageForBlockExecution(self: Block.Ptr, message: []const u8) bool {
        if (self.getArgumentSlotCount() == 0) {
            return std.mem.eql(u8, message, "value");
        }

        if (message.len < 6 or !std.mem.eql(u8, message[0..6], "value:")) {
            return false;
        }

        var remaining_message = message[6..];
        var remaining_arguments = self.getArgumentSlotCount() - 1;
        while (remaining_arguments > 0) : (remaining_arguments -= 1) {
            if (remaining_message.len == 0)
                return false;

            var with_slice = remaining_message[0..5];
            if (!std.mem.eql(u8, with_slice, "With:"))
                return false;

            remaining_message = remaining_message[5..];
        }

        return remaining_message.len == 0;
    }

    /// Creates a block activation object for this block and returns it. Copies
    /// `source_range`.
    pub fn activateBlock(
        self: Block.Ptr,
        vm: *VirtualMachine,
        token: *Heap.AllocationToken,
        receiver: Value,
        arguments: []const Value,
        target_location: bytecode.RegisterLocation,
        creator_message: Value,
        created_from: SourceRange,
        out_activation: *Activation,
    ) void {
        const activation_object = ActivationObject.create(token, vm.current_actor.id, .Block, self.slots.object.getMap(), arguments, self.getAssignableSlots(), receiver);

        out_activation.initInPlace(ActivationObject.Value.init(activation_object), target_location, vm.takeStackSnapshot(), creator_message, created_from);
        out_activation.parent_activation = self.getMap().parent_activation;
        out_activation.nonlocal_return_target_activation = self.getMap().nonlocal_return_target_activation;
    }
};

/// A map for a block object.
pub const BlockMap = extern struct {
    base_map: ExecutableMap align(@alignOf(u64)),
    /// A weak reference to the parent activation of this block. The block must
    /// not be activated if this activation has left the stack.
    parent_activation: Activation.ActivationRef align(@alignOf(u64)),
    /// A weak reference to the non-local return target activation of this
    /// block. If a non-local return happens inside this block, then it will
    /// target this activation.
    nonlocal_return_target_activation: Activation.ActivationRef align(@alignOf(u64)),

    pub const ObjectType = Block;
    pub const Ptr = stage2_compat.HeapPtr(BlockMap, .Mutable);

    pub usingnamespace SlotsLikeMapBase(BlockMap);

    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    pub fn create(
        vm: *VirtualMachine,
        token: *Heap.AllocationToken,
        argument_slot_count: u8,
        total_slot_count: u32,
        parent_activation: Activation.ActivationRef,
        nonlocal_return_target_activation: Activation.ActivationRef,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) !BlockMap.Ptr {
        const size = BlockMap.requiredSizeForSelfAllocation(total_slot_count);

        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(BlockMap.Ptr, memory_area);
        self.init(vm, token, argument_slot_count, total_slot_count, parent_activation, nonlocal_return_target_activation, block, executable);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(
        self: BlockMap.Ptr,
        vm: *VirtualMachine,
        token: *Heap.AllocationToken,
        argument_slot_count: u8,
        total_slot_count: u32,
        parent_activation: Activation.ActivationRef,
        nonlocal_return_target_activation: Activation.ActivationRef,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) void {
        self.base_map.allocateAndInit(vm, token, .Block, argument_slot_count, total_slot_count, block, executable);
        self.parent_activation = parent_activation;
        self.nonlocal_return_target_activation = nonlocal_return_target_activation;
    }

    pub fn canFinalize(self: BlockMap.Ptr) bool {
        _ = self;
        return true;
    }

    pub fn finalize(self: BlockMap.Ptr, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    pub fn getArgumentSlotCount(self: BlockMap.Ptr) u8 {
        return self.base_map.getArgumentSlotCount();
    }

    pub fn clone(self: BlockMap.Ptr, vm: *VirtualMachine, token: *Heap.AllocationToken) !BlockMap.Ptr {
        const new_map = try create(
            vm,
            token,
            self.getArgumentSlotCount(),
            self.base_map.slots.information.slot_count,
            self.parent_activation,
            self.nonlocal_return_target_activation,
            self.base_map.block.get(),
            self.base_map.definition_executable_ref.get(),
        );

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        @memcpy(new_map.getSlots(), self.getSlots());

        return new_map;
    }

    pub fn getSizeInMemory(self: BlockMap.Ptr) usize {
        return requiredSizeForSelfAllocation(self.base_map.slots.information.slot_count);
    }

    pub fn getSizeForCloning(self: BlockMap.Ptr) usize {
        return requiredSizeForAllocation(self.base_map.block.get(), self.base_map.slots.information.slot_count);
    }

    /// Return the size required for allocating just the map itself.
    pub fn requiredSizeForSelfAllocation(slot_count: u32) usize {
        return @sizeOf(BlockMap) + slot_count * @sizeOf(Slot);
    }

    pub fn requiredSizeForAllocation(bytecode_block: *bytecode.Block, slot_count: u32) usize {
        var required_memory = requiredSizeForSelfAllocation(slot_count);
        required_memory += ExecutableMap.requiredSizeForAllocation(bytecode_block);
        return required_memory;
    }

    pub fn getOrInvalidateMethodFromInlineCacheAtOffsetForReceiver(self: BlockMap.Ptr, vm: *VirtualMachine, offset: usize, receiver: Object.Ptr) ?MethodObject.Ptr {
        return self.base_map.getOrInvalidateMethodFromInlineCacheAtOffsetForReceiver(vm, offset, receiver);
    }

    pub fn writeIntoInlineCacheAtOffset(self: BlockMap.Ptr, vm: *VirtualMachine, offset: usize, object: Object.Ptr, method: MethodObject.Ptr) void {
        self.base_map.writeIntoInlineCacheAtOffset(vm, offset, object, method);
    }
};
