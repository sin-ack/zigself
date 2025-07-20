// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("../map.zig").Map;
const Slot = @import("../slot.zig").Slot;
const heap_import = @import("../Heap.zig");
const Actor = @import("../Actor.zig");
const Value = value_import.Value;
const debug = @import("../../debug.zig");
const slots = @import("slots.zig");
const Object = @import("../object.zig").Object;
const pointer = @import("../../utility/pointer.zig");
const context = @import("../context.zig");
const Selector = @import("../Selector.zig");
const bytecode = @import("../bytecode.zig");
const ByteArray = @import("../ByteArray.zig");
const ValueSlot = @import("../object_lookup.zig").ValueSlot;
const Activation = @import("../Activation.zig");
const SlotsObject = slots.Slots;
const SourceRange = @import("../SourceRange.zig");
const LookupResult = @import("../object_lookup.zig").LookupResult;
const value_import = @import("../value.zig");
const ExecutableMap = @import("executable_map.zig").ExecutableMap;
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationObject = @import("activation.zig").Activation;
const InlineCacheEntry = @import("../inline_cache.zig").InlineCacheEntry;
const SlotsLikeMapBase = slots.SlotsLikeMapBase;
const SlotsLikeObjectBase = slots.SlotsLikeObjectBase;
const AssignableSlotsMixin = slots.AssignableSlotsMixin;

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// A block object is an executable object which can be defined in a method and
/// then executed later. The block must be executed while the method in which it
/// is created is still on the activation stack.
pub const Block = extern struct {
    slots: SlotsObject align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Block, .Mutable);

    pub usingnamespace SlotsLikeObjectBase(Block);
    pub usingnamespace AssignableSlotsMixin(Block);

    /// Create a block object with the given block map. Allocates enough space
    /// for assignable slots.
    ///
    /// All assignable slots *must* be initialized right after creation.
    pub fn create(token: *heap_import.AllocationToken, actor_id: Actor.ActorID, map: BlockMap.Ptr) Block.Ptr {
        const size = Block.requiredSizeForAllocation(map.getAssignableSlotCount());

        const memory_area = token.allocate(size);
        var self: Block.Ptr = @ptrCast(memory_area);
        self.init(actor_id, map);

        return self;
    }

    fn init(self: Block.Ptr, actor_id: Actor.ActorID, map: BlockMap.Ptr) void {
        self.slots.object.init(.Block, actor_id, map.asValue());
    }

    pub fn getMap(self: Block.Ptr) BlockMap.Ptr {
        return self.slots.object.getMap().unsafeAsType(.Block);
    }

    pub fn getMapForCaching(self: Block.Ptr, vm: *const VirtualMachine) ?Map.Ptr {
        _ = vm;
        return @ptrCast(self.getMap());
    }

    pub fn getSlots(self: Block.Ptr) Slot.Slice {
        return self.getMap().getSlots();
    }

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: Block.Ptr, visitor: anytype) !void {
        try self.slots.object.visitEdges(visitor);
        try self.visitAssignableSlotValues(visitor);
    }

    /// Return a shallow copy of this object.
    pub fn clone(self: Block.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) Block.Ptr {
        _ = allocator;
        _ = heap;

        const new_block = create(token, actor_id, self.getMap());
        @memcpy(new_block.getAssignableSlots(), self.getAssignableSlots());

        return new_block;
    }

    // --- Well-known value slots ---
    const VALUE_SLOT_PARENT = 0;

    pub fn lookup(self: Block.Ptr, selector: Selector, previously_visited: ?*const Selector.VisitedValueLink) LookupResult {
        // NOTE: executeMessage will handle the execution of the block itself.
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("Block.lookup: Looking at traits block\n", .{});
        if (selector.equals(Selector.well_known.parent))
            return .{ .Found = .{
                .object = @ptrCast(self),
                .value_slot = self.getValueSlot(VALUE_SLOT_PARENT),
                .value_slot_index = VALUE_SLOT_PARENT,
            } };

        const block_traits = context.getVM().block_traits;
        return block_traits.lookup(selector);
    }

    /// Get the value slot at the given index.
    pub fn getValueSlot(self: Block.Ptr, index: usize) ValueSlot {
        _ = self;

        switch (index) {
            VALUE_SLOT_PARENT => return .{ .Constant = context.getVM().block_traits },
            else => unreachable,
        }
    }

    // --- Slot counts ---

    pub fn getArgumentSlotCount(self: Block.Ptr) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: Block.Ptr) u15 {
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

            const with_slice = remaining_message[0..5];
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
        token: *heap_import.AllocationToken,
        receiver: Value,
        arguments: []const Value,
        target_location: bytecode.RegisterLocation,
        creator_message: ByteArray,
        created_from: SourceRange,
        out_activation: *Activation,
    ) void {
        const activation_object = ActivationObject.create(token, context.getActor().id, .Block, self.slots.object.getMap(), arguments, self.getAssignableSlots(), receiver);

        out_activation.initInPlace(ActivationObject.Value.init(activation_object), target_location, creator_message, created_from);
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
    pub const Ptr = pointer.HeapPtr(BlockMap, .Mutable);

    pub usingnamespace SlotsLikeMapBase(BlockMap);

    /// Create a new block map.
    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    /// All slots *must* be initialized right after creation.
    pub fn create(
        allocator: Allocator,
        heap: *VirtualMachine.Heap,
        token: *heap_import.AllocationToken,
        slot_count: u16,
        assignable_slot_count: u15,
        argument_slot_count: u8,
        parent_activation: Activation.ActivationRef,
        nonlocal_return_target_activation: Activation.ActivationRef,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) !BlockMap.Ptr {
        const size = BlockMap.requiredSizeForAllocation(slot_count);

        const inline_cache = try block.allocateInlineCache(allocator);
        errdefer allocator.free(inline_cache);

        const memory_area = token.allocate(size);
        try heap.markAddressAsNeedingFinalization(memory_area);

        var self: BlockMap.Ptr = @ptrCast(memory_area);
        self.init(slot_count, assignable_slot_count, argument_slot_count, parent_activation, nonlocal_return_target_activation, block, executable, inline_cache);
        return self;
    }

    fn init(
        self: BlockMap.Ptr,
        slot_count: u16,
        assignable_slot_count: u15,
        argument_slot_count: u8,
        parent_activation: Activation.ActivationRef,
        nonlocal_return_target_activation: Activation.ActivationRef,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
        inline_cache: []InlineCacheEntry,
    ) void {
        self.base_map.init(.Block, argument_slot_count, slot_count, block, executable, inline_cache);
        self.parent_activation = parent_activation;
        self.nonlocal_return_target_activation = nonlocal_return_target_activation;
        self.setAssignableSlotCount(assignable_slot_count);
    }

    pub fn finalize(self: BlockMap.Ptr, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: BlockMap.Ptr, visitor: anytype) !void {
        try self.base_map.visitEdges(visitor);
        try self.visitSlots(visitor);
    }

    pub fn getArgumentSlotCount(self: BlockMap.Ptr) u8 {
        return self.base_map.getArgumentSlotCount();
    }

    pub fn clone(self: BlockMap.Ptr, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken) !BlockMap.Ptr {
        const new_map = try create(
            // FIXME: Pass vm into clone!
            context.getVM().allocator,
            heap,
            token,
            self.getSlotCount(),
            self.getAssignableSlotCount(),
            self.getArgumentSlotCount(),
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
        return requiredSizeForAllocation(self.getSlotCount());
    }

    pub fn getSizeForCloning(self: BlockMap.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation(slot_count: u16) usize {
        return @sizeOf(BlockMap) + slot_count * @sizeOf(Slot);
    }
};
