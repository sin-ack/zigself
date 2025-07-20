// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("../map.zig").Map;
const Slot = @import("../slot.zig").Slot;
const Actor = @import("../Actor.zig");
const slots = @import("slots.zig");
const context = @import("../context.zig");
const pointer = @import("../../utility/pointer.zig");
const Selector = @import("../Selector.zig");
const bytecode = @import("../bytecode.zig");
const MapSlots = slots.MapSlots;
const ByteArray = @import("byte_array.zig").ByteArray;
const Activation = @import("../Activation.zig");
const SlotsObject = slots.Slots;
const heap_import = @import("../Heap.zig");
const ObjectValue = value_import.ObjectValue;
const SourceRange = @import("../SourceRange.zig");
const GenericValue = value_import.Value;
const value_import = @import("../value.zig");
const ExecutableMap = @import("executable_map.zig").ExecutableMap;
const VirtualMachine = @import("../VirtualMachine.zig");
const AssignableSlots = slots.AssignableSlots;
const ActivationObject = @import("activation.zig").Activation;
const InlineCacheEntry = @import("../inline_cache.zig").InlineCacheEntry;

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = extern struct {
    slots: SlotsObject align(@alignOf(u64)),
    assignable_slots: AssignableSlots(Method) align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Method, .Mutable);
    pub const Type = .Method;
    pub const Value = value_import.ObjectValue(Method);

    /// Create a method object with the given method map. Allocates enough
    /// space for assignable slots.
    ///
    /// All assignable slots *must* be initialized right after creation.
    pub fn create(token: *heap_import.AllocationToken, actor_id: Actor.ActorID, map: MethodMap.Ptr) Method.Ptr {
        const size = Method.requiredSizeForAllocation(map.getAssignableSlotCount());

        const memory_area = token.allocate(size);
        var self: Method.Ptr = @ptrCast(memory_area);
        self.init(actor_id, map);

        return self;
    }

    fn init(self: Method.Ptr, actor_id: Actor.ActorID, map: MethodMap.Ptr) void {
        self.slots.object.init(.Method, actor_id, map.asValue());
    }

    pub fn getMap(self: Method.Ptr) MethodMap.Ptr {
        return self.slots.object.getMap().unsafeAsType(.Method);
    }

    pub fn getMapForCaching(self: Method.Ptr, vm: *const VirtualMachine) ?Map.Ptr {
        _ = self;
        _ = vm;
        @panic("!!! getMapForCaching should never be called on Method!");
    }

    pub fn getSlots(self: Method.Ptr) Slot.Slice {
        return self.getMap().getSlots();
    }

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: Method.Ptr, visitor: anytype) !void {
        try self.slots.object.visitEdges(visitor);
        try self.assignable_slots.visitEdges(visitor);
    }

    /// Return a shallow copy of this object.
    pub fn clone(self: Method.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) Method.Ptr {
        _ = allocator;
        _ = heap;

        const new_method = create(token, actor_id, self.getMap());
        @memcpy(new_method.getAssignableSlots(), self.getAssignableSlots());

        return new_method;
    }

    /// Return the address of the current object.
    fn asObjectAddress(self: Method.Ptr) [*]u64 {
        return @ptrCast(@alignCast(self));
    }

    /// Return this object as a value.
    pub fn asValue(self: Method.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    /// Return the amount of bytes the object occupies in memory.
    pub fn getSizeInMemory(self: Method.Ptr) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    /// Return the amount of bytes required to clone the object.
    pub fn getSizeForCloning(self: Method.Ptr) usize {
        return self.getSizeInMemory();
    }

    /// Return the required size for allocation of this object.
    pub fn requiredSizeForAllocation(assignable_slot_count: u15) usize {
        return @sizeOf(Method) + AssignableSlots(Method).requiredMemorySize(assignable_slot_count);
    }

    /// Return the assignable slots on this object.
    pub fn getAssignableSlots(self: Method.Ptr) pointer.HeapSlice(GenericValue, .Mutable) {
        return self.assignable_slots.getAssignableSlots();
    }

    /// Return the assignable slot value for the given slot.
    pub fn getAssignableSlotValue(self: Method.Ptr, slot: Slot) pointer.HeapPtr(GenericValue, .Mutable) {
        return self.assignable_slots.getAssignableSlotValue(slot);
    }

    // --- Top level context creation ---

    const toplevel_context_string = "<top level>";
    pub fn createTopLevelContextForExecutable(
        allocator: Allocator,
        heap: *VirtualMachine.Heap,
        token: *heap_import.AllocationToken,
        executable: bytecode.Executable.Ref,
        block: *bytecode.Block,
    ) !Method.Ptr {
        const toplevel_context_method_map = blk: {
            const toplevel_context_name = try ByteArray.createWithValues(allocator, heap, token, .Global, toplevel_context_string);
            break :blk try MethodMap.create(allocator, heap, token, 0, 0, 0, false, toplevel_context_name, block, executable);
        };
        return create(token, context.getActor().id, toplevel_context_method_map);
    }

    pub fn requiredSizeForCreatingTopLevelContext() usize {
        return MethodMap.requiredSizeForAllocation(0) +
            Method.requiredSizeForAllocation(0) +
            ByteArray.requiredSizeForAllocation();
    }

    // --- Map forwarding ---

    pub fn expectsActivationObjectAsReceiver(self: Method.Ptr) bool {
        return self.getMap().expectsActivationObjectAsReceiver();
    }

    // --- Slot counts ---

    pub fn getArgumentSlotCount(self: Method.Ptr) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: Method.Ptr) u15 {
        return self.getMap().getAssignableSlotCount();
    }

    // --- Activation ---

    /// Creates a method activation object for this block and returns it.
    /// Copies `source_range`.
    pub fn activateMethod(
        self: Method.Ptr,
        token: *heap_import.AllocationToken,
        actor_id: Actor.ActorID,
        receiver: GenericValue,
        arguments: []const GenericValue,
        target_location: bytecode.RegisterLocation,
        created_from: SourceRange,
        out_activation: *Activation,
    ) void {
        const activation_object = ActivationObject.create(token, actor_id, .Method, self.slots.object.getMap(), arguments, self.getAssignableSlots(), receiver);
        out_activation.initInPlace(ActivationObject.Value.init(activation_object), target_location, self.getMap().method_name.get().getByteArray(), created_from);
    }

    pub fn requiredSizeForActivation(self: Method.Ptr) usize {
        return ActivationObject.requiredSizeForAllocation(self.getArgumentSlotCount(), self.getAssignableSlotCount());
    }
};

/// A map for a method. A method object is a slots object which has two separate
/// slot sections for argument slots and regular slots defined on the method
/// respectively. It also contains a pointer to the actual set of statements to
/// be executed. Finally, some debug info is stored which is then displayed in
/// stack traces.
pub const MethodMap = extern struct {
    base_map: ExecutableMap align(@alignOf(u64)),
    /// What the method is called.
    method_name: ObjectValue(ByteArray) align(@alignOf(u64)),
    slots: MapSlots(MethodMap) align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(MethodMap, .Mutable);
    pub const ObjectType = Method;

    const MethodInformation = packed struct(u1) { is_inline: bool };
    pub const ExtraBits = ExecutableMap.ExtraBits.reserve(MethodInformation);

    /// Create a new method map.
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
        is_inline_method: bool,
        method_name: ByteArray.Ptr,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) !MethodMap.Ptr {
        const size = MethodMap.requiredSizeForAllocation(slot_count);

        const inline_cache = try block.allocateInlineCache(allocator);
        errdefer allocator.free(inline_cache);

        const memory_area = token.allocate(size);
        try heap.markAddressAsNeedingFinalization(memory_area);

        var self: MethodMap.Ptr = @ptrCast(memory_area);
        self.init(slot_count, assignable_slot_count, argument_slot_count, is_inline_method, method_name, block, executable, inline_cache);

        return self;
    }

    fn init(
        self: MethodMap.Ptr,
        slot_count: u16,
        assignable_slot_count: u15,
        argument_slot_count: u8,
        is_inline_method: bool,
        method_name: ByteArray.Ptr,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
        inline_cache: []InlineCacheEntry,
    ) void {
        self.base_map.init(.Method, argument_slot_count, slot_count, block, executable, inline_cache);
        self.setAssignableSlotCount(assignable_slot_count);
        self.setInlineMethod(is_inline_method);
        self.method_name = .init(method_name);
    }

    fn setInlineMethod(self: MethodMap.Ptr, is_inline_method: bool) void {
        MethodMap.ExtraBits.write(self.base_map.slots.map.getMetadata(), .{ .is_inline = is_inline_method });
    }

    fn isInlineMethod(self: MethodMap.Ptr) bool {
        return MethodMap.ExtraBits.read(self.base_map.slots.map.getMetadata().*).is_inline;
    }

    pub fn expectsActivationObjectAsReceiver(self: MethodMap.Ptr) bool {
        return self.isInlineMethod();
    }

    pub fn finalize(self: MethodMap.Ptr, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: MethodMap.Ptr, visitor: anytype) !void {
        try self.base_map.visitEdges(visitor);
        try visitor.visit(&self.method_name.value, @ptrCast(self));
        try self.slots.visitEdges(visitor);
    }

    fn asObjectAddress(self: MethodMap.Ptr) [*]u64 {
        return @ptrCast(self);
    }

    pub fn asValue(self: MethodMap.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(asObjectAddress(self));
    }

    /// Return the amount of slots that this slot map contains.
    pub fn getSlotCount(self: MethodMap.Ptr) u16 {
        return self.base_map.slots.getSlotCount();
    }

    /// Get the assignable slot count for this map.
    pub fn getAssignableSlotCount(self: MethodMap.Ptr) u15 {
        return self.slots.getAssignableSlotCount();
    }

    /// Set the assignable slot count for this map.
    pub fn setAssignableSlotCount(self: MethodMap.Ptr, count: u15) void {
        self.slots.setAssignableSlotCount(count);
    }

    pub fn getArgumentSlotCount(self: MethodMap.Ptr) u8 {
        return self.base_map.getArgumentSlotCount();
    }

    /// Return the slots contained in this map.
    pub fn getSlots(self: MethodMap.Ptr) Slot.Slice {
        return self.slots.getSlots();
    }

    pub fn clone(self: MethodMap.Ptr, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken) !MethodMap.Ptr {
        const new_map = try create(
            // FIXME: Pass vm into clone!
            context.getVM().allocator,
            heap,
            token,
            self.getSlotCount(),
            self.getAssignableSlotCount(),
            self.getArgumentSlotCount(),
            self.isInlineMethod(),
            self.method_name.get(),
            self.base_map.block.get(),
            self.base_map.definition_executable_ref.get(),
        );

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        @memcpy(new_map.getSlots(), self.getSlots());

        return new_map;
    }

    pub fn getSizeInMemory(self: MethodMap.Ptr) usize {
        return requiredSizeForAllocation(self.getSlotCount());
    }

    pub fn getSizeForCloning(self: MethodMap.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation(slot_count: u16) usize {
        return @sizeOf(MethodMap) + MapSlots(MethodMap).requiredMemorySize(slot_count);
    }
};
