// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("../slot.zig").Slot;
const Actor = @import("../Actor.zig");
const slots = @import("slots.zig");
const context = @import("../context.zig");
const pointer = @import("../../utility/pointer.zig");
const Selector = @import("../Selector.zig");
const bytecode = @import("../bytecode.zig");
const ByteArray = @import("byte_array.zig").ByteArray;
const Activation = @import("../Activation.zig");
const SlotsObject = slots.Slots;
const heap_import = @import("../Heap.zig");
const ObjectValue = value_import.ObjectValue;
const SourceRange = @import("../SourceRange.zig");
const GenericValue = value_import.Value;
const value_import = @import("../value.zig");
const LookupResult = @import("../object_lookup.zig").LookupResult;
const ExecutableMap = @import("executable_map.zig").ExecutableMap;
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationObject = @import("activation.zig").Activation;
const SlotsLikeMapBase = slots.SlotsLikeMapBase;
const SlotsLikeObjectBase = slots.SlotsLikeObjectBase;
const AssignableSlotsMixin = slots.AssignableSlotsMixin;

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = extern struct {
    slots: SlotsObject align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Method, .Mutable);
    pub const Type = .Method;
    pub const Value = value_import.ObjectValue(Method);

    pub usingnamespace SlotsLikeObjectBase(Method);
    pub usingnamespace AssignableSlotsMixin(Method);

    pub fn create(token: *heap_import.AllocationToken, actor_id: Actor.ActorID, map: MethodMap.Ptr, assignable_slot_values: []const GenericValue) Method.Ptr {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = Method.requiredSizeForAllocation(@intCast(assignable_slot_values.len));

        const memory_area = token.allocate(size);
        var self: Method.Ptr = @ptrCast(memory_area);
        self.init(actor_id, map);
        @memcpy(self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: Method.Ptr, actor_id: Actor.ActorID, map: MethodMap.Ptr) void {
        self.slots.object.init(.Method, actor_id, map.asValue());
    }

    pub fn getMap(self: Method.Ptr) MethodMap.Ptr {
        return self.slots.object.getMap().unsafeAsType(.Method);
    }

    pub fn getSlots(self: Method.Ptr) Slot.Slice {
        return self.getMap().getSlots();
    }

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: Method.Ptr, visitor: anytype) !void {
        try self.slots.object.visitEdges(visitor);
        try self.visitAssignableSlotValues(visitor);
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
            break :blk try MethodMap.create(heap, token, 0, 0, false, toplevel_context_name, block, executable);
        };
        return create(token, context.getActor().id, toplevel_context_method_map, &.{});
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

    pub const Ptr = pointer.HeapPtr(MethodMap, .Mutable);
    pub const ObjectType = Method;

    const MethodInformation = packed struct(u1) { is_inline: bool };
    pub const ExtraBits = ExecutableMap.ExtraBits.reserve(MethodInformation);

    pub usingnamespace SlotsLikeMapBase(MethodMap);

    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    pub fn create(
        heap: *VirtualMachine.Heap,
        token: *heap_import.AllocationToken,
        argument_slot_count: u8,
        total_slot_count: u16,
        is_inline_method: bool,
        method_name: ByteArray.Ptr,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) !MethodMap.Ptr {
        const size = MethodMap.requiredSizeForAllocation(total_slot_count);

        const memory_area = token.allocate(size);
        var self: MethodMap.Ptr = @ptrCast(memory_area);
        self.init(argument_slot_count, total_slot_count, is_inline_method, method_name, block, executable);

        try heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(
        self: MethodMap.Ptr,
        argument_slot_count: u8,
        total_slot_count: u16,
        is_inline_method: bool,
        method_name: ByteArray.Ptr,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) void {
        self.base_map.init(.Method, argument_slot_count, total_slot_count, block, executable);
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
        try visitor.visit(&self.method_name.value, @ptrCast(self));
        try self.visitSlots(visitor);
    }

    pub fn getArgumentSlotCount(self: MethodMap.Ptr) u8 {
        return self.base_map.getArgumentSlotCount();
    }

    pub fn clone(self: MethodMap.Ptr, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken) !MethodMap.Ptr {
        const new_map = try create(
            heap,
            token,
            self.getArgumentSlotCount(),
            self.getSlotCount(),
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
        return @sizeOf(MethodMap) + slot_count * @sizeOf(Slot);
    }
};
