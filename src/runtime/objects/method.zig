// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("../Heap.zig");
const Slot = @import("../slot.zig").Slot;
const slots = @import("slots.zig");
const bytecode = @import("../bytecode.zig");
const ByteArray = @import("../ByteArray.zig");
const Activation = @import("../Activation.zig");
const SlotsObject = slots.Slots;
const SourceRange = @import("../SourceRange.zig");
const GenericValue = value_import.Value;
const value_import = @import("../value.zig");
const object_lookup = @import("../object_lookup.zig");
const ExecutableMap = @import("executable_map.zig").ExecutableMap;
const stage2_compat = @import("../../utility/stage2_compat.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationObject = @import("activation.zig").Activation;
const SlotsLikeMapBase = slots.SlotsLikeMapBase;
const SlotsLikeObjectBase = slots.SlotsLikeObjectBase;
const AssignableSlotsMixin = slots.AssignableSlotsMixin;

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = extern struct {
    slots: SlotsObject align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(Method, .Mutable);
    pub const Type = .Method;
    pub const Value = value_import.ObjectValue(Method);

    pub usingnamespace SlotsLikeObjectBase(Method);
    pub usingnamespace AssignableSlotsMixin(Method);

    pub fn create(token: *Heap.AllocationToken, actor_id: u31, map: MethodMap.Ptr, assignable_slot_values: []const GenericValue) Method.Ptr {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = Method.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(Method.Ptr, memory_area);
        self.init(actor_id, map);
        @memcpy(self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: Method.Ptr, actor_id: u31, map: MethodMap.Ptr) void {
        self.slots.object = .{
            .object_information = .{
                .object_type = .Method,
                .actor_id = actor_id,
            },
            .map = map.asValue(),
        };
    }

    pub fn getMap(self: Method.Ptr) MethodMap.Ptr {
        return self.slots.object.getMap().mustBeType(.Method);
    }

    pub fn getSlots(self: Method.Ptr) Slot.Slice {
        return self.getMap().getSlots();
    }

    pub fn canFinalize(self: Method.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: Method.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call Method.finalize");
    }

    pub fn lookup(self: Method.Ptr, vm: *VirtualMachine, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = self;
        _ = vm;
        _ = selector_hash;
        _ = previously_visited;
        @panic("Attempted to call Method.lookup");
    }

    // --- Top level context creation ---

    const toplevel_context_string = "<top level>";
    pub fn createTopLevelContextForExecutable(
        vm: *VirtualMachine,
        token: *Heap.AllocationToken,
        executable: bytecode.Executable.Ref,
        block: *bytecode.Block,
    ) !Method.Ptr {
        const toplevel_context_method_map = blk: {
            const toplevel_context_name = ByteArray.createFromString(token, toplevel_context_string);
            break :blk try MethodMap.create(vm.getMapMap(), token, 0, 0, false, toplevel_context_name, block, executable);
        };
        return create(token, vm.current_actor.id, toplevel_context_method_map, &.{});
    }

    pub fn requiredSizeForCreatingTopLevelContext() usize {
        return ByteArray.requiredSizeForAllocation(toplevel_context_string.len) +
            MethodMap.requiredSizeForAllocation(0) +
            Method.requiredSizeForAllocation(0);
    }

    // --- Map forwarding ---

    pub fn expectsActivationObjectAsReceiver(self: Method.Ptr) bool {
        return self.getMap().expectsActivationObjectAsReceiver();
    }

    // --- Slot counts ---

    pub fn getArgumentSlotCount(self: Method.Ptr) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: Method.Ptr) u8 {
        return self.getMap().getAssignableSlotCount();
    }

    // --- Activation ---

    /// Creates a method activation object for this block and returns it.
    /// Copies `source_range`.
    pub fn activateMethod(
        self: Method.Ptr,
        vm: *VirtualMachine,
        token: *Heap.AllocationToken,
        actor_id: u31,
        receiver: GenericValue,
        arguments: []const GenericValue,
        target_location: bytecode.RegisterLocation,
        created_from: SourceRange,
        out_activation: *Activation,
    ) void {
        const activation_object = ActivationObject.create(token, actor_id, .Method, self.slots.object.getMap(), arguments, self.getAssignableSlots(), receiver);
        out_activation.initInPlace(ActivationObject.Value.init(activation_object), target_location, vm.takeStackSnapshot(), self.getMap().method_name, created_from);
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
    method_name: GenericValue align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(MethodMap, .Mutable);
    pub const ObjectType = Method;

    const MethodInformation = packed struct(u32) {
        is_inline: bool,
        padding: u31 = 0,
    };

    pub usingnamespace SlotsLikeMapBase(MethodMap);

    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    pub fn create(
        map_map: Map.Ptr,
        token: *Heap.AllocationToken,
        argument_slot_count: u8,
        total_slot_count: u32,
        is_inline_method: bool,
        method_name: ByteArray,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) !MethodMap.Ptr {
        const size = MethodMap.requiredSizeForAllocation(total_slot_count);

        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(MethodMap.Ptr, memory_area);
        self.init(map_map, argument_slot_count, total_slot_count, is_inline_method, method_name, block, executable);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(
        self: MethodMap.Ptr,
        map_map: Map.Ptr,
        argument_slot_count: u8,
        total_slot_count: u32,
        is_inline_method: bool,
        method_name: ByteArray,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) void {
        self.base_map.init(.Method, map_map, argument_slot_count, total_slot_count, block, executable);
        self.setInlineMethod(is_inline_method);
        self.method_name = method_name.asValue();
    }

    fn setInlineMethod(self: MethodMap.Ptr, is_inline_method: bool) void {
        @ptrCast(*MethodInformation, &self.base_map.slots.map.map_information.extra).is_inline = is_inline_method;
    }

    fn isInlineMethod(self: MethodMap.Ptr) bool {
        return @ptrCast(*MethodInformation, &self.base_map.slots.map.map_information.extra).is_inline;
    }

    pub fn expectsActivationObjectAsReceiver(self: MethodMap.Ptr) bool {
        return self.isInlineMethod();
    }

    pub fn canFinalize(self: MethodMap.Ptr) bool {
        _ = self;
        return true;
    }

    pub fn finalize(self: MethodMap.Ptr, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    pub fn getArgumentSlotCount(self: MethodMap.Ptr) u8 {
        return self.base_map.getArgumentSlotCount();
    }

    pub fn clone(self: MethodMap.Ptr, vm: *VirtualMachine, token: *Heap.AllocationToken) !MethodMap.Ptr {
        const new_map = try create(
            vm.getMapMap(),
            token,
            self.getArgumentSlotCount(),
            self.base_map.slots.information.slot_count,
            self.isInlineMethod(),
            self.method_name.asByteArray(),
            self.base_map.block.get(),
            self.base_map.definition_executable_ref.get(),
        );

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        @memcpy(new_map.getSlots(), self.getSlots());

        return new_map;
    }

    pub fn getSizeInMemory(self: MethodMap.Ptr) usize {
        return requiredSizeForAllocation(self.base_map.slots.information.slot_count);
    }

    pub fn getSizeForCloning(self: MethodMap.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation(slot_count: u32) usize {
        return @sizeOf(MethodMap) + slot_count * @sizeOf(Slot);
    }
};
