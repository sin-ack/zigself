// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = map_import.Map;
const Slot = @import("../slot.zig").Slot;
const Heap = @import("../Heap.zig");
const Actor = @import("../Actor.zig");
const slots = @import("slots.zig");
const MapType = map_import.MapType;
const pointer = @import("../../utility/pointer.zig");
const bytecode = @import("../bytecode.zig");
const BlockMap = @import("block.zig").BlockMap;
const MethodMap = @import("method.zig").MethodMap;
const map_import = @import("map.zig");
const SlotsObject = slots.Slots;
const GenericValue = value_import.Value;
const value_import = @import("../value.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const exceedsBoundsOf = @import("../../utility/bounds_check.zig").exceedsBoundsOf;
const SlotsLikeObjectBase = slots.SlotsLikeObjectBase;

/// An activation object, which is just a slots object but with an extra
/// "receiver" value that is the actual value on which a message was activated.
pub const Activation = extern struct {
    slots: SlotsObject align(@alignOf(u64)),
    receiver: GenericValue align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Activation, .Mutable);
    pub const Type = .Activation;
    pub const Value = value_import.ObjectValue(Activation);

    pub usingnamespace SlotsLikeObjectBase(Activation);

    /// Borrows a ref from `message_script`.
    pub fn create(
        token: *Heap.AllocationToken,
        actor_id: Actor.ActorID,
        comptime map_type: MapType,
        map: Map.Ptr,
        arguments: []const GenericValue,
        assignable_slot_values: []GenericValue,
        receiver: GenericValue,
    ) Activation.Ptr {
        const assignable_slot_count = switch (map_type) {
            .Block => map.mustBeType(.Block).getAssignableSlotCount(),
            .Method => map.mustBeType(.Method).getAssignableSlotCount(),
            else => unreachable,
        };
        const argument_slot_count = switch (map_type) {
            .Block => map.mustBeType(.Block).getArgumentSlotCount(),
            .Method => map.mustBeType(.Method).getArgumentSlotCount(),
            else => unreachable,
        };

        if (arguments.len != argument_slot_count) {
            std.debug.panic(
                "Passed argument slice does not match argument slot count in map (expected {}, got {})",
                .{ argument_slot_count, arguments.len },
            );
        }

        if (assignable_slot_values.len != assignable_slot_count) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ assignable_slot_count, assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(argument_slot_count, assignable_slot_count);

        const memory_area = token.allocate(.Object, size);
        var self: Activation.Ptr = @ptrCast(memory_area);
        self.init(map_type, actor_id, map, receiver);

        // NOTE: Inlining getAssignableSlots here in order to avoid multiple
        //       dynamic dispatches.
        const activation_header_size = @sizeOf(Activation);
        const aligned_memory_area: [*]align(@alignOf(u64)) u8 = @ptrCast(memory_area);
        const assignable_slots = std.mem.bytesAsSlice(
            GenericValue,
            aligned_memory_area[activation_header_size..size],
        );

        @memcpy(assignable_slots[0..argument_slot_count], arguments);
        @memcpy(assignable_slots[argument_slot_count..], assignable_slot_values);

        return self;
    }

    fn init(
        self: Activation.Ptr,
        comptime map_type: MapType,
        actor_id: Actor.ActorID,
        map: Map.Ptr,
        receiver: GenericValue,
    ) void {
        self.slots.object = .{
            .object_information = .{
                .object_type = .Activation,
                .actor_id = actor_id,
            },
            .map = map.asValue(),
        };

        self.setActivationType(if (map_type == .Block) ActivationType.Block else ActivationType.Method);

        self.receiver = receiver;
    }

    // --- Activation type ---

    const ActivationInformation = packed struct(u8) {
        activation_type: ActivationType,
        padding: u7,
    };

    pub const ActivationType = enum(u1) { Method, Block };
    pub fn getActivationType(self: Activation.Ptr) ActivationType {
        const activation_information: *ActivationInformation = @ptrCast(&self.slots.object.object_information.extra);
        return activation_information.activation_type;
    }

    fn setActivationType(self: Activation.Ptr, comptime activation_type: ActivationType) void {
        const activation_information: *ActivationInformation = @ptrCast(&self.slots.object.object_information.extra);
        activation_information.activation_type = activation_type;
    }

    // --- Slot counts ---

    pub fn getAssignableSlotCount(self: Activation.Ptr) u8 {
        return self.dispatch("getAssignableSlotCount");
    }

    pub fn getArgumentSlotCount(self: Activation.Ptr) u8 {
        return self.dispatch("getArgumentSlotCount");
    }

    // --- Map forwarding ---

    pub fn getDefinitionExecutable(self: Activation.Ptr) bytecode.Executable.Ref {
        return switch (self.getActivationType()) {
            .Method => self.getMethodMap().base_map.definition_executable_ref.get(),
            .Block => self.getBlockMap().base_map.definition_executable_ref.get(),
        };
    }

    pub fn getBytecodeBlock(self: Activation.Ptr) *bytecode.Block {
        return switch (self.getActivationType()) {
            .Method => self.getMethodMap().base_map.block.get(),
            .Block => self.getBlockMap().base_map.block.get(),
        };
    }

    // --- Slots and slot values ---

    pub fn getSlots(self: Activation.Ptr) Slot.Slice {
        return self.dispatch("getSlots");
    }

    /// Return a slice of `GenericValue`s for the assignable slots that are after the
    /// Activation object header.
    fn getAssignableSlots(self: Activation.Ptr) []GenericValue {
        const activation_header_size = @sizeOf(Activation);
        const object_memory: [*]u8 = @ptrCast(self);

        return @alignCast(std.mem.bytesAsSlice(
            GenericValue,
            object_memory[activation_header_size..self.getSizeInMemory()],
        ));
    }

    fn getArgumentSlots(self: Activation.Ptr) []GenericValue {
        return self.getAssignableSlots()[0..self.getArgumentSlotCount()];
    }

    fn getNonargumentSlots(self: Activation.Ptr) []GenericValue {
        const slot_values = self.getAssignableSlots();
        std.debug.assert(slot_values.len - self.getArgumentSlotCount() == self.getAssignableSlotCount());

        return slot_values[self.getArgumentSlotCount()..];
    }

    pub fn getAssignableSlotValue(self: Activation.Ptr, slot: Slot) *GenericValue {
        std.debug.assert(slot.isAssignable());

        const offset_int = slot.value.asUnsignedInteger();
        std.debug.assert(!exceedsBoundsOf(offset_int, usize));

        const offset: usize = @intCast(offset_int);
        return if (slot.isArgument())
            &self.getArgumentSlots()[offset]
        else
            &self.getNonargumentSlots()[offset];
    }

    pub fn canFinalize(self: Activation.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: Activation.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call Activation.finalize");
    }

    pub fn lookup(self: Activation.Ptr, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        const slots_lookup_result = slots.slotsLookup(Activation, self, selector_hash, previously_visited);
        if (slots_lookup_result != .Nothing) return slots_lookup_result;

        // Receiver lookup
        return self.receiver.lookupByHash(selector_hash);
    }

    // --- Finding activation receiver ---

    /// Return the object on which the method activation was executed. If the
    /// receiver is also an activation object, then returns its receiver
    /// instead.
    pub fn findActivationReceiver(self: Activation.Ptr) GenericValue {
        var object = self.asValue().asObject();
        while (object.object_information.object_type == .Activation) {
            const receiver = object.mustBeType(.Activation).receiver;
            if (receiver.isObjectReference()) {
                object = receiver.asObject();
            } else {
                return receiver;
            }
        }

        std.debug.assert(@intFromPtr(object.getAddress()) != @intFromPtr(self));
        return object.asValue();
    }

    // --- Allocation and current size ---

    pub fn getSizeInMemory(self: Activation.Ptr) usize {
        return requiredSizeForAllocation(self.getArgumentSlotCount(), self.getAssignableSlotCount());
    }

    pub fn getSizeForCloning(self: Activation.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation(argument_slot_count: u8, assignable_slot_count: u8) usize {
        return @sizeOf(Activation) + (argument_slot_count + assignable_slot_count) * @sizeOf(GenericValue);
    }

    // --- Map dispatch ---

    fn getMethodMap(self: Activation.Ptr) MethodMap.Ptr {
        if (self.getActivationType() == .Block) {
            std.debug.panic("Attempted to call getMethodMap on a block activation object", .{});
        }

        return self.slots.object.getMap().mustBeType(.Method);
    }

    fn getBlockMap(self: Activation.Ptr) BlockMap.Ptr {
        if (self.getActivationType() == .Method) {
            std.debug.panic("Attempted to call getBlockMap on a method activation object", .{});
        }

        return self.slots.object.getMap().mustBeType(.Block);
    }

    fn DispatchReturn(comptime fn_name: []const u8) type {
        return @typeInfo(@TypeOf(@field(MethodMap, fn_name))).Fn.return_type.?;
    }

    fn dispatch(self: Activation.Ptr, comptime fn_name: []const u8) DispatchReturn(fn_name) {
        return switch (self.getActivationType()) {
            .Method => @call(.auto, @field(MethodMap, fn_name), .{self.getMethodMap()}),
            .Block => @call(.auto, @field(BlockMap, fn_name), .{self.getBlockMap()}),
        };
    }
};
