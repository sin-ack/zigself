// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = Object.Map;
const AST = @import("../../language/ast.zig");
const hash = @import("../../utility/hash.zig");
const Heap = @import("../heap.zig");
const Slot = @import("../slot.zig").Slot;
const Value = @import("../value.zig").Value;
const Script = @import("../../language/script.zig");
const Object = @import("../object.zig");
const MapType = @import("./map.zig").MapType;
const Location = @import("../../language/location.zig");
const SourceRange = @import("../../language/source_range.zig");
const RuntimeActivation = @import("../activation.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// A slots object. A slots object does not contain all the slots that are
/// actually in the object; for that, the map must be consulted. The slot
/// object begins with a header followed by Values for each of the
/// assignable slots.
pub const Slots = packed struct {
    header: Object.Header,

    pub fn create(heap: *Heap, map: *Map.Slots, assignable_slot_values: []const Value) !*Slots {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Slots, memory_area);
        self.init(map.asValue());
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: *Slots, map: Value) void {
        self.header.init(.Slots, map);
    }

    pub fn getMap(self: *Slots) *Map.Slots {
        return self.header.getMap().asSlotsMap();
    }

    pub fn getSizeInMemory(self: *Slots) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn asObjectAddress(self: *Slots) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *Slots) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    /// Returns a slice of `Value`s for the assignable slots that are after the
    /// given object offset. Should not be called from the outside, it's only
    /// intended for Slots and its "subclasses".
    pub fn getAssignableSlotsInternal(self: *Slots, comptime ObjectSize: usize, comptime MapT: type, map: *MapT) []Value {
        const object_memory = @ptrCast([*]u8, self);
        const assignable_slot_count = map.getAssignableSlotCount();

        return std.mem.bytesAsSlice(
            Value,
            object_memory[ObjectSize .. ObjectSize + assignable_slot_count * @sizeOf(Value)],
        );
    }

    /// Returns a slice of `Value`s for the assignable slots that are after the
    /// Slots object header.
    pub fn getAssignableSlots(self: *Slots) []Value {
        return self.getAssignableSlotsInternal(@sizeOf(Slots), Map.Slots, self.getMap());
    }

    /// Returns a slice of `Slot`s that exist on the slots map.
    pub fn getSlots(self: *Slots) []Slot {
        return self.getMap().getSlots();
    }

    /// Adds the given slots to the current object. May cause a move of this
    /// object.
    pub fn addSlotsFrom(self: *Slots, source_object: *Slots, heap: *Heap, allocator: Allocator) !*Slots {
        _ = heap;

        var new_assignable_slots: u32 = 0;
        var new_constant_slots: u32 = 0;

        calculateNewSlotsAfterMerging(self, source_object, &new_constant_slots, &new_assignable_slots);

        // If there are new slots to be added, then we need to create a new map
        // (or worse, create a new object and link everything to that new
        // object if there are new assignable slots!).
        if (new_constant_slots != 0 or new_assignable_slots != 0) {
            // Need to copy the current map into a new one.
            const target_object_map = self.getMap();
            const source_object_map = source_object.getMap();
            const existing_slot_count = target_object_map.slot_count;

            // Try to copy the existing slots from our map; if a slot with the
            // same hash exists on the source map, then use that instead of our
            // slot. This allows us to override existing slots on the target object.
            var new_map = try Map.Slots.create(heap, existing_slot_count + new_constant_slots + new_assignable_slots);
            var new_map_slots = new_map.getSlots();
            for (target_object_map.getSlots()) |target_object_slot, i| {
                var slot_to_add = blk: {
                    for (source_object_map.getSlots()) |source_object_slot| {
                        if (target_object_slot.hash == source_object_slot.hash) {
                            if (target_object_slot.isMutable() != source_object_slot.isMutable()) {
                                std.debug.panic("TODO: Sorry, changing the mutability of an existing slot has not been implemented yet.", .{});
                            }

                            break :blk source_object_slot;
                        }
                    }
                    break :blk target_object_slot;
                };

                const parent_flag: Slot.ParentFlag = if (slot_to_add.isParent()) .Parent else .NotParent;
                if (slot_to_add.isMutable()) {
                    new_map_slots[i].initMutable(Map.Slots, new_map, slot_to_add.name.asByteArray(), parent_flag);
                } else {
                    new_map_slots[i].initConstant(slot_to_add.name.asByteArray(), parent_flag, slot_to_add.value);
                }
            }

            // Let's add all the new slots.
            var new_slot_offset = existing_slot_count;
            new_slot_loop: for (source_object_map.getSlots()) |source_object_slot| {
                for (target_object_map.getSlots()) |target_object_slot| {
                    if (target_object_slot.hash == source_object_slot.hash) {
                        if (target_object_slot.isMutable() != source_object_slot.isMutable()) {
                            std.debug.panic("TODO: Sorry, changing the mutability of an existing slot has not been implemented yet.", .{});
                        }
                        continue :new_slot_loop;
                    }
                }

                const parent_flag: Slot.ParentFlag = if (source_object_slot.isParent()) .Parent else .NotParent;
                if (source_object_slot.isMutable()) {
                    new_map_slots[new_slot_offset].initMutable(Map.Slots, new_map, source_object_slot.name.asByteArray(), parent_flag);
                } else {
                    new_map_slots[new_slot_offset].initConstant(source_object_slot.name.asByteArray(), parent_flag, source_object_slot.value);
                }

                new_slot_offset += 1;
            }

            // Construct a slice of assignable slots.
            var assignable_slot_values = try std.ArrayList(Value).initCapacity(allocator, new_map.getAssignableSlotCount());
            defer assignable_slot_values.deinit();

            for (target_object_map.getSlots()) |target_object_slot| {
                if (!target_object_slot.isMutable())
                    continue;

                const value_to_append = blk: {
                    for (source_object_map.getSlots()) |source_object_slot| {
                        if (target_object_slot.hash == source_object_slot.hash) {
                            const slot_value = source_object.getAssignableSlots()[source_object_slot.value.asUnsignedInteger()];
                            break :blk slot_value;
                        }
                    }

                    const slot_value = self.getAssignableSlots()[target_object_slot.value.asUnsignedInteger()];
                    break :blk slot_value;
                };

                assignable_slot_values.appendAssumeCapacity(value_to_append);
            }

            source_assignable_slot_loop: for (source_object_map.getSlots()) |source_object_slot| {
                if (!source_object_slot.isMutable())
                    continue;

                for (target_object_map.getSlots()) |target_object_slot| {
                    if (target_object_slot.hash == source_object_slot.hash)
                        continue :source_assignable_slot_loop;
                }

                const slot_value = source_object.getAssignableSlots()[source_object_slot.value.asUnsignedInteger()];
                assignable_slot_values.appendAssumeCapacity(slot_value);
            }

            // Do we also have to create a new object?
            if (new_assignable_slots != 0) {
                // Yup. Create a new object and update all the references to it.
                var new_object = try Slots.create(heap, new_map, assignable_slot_values.items);
                heap.updateAllReferencesTo(self.asValue(), new_object.asValue());
                return new_object;
            } else {
                // Nope, just update our assignable slots and map.
                std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values.items);
                self.header.map_pointer = new_map.asValue();
                return self;
            }
        } else {
            std.debug.panic("TODO: Implement the easy part of adding slots", .{});
        }
    }

    /// Return the amount of bytes that must be available on the heap in order
    /// to merge `source_object` into `target_object`.
    pub fn requiredSizeForMerging(target_object: *Slots, source_object: *Slots) usize {
        var new_assignable_slots: u32 = 0;
        var new_constant_slots: u32 = 0;

        calculateNewSlotsAfterMerging(target_object, source_object, &new_constant_slots, &new_assignable_slots);

        var required_size = Map.Slots.requiredSizeForAllocation(
            (target_object.getMap().slot_count - target_object.getMap().getAssignableSlotCount()) + new_constant_slots,
        );
        if (new_assignable_slots > 0) {
            required_size += Slots.requiredSizeForAllocation(
                target_object.getMap().getAssignableSlotCount() + @intCast(u8, new_assignable_slots),
            );
        }

        return required_size;
    }

    /// Finds out how many new constant and assignable slots are required if the
    /// source object were to be merged into the target object.
    /// new_constant_slots and new_assignable_slots are out parameters.
    fn calculateNewSlotsAfterMerging(target_object: *Slots, source_object: *Slots, new_constant_slots: *u32, new_assignable_slots: *u32) void {
        for (source_object.getSlots()) |source_slot| {
            var did_find_matching_slot = false;
            for (target_object.getSlots()) |target_slot| {
                if (source_slot.hash == target_slot.hash) {
                    did_find_matching_slot = true;
                    break;
                }
            }

            if (!did_find_matching_slot) {
                if (source_slot.isMutable()) {
                    new_assignable_slots.* += 1;
                } else {
                    new_constant_slots.* += 1;
                }
            }
        }
    }

    pub fn clone(self: *Slots, heap: *Heap) !*Slots {
        return create(heap, self.getMap(), self.getAssignableSlots());
    }

    pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return @sizeOf(Object.Header) + assignable_slot_count * @sizeOf(Value);
    }
};

/// An activation object, which is just a slots object but with an extra
/// "receiver" value that is the actual value on which a message was activated.
/// Additionally, it carries a bit of debug information to help with
pub const Activation = packed struct {
    slots: Slots,
    receiver: Value,

    /// Borrows a ref from `message_script`.
    pub fn create(
        heap: *Heap,
        comptime map_type: MapType,
        map: *Map,
        assignable_slot_values: []Value,
        receiver: Value,
    ) !*Activation {
        const assignable_slot_count = switch (map_type) {
            .Block => map.asBlockMap().getAssignableSlotCount(),
            .Method => map.asMethodMap().getAssignableSlotCount(),
            else => unreachable,
        };

        if (assignable_slot_values.len != assignable_slot_count) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ assignable_slot_count, assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(assignable_slot_count);

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Activation, memory_area);
        self.init(map.asValue(), map_type, receiver);
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(
        self: *Activation,
        map: Value,
        comptime map_type: MapType,
        receiver: Value,
    ) void {
        self.slots.header.init(.Activation, map);
        self.setActivationType(if (map_type == .Block) ActivationType.Block else ActivationType.Method);

        self.receiver = receiver;
    }

    const ActivationTypeShift = Object.ObjectTypeShift + Object.ObjectTypeBits;
    const ActivationTypeBit: u64 = 1 << ActivationTypeShift;

    pub const ActivationType = enum(u64) {
        Method = 0 << ActivationTypeShift,
        Block = 1 << ActivationTypeShift,
    };
    pub fn getActivationType(self: *Activation) ActivationType {
        return @intToEnum(ActivationType, self.slots.header.object_information & ActivationTypeBit);
    }

    fn setActivationType(self: *Activation, comptime activation_type: ActivationType) void {
        self.slots.header.object_information = (self.slots.header.object_information & ~ActivationTypeBit) | @enumToInt(activation_type);
    }

    pub fn getMethodMap(self: *Activation) *Map.Method {
        if (self.getActivationType() == .Block) {
            std.debug.panic("Attempted to call getMethodMap on a block activation object", .{});
        }

        return self.slots.header.getMap().asMethodMap();
    }

    pub fn getBlockMap(self: *Activation) *Map.Block {
        if (self.getActivationType() == .Method) {
            std.debug.panic("Attempted to call getBlockMap on a method activation object", .{});
        }

        return self.slots.header.getMap().asBlockMap();
    }

    pub fn getSizeInMemory(self: *Activation) usize {
        return requiredSizeForAllocation(self.getAssignableSlotCount());
    }

    pub fn asObjectAddress(self: *Activation) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *Activation) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    fn getAssignableSlotCount(self: *Activation) u8 {
        return switch (self.getActivationType()) {
            .Method => self.getMethodMap().getAssignableSlotCount(),
            .Block => self.getBlockMap().getAssignableSlotCount(),
        };
    }

    fn getArgumentSlotCount(self: *Activation) u8 {
        return switch (self.getActivationType()) {
            .Method => self.getMethodMap().getArgumentSlotCount(),
            .Block => self.getBlockMap().getArgumentSlotCount(),
        };
    }

    /// Returns a slice of `Value`s for the assignable slots that are after the
    /// Activation object header.
    pub fn getAssignableSlots(self: *Activation) []Value {
        const activation_header_size = @sizeOf(Activation);
        const object_memory = @ptrCast([*]u8, self);

        return std.mem.bytesAsSlice(
            Value,
            object_memory[activation_header_size..self.getSizeInMemory()],
        );
    }

    fn getArgumentSlots(self: *Activation) []Value {
        return self.getAssignableSlots()[0..self.getArgumentSlotCount()];
    }

    pub fn setArguments(self: *Activation, arguments: []const Value) void {
        if (arguments.len != self.getArgumentSlotCount()) {
            std.debug.panic("!!! Passed arguments slice does not equal argument count", .{});
        }

        std.mem.copy(Value, self.getArgumentSlots(), arguments);
    }

    pub fn getSlots(self: *Activation) []Slot {
        return switch (self.getActivationType()) {
            .Method => self.getMethodMap().getSlots(),
            .Block => self.getBlockMap().getSlots(),
        };
    }

    /// Return the object on which the method activation was executed. If the
    /// receiver is also an activation object, then returns its receiver
    /// instead.
    pub fn findActivationReceiver(self: *Activation) Value {
        var object = self.asValue().asObject();
        while (object.isActivationObject()) {
            const receiver = object.asActivationObject().receiver;
            if (receiver.isObjectReference()) {
                object = receiver.asObject();
            } else {
                return receiver;
            }
        }

        std.debug.assert(@ptrToInt(object.getAddress()) != @ptrToInt(self));
        return object.asValue();
    }

    pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return @sizeOf(Activation) + assignable_slot_count * @sizeOf(Value);
    }
};

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = packed struct {
    slots: Slots,

    pub fn create(heap: *Heap, map: *Map.Method, assignable_slot_values: []const Value) !*Method {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Method, memory_area);
        self.init(map.asValue());
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: *Method, map: Value) void {
        self.slots.header.init(.Method, map);
    }

    pub fn asValue(self: *Method) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getMap(self: *Method) *Map.Method {
        return self.slots.header.getMap().asMethodMap();
    }

    pub fn getSizeInMemory(self: *Method) usize {
        return requiredSizeForAllocation(self.getAssignableSlotCount());
    }

    pub fn getDefinitionScript(self: *Method) Script.Ref {
        return self.getMap().getDefinitionScript();
    }

    pub fn getStatementsSlice(self: *Method) []AST.ExpressionNode {
        return self.getMap().getStatementsSlice();
    }

    pub fn getAssignableSlotCount(self: *Method) u8 {
        return self.getMap().getAssignableSlotCount();
    }

    pub fn getAssignableSlots(self: *Method) []Value {
        return self.slots.getAssignableSlotsInternal(@sizeOf(Method), Map.Method, self.getMap());
    }

    /// Creates a method activation object for this block and returns it.
    /// Copies `source_range`.
    pub fn activateMethod(
        self: *Method,
        heap: *Heap,
        receiver: Value,
        arguments: []const Value,
        source_range: SourceRange,
        out_activation: *RuntimeActivation,
    ) !void {
        const activation_object = try Activation.create(heap, .Method, self.slots.header.getMap(), self.getAssignableSlots(), receiver);
        activation_object.setArguments(arguments);

        const tracked_method_name = try heap.track(self.getMap().method_name);
        errdefer tracked_method_name.untrack(heap);

        try out_activation.initInPlace(heap, activation_object.asValue(), tracked_method_name, source_range, true);
    }

    pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return Slots.requiredSizeForAllocation(assignable_slot_count);
    }
};

/// A block object. A block object is a slots object with a block map as its
/// parent.
pub const Block = packed struct {
    slots: Slots,

    pub fn create(heap: *Heap, map: *Map.Block, assignable_slot_values: []const Value) !*Block {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Block, memory_area);
        self.init(map.asValue());
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: *Block, map: Value) void {
        self.slots.header.init(.Block, map);
    }

    pub fn asObjectAddress(self: *Block) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *Block) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getMap(self: *Block) *Map.Block {
        return self.slots.header.getMap().asBlockMap();
    }

    pub fn getSizeInMemory(self: *Block) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn getDefinitionScript(self: *Block) Script.Ref {
        return self.getMap().getDefinitionScript();
    }

    pub fn getStatementsSlice(self: *Block) []AST.ExpressionNode {
        return self.getMap().getStatementsSlice();
    }

    pub fn getArgumentSlotCount(self: *Block) usize {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: *Block) u8 {
        return self.getMap().getAssignableSlotCount();
    }

    pub fn getAssignableSlots(self: *Block) []Value {
        return self.slots.getAssignableSlotsInternal(@sizeOf(Block), Map.Block, self.getMap());
    }

    pub fn clone(self: *Block, heap: *Heap) !*Block {
        return create(heap, self.getMap(), self.getAssignableSlots());
    }

    /// Returns whether the passed message name is the correct one for this block
    /// to be executed. The logic is:
    ///
    /// - For blocks with no arguments, `value`
    /// - For blocks with a single argument, `value:`
    /// - For blocks with more than one argument, `value:With:`, with as many
    ///   `With:`s as needed (number of colons should match number of arguments)
    pub fn isCorrectMessageForBlockExecution(self: *Block, message: []const u8) bool {
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
        self: *Block,
        context: *InterpreterContext,
        receiver: Value,
        arguments: []const Value,
        message_name: Heap.Tracked,
        source_range: SourceRange,
        out_activation: *RuntimeActivation,
    ) !void {
        const activation_object = try Activation.create(context.vm.heap, .Block, self.slots.header.getMap(), self.getAssignableSlots(), receiver);
        activation_object.setArguments(arguments);

        try out_activation.initInPlace(context.vm.heap, activation_object.asValue(), message_name, source_range, false);
        out_activation.parent_activation = self.getMap().parent_activation.get(context);
        out_activation.nonlocal_return_target_activation = self.getMap().nonlocal_return_target_activation.get(context);
    }

    pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return Slots.requiredSizeForAllocation(assignable_slot_count);
    }
};
