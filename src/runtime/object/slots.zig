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

/// Information about added/changed slots when an object is merged into another.
const MergeInfo = struct {
    added_constant_slots: usize,
    added_assignable_slots: usize,
    changed_constant_slots: usize,
    changed_assignable_slots: usize,
    constant_to_assignable_slots: usize,
    assignable_to_constant_slots: usize,

    pub fn hasChanges(self: MergeInfo) bool {
        return self.added_constant_slots != 0 or
            self.added_assignable_slots != 0 or
            self.changed_constant_slots != 0 or
            self.changed_assignable_slots != 0 or
            self.constant_to_assignable_slots != 0 or
            self.assignable_to_constant_slots != 0;
    }
};

/// A mixin struct which can be added to a slots-like object through `pub
/// usingnamespace`. Adds common functions expected from slots-like objects.
fn SlotsLikeObjectBase(comptime ObjectT: type, comptime map_cast_fn: ?[]const u8, custom_assignable_slots: bool) type {
    return struct {
        /// Return the address of the current object.
        pub fn asObjectAddress(self: *ObjectT) [*]u64 {
            return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
        }

        /// Return this object as a value.
        pub fn asValue(self: *ObjectT) Value {
            return Value.fromObjectAddress(asObjectAddress(self));
        }

        pub usingnamespace if (map_cast_fn) |cast_fn|
            struct {
                fn asSlotsObject(self: *ObjectT) *Slots {
                    return @ptrCast(*Slots, self);
                }

                // fn [map_cast_fn](self: *Map) *XMap -> XMap
                const MapT = @typeInfo(@typeInfo(@TypeOf(@field(Object.Map, cast_fn))).Fn.return_type.?).Pointer.child;

                /// Return this object's map with the correct type.
                pub fn getMap(self: *ObjectT) *MapT {
                    return @call(.{}, @field(asSlotsObject(self).header.getMap(), cast_fn), .{});
                }

                /// Return a slice of `Slot`s that exist on the slots map.
                pub fn getSlots(self: *ObjectT) []Slot {
                    return getMap(self).getSlots();
                }
            }
        else
            struct {};

        pub usingnamespace if (!custom_assignable_slots)
            struct {
                /// Return the amount of bytes that this object takes up in the
                /// heap.
                pub fn getSizeInMemory(self: *ObjectT) usize {
                    return requiredSizeForAllocation(ObjectT.getMap(self).getAssignableSlotCount());
                }

                /// Return the amount of bytes required to create this object.
                pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
                    return @sizeOf(ObjectT) + assignable_slot_count * @sizeOf(Value);
                }

                /// Return a slice of `Value`s for the assignable slots that are
                /// after the Slots object header. Should not be called from
                /// outside, use getAssignableSlotValue instead.
                pub fn getAssignableSlots(self: *ObjectT) []Value {
                    const object_size = @sizeOf(ObjectT);
                    const object_memory = @ptrCast([*]u8, self);
                    const assignable_slot_count = ObjectT.getMap(self).getAssignableSlotCount();

                    return std.mem.bytesAsSlice(
                        Value,
                        object_memory[object_size .. object_size + assignable_slot_count * @sizeOf(Value)],
                    );
                }

                /// Return the assignable slot value for this slot.
                pub fn getAssignableSlotValue(self: *ObjectT, slot: Slot) *Value {
                    std.debug.assert(slot.isAssignable());
                    std.debug.assert(!slot.isArgument());

                    return &getAssignableSlots(self)[slot.value.asUnsignedInteger()];
                }

                /// Return a shallow copy of this object.
                pub fn clone(self: *ObjectT, heap: *Heap) !*ObjectT {
                    return ObjectT.create(heap, self.getMap(), getAssignableSlots(self));
                }
            }
        else
            struct {};
    };
}

/// A slots object. A slots object does not contain all the slots that are
/// actually in the object; for that, the map must be consulted. The slot
/// object begins with a header followed by Values for each of the
/// assignable slots.
pub const Slots = packed struct {
    header: Object.Header,

    pub usingnamespace SlotsLikeObjectBase(Slots, "asSlotsMap", false);

    pub fn create(heap: *Heap, map: *Map.Slots, assignable_slot_values: []const Value) !*Slots {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = Slots.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Slots, memory_area);
        self.init(map.asValue());
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: *Slots, map: Value) void {
        self.header.init(.Slots, map);
    }

    /// Returns a slice of `Value`s for the assignable slots that are after the
    /// given object offset. Should not be called from the outside, it's only
    /// intended for Slots and its "subclasses".
    /// Add the source slots into the target object. This operation may cause a
    /// move of this object if the number of assignable slots changes. A pointer
    /// to the new location of the slots object is returned in this case.
    /// Otherwise, the original location is returned as a pointer.
    pub fn addSlotsFrom(self: *Slots, source_object: *Slots, heap: *Heap) !*Slots {
        const merge_info = self.calculateMergeOf(source_object);

        const target_constant_slot_count = self.getMap().slot_count - self.getMap().getAssignableSlotCount();
        const constant_slot_count_after_merge =
            target_constant_slot_count - merge_info.constant_to_assignable_slots +
            merge_info.assignable_to_constant_slots +
            merge_info.added_constant_slots;

        const target_assignable_slot_count = self.getMap().getAssignableSlotCount();
        const assignable_slot_count_after_merge = target_assignable_slot_count - merge_info.assignable_to_constant_slots +
            merge_info.constant_to_assignable_slots +
            merge_info.added_assignable_slots;
        const assignable_slot_count_diff = @intCast(isize, assignable_slot_count_after_merge) - @intCast(isize, target_assignable_slot_count);

        // If there is anything that could change any of the slots, then we need
        // to create a new copy of the map.
        const map_needs_change = merge_info.hasChanges();
        // We only create a new object if the assignable slot count changes;
        // otherwise we just overwrite the current object with the new slots.
        const object_needs_change = assignable_slot_count_diff != 0;

        if (!map_needs_change) {
            // If the map doesn't need to change at all then there is nothing
            // to merge in the source object.
            return self;
        }

        // Let's allocate a new map with the target slot count.
        const current_map = self.getMap();
        const source_map = source_object.getMap();
        var new_map = try Object.Map.Slots.create(heap, @intCast(u32, constant_slot_count_after_merge + assignable_slot_count_after_merge));

        var map_builder = try new_map.getMapBuilder(heap);
        defer map_builder.deinit();

        // We go through all of the current map's slots first, seeing if there
        // is any slot that should override ours. Any slot in the source object
        // with the same name should override ours.
        next_target_slot: for (current_map.getSlots()) |target_slot| {
            for (source_map.getSlots()) |source_slot| {
                if (source_slot.getHash() == target_slot.getHash()) {
                    const source_slot_copy = source_slot.copy(source_object);
                    try map_builder.addSlot(source_slot_copy);
                    continue :next_target_slot;
                }
            }

            const target_slot_copy = target_slot.copy(self);
            try map_builder.addSlot(target_slot_copy);
        }

        // We then go through all of the source map's slots, adding slots that
        // we haven't seen before to the map builder.
        next_source_slot: for (source_map.getSlots()) |source_slot| {
            for (current_map.getSlots()) |target_slot| {
                if (source_slot.getHash() == target_slot.getHash()) {
                    continue :next_source_slot;
                }
            }

            const source_slot_copy = source_slot.copy(source_object);
            try map_builder.addSlot(source_slot_copy);
        }

        // At this point, we have a map builder which has initialized our new
        // map with all the constant slots.
        // We now need to determine whether we need to create a new object on
        // the heap, or whether we can cheese it and change just the current
        // object on the heap instead.
        if (object_needs_change) {
            // We do need to create a new object, and then update all the heap
            // references to it.
            const new_object = try map_builder.createObject();
            heap.updateAllReferencesTo(self.asValue(), new_object.asValue());
            return new_object;
        }

        // We can simply update the assignable slots and map pointer of the
        // object.
        self.header.map_pointer = map_builder.map.getValue();
        try heap.rememberObjectReference(self.asValue(), self.header.map_pointer);
        var assignable_values = self.getAssignableSlots();
        map_builder.writeAssignableSlotValuesTo(assignable_values);
        return self;
    }

    /// Return the amount of bytes that must be available on the heap in order
    /// to merge `source_object` into `target_object`.
    pub fn requiredSizeForMerging(target_object: *Slots, source_object: *Slots) usize {
        const merge_info = target_object.calculateMergeOf(source_object);

        // FIXME: Avoid duplicating these with addSlotsFrom
        const target_constant_slot_count = target_object.getMap().slot_count - target_object.getMap().getAssignableSlotCount();
        const constant_slot_count_after_merge =
            target_constant_slot_count - merge_info.constant_to_assignable_slots +
            merge_info.assignable_to_constant_slots +
            merge_info.added_constant_slots;

        const target_assignable_slot_count = target_object.getMap().getAssignableSlotCount();
        const assignable_slot_count_after_merge = target_assignable_slot_count - merge_info.assignable_to_constant_slots +
            merge_info.constant_to_assignable_slots +
            merge_info.added_assignable_slots;
        const assignable_slot_count_diff = @intCast(isize, assignable_slot_count_after_merge) - @intCast(isize, target_assignable_slot_count);

        // If there is anything that could change any of the slots, then we need
        // to create a new copy of the map.
        const map_needs_change = merge_info.hasChanges();
        // We only create a new object if the assignable slot count changes;
        // otherwise we just overwrite the current object with the new slots.
        const object_needs_change = assignable_slot_count_diff != 0;

        var required_size: usize = 0;
        if (map_needs_change)
            required_size += Map.Slots.requiredSizeForAllocation(@intCast(u32, constant_slot_count_after_merge));
        if (object_needs_change)
            required_size += Slots.requiredSizeForAllocation(@intCast(u8, assignable_slot_count_after_merge));

        return required_size;
    }

    /// Finds out how many new constant and assignable slots are required if the
    /// source object were to be merged into the target object, and how many
    /// slots would change from a constant to assignable slot and vice versa.
    ///
    /// Returns a MergeInfo.
    fn calculateMergeOf(target_object: *Slots, source_object: *Slots) MergeInfo {
        var added_constant_slots: usize = 0;
        var added_assignable_slots: usize = 0;
        var changed_constant_slots: usize = 0;
        var changed_assignable_slots: usize = 0;
        var constant_to_assignable_slots: usize = 0;
        var assignable_to_constant_slots: usize = 0;

        for (source_object.getSlots()) |source_slot| {
            // Argument objects are a method & block object thing. We don't want
            // any of that here.
            std.debug.assert(!source_slot.isArgument());

            var did_find_matching_slot = false;
            for (target_object.getSlots()) |target_slot| {
                if (source_slot.getHash() == target_slot.getHash()) {
                    if (source_slot.isAssignable() and !target_slot.isAssignable()) {
                        constant_to_assignable_slots += 1;
                    } else if (!source_slot.isAssignable() and target_slot.isAssignable()) {
                        assignable_to_constant_slots += 1;
                    } else {
                        const parent_flag_matches = source_slot.isParent() != target_slot.isParent();
                        const source_value = if (source_slot.isAssignable())
                            source_object.getAssignableSlots()[source_slot.value.asUnsignedInteger()]
                        else
                            source_slot.value;
                        const target_value = if (target_slot.isAssignable())
                            target_object.getAssignableSlots()[target_slot.value.asUnsignedInteger()]
                        else
                            target_slot.value;

                        if (!parent_flag_matches or source_value.data != target_value.data) {
                            if (source_slot.isAssignable()) {
                                changed_assignable_slots += 1;
                            } else {
                                changed_constant_slots += 1;
                            }
                        }
                    }

                    did_find_matching_slot = true;
                    break;
                }
            }

            if (!did_find_matching_slot) {
                if (source_slot.isAssignable()) {
                    added_assignable_slots += 1;
                } else {
                    added_constant_slots += 1;
                }
            }
        }

        return MergeInfo{
            .added_constant_slots = added_constant_slots,
            .added_assignable_slots = added_assignable_slots,
            .changed_constant_slots = changed_constant_slots,
            .changed_assignable_slots = changed_assignable_slots,
            .constant_to_assignable_slots = constant_to_assignable_slots,
            .assignable_to_constant_slots = assignable_to_constant_slots,
        };
    }
};

/// An activation object, which is just a slots object but with an extra
/// "receiver" value that is the actual value on which a message was activated.
/// Additionally, it carries a bit of debug information to help with
pub const Activation = packed struct {
    slots: Slots,
    receiver: Value,

    pub usingnamespace SlotsLikeObjectBase(Activation, null, true);

    /// Borrows a ref from `message_script`.
    pub fn create(
        heap: *Heap,
        comptime map_type: MapType,
        map: *Map,
        arguments: []const Value,
        assignable_slot_values: []Value,
        receiver: Value,
    ) !*Activation {
        const assignable_slot_count = switch (map_type) {
            .Block => map.asBlockMap().getAssignableSlotCount(),
            .Method => map.asMethodMap().getAssignableSlotCount(),
            else => unreachable,
        };
        const argument_slot_count = switch (map_type) {
            .Block => map.asBlockMap().getArgumentSlotCount(),
            .Method => map.asMethodMap().getArgumentSlotCount(),
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

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Activation, memory_area);
        self.init(map_type, map.asValue(), receiver);
        std.mem.copy(Value, self.getArgumentSlots(), arguments);
        std.mem.copy(Value, self.getNonargumentSlots(), assignable_slot_values);

        return self;
    }

    fn init(
        self: *Activation,
        comptime map_type: MapType,
        map: Value,
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

    pub fn getAssignableSlotCount(self: *Activation) u8 {
        return self.dispatch("getAssignableSlotCount");
    }

    pub fn getArgumentSlotCount(self: *Activation) u8 {
        return self.dispatch("getArgumentSlotCount");
    }

    pub fn getSlots(self: *Activation) []Slot {
        return self.dispatch("getSlots");
    }

    /// Return a slice of `Value`s for the assignable slots that are after the
    /// Activation object header.
    fn getAssignableSlots(self: *Activation) []Value {
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

    fn getNonargumentSlots(self: *Activation) []Value {
        const slot_values = self.getAssignableSlots();
        std.debug.assert(slot_values.len - self.getArgumentSlotCount() == self.getAssignableSlotCount());

        return slot_values[self.getArgumentSlotCount()..];
    }

    pub fn getAssignableSlotValue(self: *Activation, slot: Slot) *Value {
        std.debug.assert(slot.isAssignable());

        return if (slot.isArgument())
            &self.getArgumentSlots()[slot.value.asUnsignedInteger()]
        else
            &self.getNonargumentSlots()[slot.value.asUnsignedInteger()];
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

    pub fn getSizeInMemory(self: *Activation) usize {
        return requiredSizeForAllocation(self.getArgumentSlotCount(), self.getAssignableSlotCount());
    }

    pub fn requiredSizeForAllocation(argument_slot_count: u8, assignable_slot_count: u8) usize {
        return @sizeOf(Activation) + (argument_slot_count + assignable_slot_count) * @sizeOf(Value);
    }

    fn dispatchReturn(comptime fn_name: []const u8) type {
        return @typeInfo(@TypeOf(@field(Map.Method, fn_name))).Fn.return_type.?;
    }

    fn getMethodMap(self: *Activation) *Map.Method {
        if (self.getActivationType() == .Block) {
            std.debug.panic("Attempted to call getMethodMap on a block activation object", .{});
        }

        return self.slots.header.getMap().asMethodMap();
    }

    fn getBlockMap(self: *Activation) *Map.Block {
        if (self.getActivationType() == .Method) {
            std.debug.panic("Attempted to call getBlockMap on a method activation object", .{});
        }

        return self.slots.header.getMap().asBlockMap();
    }

    fn dispatch(self: *Activation, comptime fn_name: []const u8) dispatchReturn(fn_name) {
        return switch (self.getActivationType()) {
            .Method => @call(.{}, @field(self.getMethodMap(), fn_name), .{}),
            .Block => @call(.{}, @field(self.getBlockMap(), fn_name), .{}),
        };
    }
};

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = packed struct {
    slots: Slots,

    pub usingnamespace SlotsLikeObjectBase(Method, "asMethodMap", false);

    pub fn create(heap: *Heap, map: *Map.Method, assignable_slot_values: []const Value) !*Method {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = Method.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Method, memory_area);
        self.init(map.asValue());
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: *Method, map: Value) void {
        self.slots.header.init(.Method, map);
    }

    pub fn getDefinitionScript(self: *Method) Script.Ref {
        return self.getMap().getDefinitionScript();
    }

    pub fn getStatementsSlice(self: *Method) []AST.ExpressionNode {
        return self.getMap().getStatementsSlice();
    }

    pub fn getArgumentSlotCount(self: *Method) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: *Method) u8 {
        return self.getMap().getAssignableSlotCount();
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
        const activation_object = try Activation.create(
            heap,
            .Method,
            self.slots.header.getMap(),
            arguments,
            self.getAssignableSlots(),
            receiver,
        );

        const tracked_method_name = try heap.track(self.getMap().method_name);
        errdefer tracked_method_name.untrack(heap);

        try out_activation.initInPlace(heap, activation_object.asValue(), tracked_method_name, source_range, true);
    }
};

/// A block object. A block object is a slots object with a block map as its
/// parent.
pub const Block = packed struct {
    slots: Slots,

    pub usingnamespace SlotsLikeObjectBase(Block, "asBlockMap", false);

    pub fn create(heap: *Heap, map: *Map.Block, assignable_slot_values: []const Value) !*Block {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = Block.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Block, memory_area);
        self.init(map.asValue());
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: *Block, map: Value) void {
        self.slots.header.init(.Block, map);
    }

    pub fn getDefinitionScript(self: *Block) Script.Ref {
        return self.getMap().getDefinitionScript();
    }

    pub fn getStatementsSlice(self: *Block) []AST.ExpressionNode {
        return self.getMap().getStatementsSlice();
    }

    pub fn getArgumentSlotCount(self: *Block) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: *Block) u8 {
        return self.getMap().getAssignableSlotCount();
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
        const activation_object = try Activation.create(
            context.vm.heap,
            .Block,
            self.slots.header.getMap(),
            arguments,
            self.getAssignableSlots(),
            receiver,
        );

        try out_activation.initInPlace(context.vm.heap, activation_object.asValue(), message_name, source_range, false);
        out_activation.parent_activation = self.getMap().parent_activation.get(context);
        out_activation.nonlocal_return_target_activation = self.getMap().nonlocal_return_target_activation.get(context);
    }
};
