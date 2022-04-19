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
const MapBuilder = @import("./map_builder.zig").MapBuilder;
const SourceRange = @import("../../language/source_range.zig");
const RuntimeActivation = @import("../activation.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

/// Information about added/changed slots when an object is merged into another.
const MergeInfo = struct {
    previous_slots: usize,
    previous_assignable_slot_values: usize,
    slots: usize,
    assignable_slot_values: usize,
    has_updated_slots: bool,

    pub fn hasChanges(self: MergeInfo) bool {
        return self.has_updated_slots or
            self.previous_slots != self.slots or
            self.previous_assignable_slot_values != self.assignable_slot_values;
    }

    pub fn assignableSlotCountChanged(self: MergeInfo) bool {
        return self.previous_assignable_slot_values != self.assignable_slot_values;
    }
};

/// A mixin struct which can be added to a slots-like object through `pub
/// usingnamespace`. Adds common functions expected from slots-like objects.
fn SlotsLikeObjectBase(comptime ObjectT: type) type {
    return struct {
        /// Return the address of the current object.
        pub fn asObjectAddress(self: *ObjectT) [*]u64 {
            return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
        }

        /// Return this object as a value.
        pub fn asValue(self: *ObjectT) Value {
            return Value.fromObjectAddress(asObjectAddress(self));
        }
    };
}

/// A mixin that provides the common implementation of assignable slots in
/// and assignable slots-dependent methods for slots-like objects.
fn AssignableSlotsMixin(comptime ObjectT: type) type {
    return struct {
        /// Return the amount of bytes that this object takes up in the
        /// heap.
        pub fn getSizeInMemory(self: *ObjectT) usize {
            return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
        }

        /// Return the amount of bytes required to create this object.
        pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
            return @sizeOf(ObjectT) + assignable_slot_count * @sizeOf(Value);
        }

        /// Return a slice of `Value`s for the assignable slots that are
        /// after the Slots object header. Should not be called from
        /// outside; use getAssignableSlotValue instead.
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
    };
}

/// A slots object. A slots object does not contain all the slots that are
/// actually in the object; for that, the map must be consulted. The slot
/// object begins with a header followed by Values for each of the
/// assignable slots.
pub const Slots = packed struct {
    header: Object.Header,

    pub usingnamespace SlotsLikeObjectBase(Slots);
    pub usingnamespace AssignableSlotsMixin(Slots);

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

    pub fn getMap(self: *Slots) *Map.Slots {
        return self.header.getMap().asSlotsMap();
    }

    pub fn getSlots(self: *Slots) []Slot {
        return self.getMap().getSlots();
    }

    // --- Adding slots ---

    /// Calls the given function with each slot in merge order.
    fn forSlotsInMergeOrder(
        target_object: *Slots,
        source_object: *Slots,
        context: anytype,
        comptime callback: fn (context: @TypeOf(context), object: *Slots, slot: Slot) Allocator.Error!void,
    ) !void {
        // We go through all of the target object's slots first, seeing if there
        // is any slot that should override ours. Any slot in the source object
        // with the same name should override ours.
        next_target_slot: for (target_object.getSlots()) |target_slot| {
            for (source_object.getSlots()) |source_slot| {
                if (source_slot.isInheritedChild())
                    continue;

                if (source_slot.getHash() == target_slot.getHash()) {
                    try callback(context, source_object, source_slot);
                    continue :next_target_slot;
                }
            }

            if (target_slot.isInheritedChild())
                continue;

            try callback(context, target_object, target_slot);
        }

        // We then go through all of the source object's slots, adding slots
        // that we haven't seen before to the map builder.
        next_source_slot: for (source_object.getSlots()) |source_slot| {
            for (target_object.getSlots()) |target_slot| {
                if (source_slot.getHash() == target_slot.getHash()) {
                    continue :next_source_slot;
                }
            }

            if (source_slot.isInheritedChild())
                continue;

            try callback(context, source_object, source_slot);
        }
    }

    /// Like forSlotsInMergeOrder but first calls with only inherited slots,
    /// and then calls with no inherited slots.
    fn forSlotsInMergeOrderWithInheritedFirst(
        target_object: *Slots,
        source_object: *Slots,
        context: anytype,
        comptime callback: fn (context: @TypeOf(context), object: *Slots, slot: Slot) Allocator.Error!void,
    ) !void {
        const ContextT = @TypeOf(context);

        try forSlotsInMergeOrder(target_object, source_object, context, struct {
            fn outerCallback(ctx: ContextT, object: *Slots, slot: Slot) !void {
                if (!slot.isInherited())
                    return;
                try callback(ctx, object, slot);
            }
        }.outerCallback);

        try forSlotsInMergeOrder(target_object, source_object, context, struct {
            fn outerCallback(ctx: ContextT, object: *Slots, slot: Slot) !void {
                if (slot.isInherited())
                    return;
                try callback(ctx, object, slot);
            }
        }.outerCallback);
    }

    /// Returns a slice of `Value`s for the assignable slots that are after the
    /// given object offset. Should not be called from the outside, it's only
    /// intended for Slots and its "subclasses".
    /// Add the source slots into the target object. This operation may cause a
    /// move of this object if the number of assignable slots changes. A pointer
    /// to the new location of the slots object is returned in this case.
    /// Otherwise, the original location is returned as a pointer.
    pub fn addSlotsFrom(self: *Slots, source_object: *Slots, allocator: Allocator, heap: *Heap) !*Slots {
        const merge_info = try self.calculateMergeOf(source_object, allocator);

        // If there is anything that could change any of the slots, then we need
        // to create a new copy of the map.
        const map_needs_change = merge_info.hasChanges();
        // We only create a new object if the assignable slot count changes;
        // otherwise we just overwrite the current object with the new slots.
        const object_needs_change = merge_info.assignableSlotCountChanged();

        if (!map_needs_change) {
            // If the map doesn't need to change at all then there is nothing
            // to merge in the source object.
            return self;
        }

        // Let's allocate a new map with the target slot count.
        var new_map = try Object.Map.Slots.create(heap, @intCast(u32, merge_info.slots));

        var map_builder = try new_map.getMapBuilder(heap);
        defer map_builder.deinit();

        const CallbackContext = struct { map_builder: *MapBuilder(Map.Slots, Slots) };

        try forSlotsInMergeOrderWithInheritedFirst(self, source_object, CallbackContext{ .map_builder = &map_builder }, struct {
            fn callback(context: CallbackContext, object: *Slots, slot: Slot) !void {
                const slot_copy = slot.copy(object);
                try context.map_builder.addSlot(slot_copy);
            }
        }.callback);

        // At this point, we have a map builder which has initialized our new
        // map with all the constant slots.
        // We now need to determine whether we need to create a new object on
        // the heap, or whether we can cheese it and change just the current
        // object on the heap instead.
        if (object_needs_change) {
            // We do need to create a new object, and then update all the heap
            // references to it.
            const new_object = try map_builder.createObject();
            try heap.updateAllReferencesTo(self.asValue(), new_object.asValue());
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
    pub fn requiredSizeForMerging(target_object: *Slots, source_object: *Slots, allocator: Allocator) !usize {
        const merge_info = try target_object.calculateMergeOf(source_object, allocator);

        // If there is anything that could change any of the slots, then we need
        // to create a new copy of the map.
        const map_needs_change = merge_info.hasChanges();
        // We only create a new object if the assignable slot count changes;
        // otherwise we just overwrite the current object with the new slots.
        const object_needs_change = merge_info.assignableSlotCountChanged();

        var required_size: usize = 0;
        if (map_needs_change)
            required_size += Map.Slots.requiredSizeForAllocation(@intCast(u32, merge_info.slots));
        if (object_needs_change)
            required_size += Slots.requiredSizeForAllocation(@intCast(u8, merge_info.assignable_slot_values));

        return required_size;
    }

    /// Finds out how many new constant and assignable slots are required if the
    /// source object were to be merged into the target object, and how many
    /// slots would change from a constant to assignable slot and vice versa.
    ///
    /// Returns a MergeInfo.
    fn calculateMergeOf(target_object: *Slots, source_object: *Slots, allocator: Allocator) !MergeInfo {
        var slots: usize = 0;
        // usize despite requiredAssignableSlotValueSpace returning isize, as
        // this should never go negative (that'd be a bug).
        var assignable_slot_values: usize = 0;
        var has_updated_slots = false;

        // Unfortunately we need to allocate an ArrayList here, because we do
        // not know ahead of time which slots from the source object and the
        // target object will end up in our merged object ahead of time. We use
        // this to look at the previous slots to see when slots need to be
        // overwritten.
        //
        // This incurs a small performance cost but hopefully shouldn't hurt
        // too much as merging slots into existing objects should not be a
        // frequent occurrence.
        var merged_slots = std.ArrayList(Slot).init(allocator);
        defer merged_slots.deinit();

        const CallbackContext = struct {
            merged_slots: *std.ArrayList(Slot),
            slots: *usize,
            assignable_slot_values: *usize,
            has_updated_slots: *bool,
            source_object: *Slots,
        };

        try forSlotsInMergeOrderWithInheritedFirst(target_object, source_object, CallbackContext{
            .merged_slots = &merged_slots,
            .slots = &slots,
            .assignable_slot_values = &assignable_slot_values,
            .has_updated_slots = &has_updated_slots,
            .source_object = source_object,
        }, struct {
            fn callback(context: CallbackContext, object: *Slots, slot: Slot) !void {
                context.slots.* += slot.requiredSlotSpace(context.merged_slots.items);
                context.assignable_slot_values.* = @intCast(usize, @intCast(isize, context.assignable_slot_values.*) +
                    slot.requiredAssignableSlotValueSpace(context.merged_slots.items));
                try context.merged_slots.append(slot);

                if (object == context.source_object)
                    context.has_updated_slots.* = true;
            }
        }.callback);

        return MergeInfo{
            .previous_slots = target_object.getSlots().len,
            .previous_assignable_slot_values = target_object.getMap().getAssignableSlotCount(),
            .slots = slots,
            .assignable_slot_values = assignable_slot_values,
            .has_updated_slots = has_updated_slots,
        };
    }
};

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = packed struct {
    slots: Slots,

    pub usingnamespace SlotsLikeObjectBase(Method);
    pub usingnamespace AssignableSlotsMixin(Method);

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

    pub fn getMap(self: *Method) *Map.Method {
        return self.slots.header.getMap().asMethodMap();
    }

    pub fn getSlots(self: *Slots) []Slot {
        return self.getMap().getSlots();
    }

    // --- Map forwarding ---

    pub fn expectsActivationObjectAsReceiver(self: *Method) bool {
        return self.getMap().expectsActivationObjectAsReceiver();
    }

    // --- Slot counts ---

    pub fn getArgumentSlotCount(self: *Method) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: *Method) u8 {
        return self.getMap().getAssignableSlotCount();
    }

    // --- Activation ---

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

    pub usingnamespace SlotsLikeObjectBase(Block);
    pub usingnamespace AssignableSlotsMixin(Block);

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

    pub fn getMap(self: *Block) *Map.Block {
        return self.slots.header.getMap().asBlockMap();
    }

    pub fn getSlots(self: *Slots) []Slot {
        return self.getMap().getSlots();
    }

    // --- Slot counts ---

    pub fn getArgumentSlotCount(self: *Block) u8 {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: *Block) u8 {
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

/// An activation object, which is just a slots object but with an extra
/// "receiver" value that is the actual value on which a message was activated.
pub const Activation = packed struct {
    slots: Slots,
    receiver: Value,

    pub usingnamespace SlotsLikeObjectBase(Activation);

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

    // --- Activation type ---

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

    // --- Slot counts ---

    pub fn getAssignableSlotCount(self: *Activation) u8 {
        return self.dispatch("getAssignableSlotCount");
    }

    pub fn getArgumentSlotCount(self: *Activation) u8 {
        return self.dispatch("getArgumentSlotCount");
    }

    // --- Map forwarding ---

    pub fn getDefinitionScript(self: *Activation) Script.Ref {
        return self.dispatch("getDefinitionScript");
    }

    pub fn getStatementsSlice(self: *Activation) []AST.ExpressionNode {
        return self.dispatch("getStatementsSlice");
    }

    // --- Slots and slot values ---

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

    // --- Finding activation receiver ---

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

    // --- Allocation and current size ---

    pub fn getSizeInMemory(self: *Activation) usize {
        return requiredSizeForAllocation(self.getArgumentSlotCount(), self.getAssignableSlotCount());
    }

    pub fn requiredSizeForAllocation(argument_slot_count: u8, assignable_slot_count: u8) usize {
        return @sizeOf(Activation) + (argument_slot_count + assignable_slot_count) * @sizeOf(Value);
    }

    // --- Map dispatch ---

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

    fn DispatchReturn(comptime fn_name: []const u8) type {
        return @typeInfo(@TypeOf(@field(Map.Method, fn_name))).Fn.return_type.?;
    }

    fn dispatch(self: *Activation, comptime fn_name: []const u8) DispatchReturn(fn_name) {
        return switch (self.getActivationType()) {
            .Method => @call(.{}, @field(self.getMethodMap(), fn_name), .{}),
            .Block => @call(.{}, @field(self.getBlockMap(), fn_name), .{}),
        };
    }
};
