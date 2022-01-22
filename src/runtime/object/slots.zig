// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
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
const Range = @import("../../language/location_range.zig");
const Script = @import("../../language/script.zig");
const Object = @import("../object.zig");
const MapType = @import("./map.zig").MapType;
const Location = @import("../../language/location.zig");
const ByteVector = @import("../byte_vector.zig");
const RuntimeActivation = @import("../activation.zig");

/// A slots object. A slots object does not contain all the slots that are
/// actually in the object; for that, the map must be consulted. The slot
/// object begins with a header followed by Values for each of the
/// assignable slots.
pub const Slots = packed struct {
    header: Object.Header,

    pub fn create(heap: *Heap, map: *Map.Slots, assignable_slot_values: []Value) !*Slots {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Slots, memory_area);
        self.init(map.asValue(), assignable_slot_values);

        return self;
    }

    fn init(self: *Slots, map: Value, assignable_slot_values: []Value) void {
        self.header.init(.Slots, map);
        std.mem.copy(Value, self.getAssignableSlots(), assignable_slot_values);
    }

    pub fn getMap(self: *Slots) *Map.Slots {
        return self.header.getMap().asSlotsMap();
    }

    pub fn getSizeInMemory(self: *Slots) usize {
        return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
    }

    pub fn asValue(self: *Slots) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    /// Returns a slice of `Value`s for the assignable slots that are after the
    /// Slots object header.
    pub fn getAssignableSlots(self: *Slots) []Value {
        const slots_header_size = @sizeOf(Slots);
        const object_memory = @ptrCast([*]u8, self);
        const assignable_slot_count = self.getMap().getAssignableSlotCount();

        return std.mem.bytesAsSlice(
            Value,
            object_memory[slots_header_size .. slots_header_size + assignable_slot_count * @sizeOf(Value)],
        );
    }

    /// Returns a slice of `Slot`s that exist on the slots map.
    pub fn getSlots(self: *Slots) []Slot {
        return self.getMap().getSlots();
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

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Activation, memory_area);
        self.init(map.asValue(), map_type, assignable_slot_values, receiver);

        return self;
    }

    fn init(
        self: *Activation,
        map: Value,
        comptime map_type: MapType,
        assignable_slot_values: []Value,
        receiver: Value,
    ) void {
        self.slots.init(map, assignable_slot_values);
        self.slots.header.init(.Activation, map);
        self.setActivationType(if (map_type == .Block) ActivationType.Block else ActivationType.Method);

        self.receiver = receiver;
    }

    const ActivationTypeShift = 5;
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

    pub fn asValue(self: *Activation) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
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

    pub fn setArguments(self: *Activation, arguments: []Value) void {
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
        return @sizeOf(Activation) + assignable_slot_count * @sizeOf(Slot);
    }
};

/// A method object. A method object is a slots object with a method map as its
/// parent.
pub const Method = packed struct {
    slots: Slots,

    pub fn create(heap: *Heap, map: *Map.Method, assignable_slot_values: []Value) !*Method {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Method, memory_area);
        self.init(map.asValue(), assignable_slot_values);

        return self;
    }

    fn init(self: *Method, map: Value, assignable_slot_values: []Value) void {
        self.slots.init(map, assignable_slot_values);
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

    pub fn getStatementsSlice(self: *Method) []AST.StatementNode {
        return self.getMap().getStatementsSlice();
    }

    pub fn getAssignableSlotCount(self: *Method) u8 {
        return self.getMap().getAssignableSlotCount();
    }

    pub fn getAssignableSlots(self: *Method) []Value {
        const slots_header_size = @sizeOf(Method);
        const object_memory = @ptrCast([*]u8, self);
        const assignable_slot_count = self.getAssignableSlotCount();

        return std.mem.bytesAsSlice(
            Value,
            object_memory[slots_header_size .. slots_header_size + assignable_slot_count * @sizeOf(Value)],
        );
    }

    /// Creates a method activation object for this block and returns it.
    /// Borrows a ref for `message_script`.
    pub fn activateMethod(
        self: *Method,
        allocator: Allocator,
        heap: *Heap,
        receiver: Value,
        arguments: []Value,
        message_name: []const u8,
        message_range: Range,
        message_script: Script.Ref,
    ) !*RuntimeActivation {
        const message_name_copy = try allocator.dupe(u8, message_name);
        errdefer allocator.free(message_name_copy);

        const activation_object = try Activation.create(heap, .Method, self.slots.header.getMap(), self.getAssignableSlots(), receiver);
        activation_object.setArguments(arguments);
        return try RuntimeActivation.create(allocator, heap, activation_object.asValue(), message_name_copy, message_range, message_script);
    }

    pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return Slots.requiredSizeForAllocation(assignable_slot_count);
    }
};

/// A block object. A block object is a slots object with a block map as its
/// parent.
pub const Block = packed struct {
    slots: Slots,

    pub fn create(heap: *Heap, map: *Map.Block, assignable_slot_values: []Value) !*Block {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len));

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*Block, memory_area);
        self.init(map.asValue(), assignable_slot_values);

        return self;
    }

    fn init(self: *Block, map: Value, assignable_slot_values: []Value) void {
        self.slots.init(map, assignable_slot_values);
        self.slots.header.init(.Block, map);
    }

    pub fn asValue(self: *Block) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
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

    pub fn getStatementsSlice(self: *Block) []AST.StatementNode {
        return self.getMap().getStatementsSlice();
    }

    pub fn getArgumentSlotCount(self: *Block) usize {
        return self.getMap().getArgumentSlotCount();
    }

    pub fn getAssignableSlotCount(self: *Block) u8 {
        return self.getMap().getAssignableSlotCount();
    }

    pub fn getAssignableSlots(self: *Block) []Value {
        return self.slots.getAssignableSlots();
    }

    /// Returns whether the passed message name is the correct one for this block
    /// to be executed. The logic is:
    ///
    /// - For blocks with no arguments, `value`
    /// - For blocks with a single argument, `value:`
    /// - For blocks with more than one argument, `value:With:`, with as many
    ///   `With:`s as needed (number of colons should match number of arguments)
    pub fn isCorrectMessageForBlockExecution(self: *Block, message: []const u8) bool {
        if (self.getArgumentSlotCount() == 0 and std.mem.eql(u8, message, "value")) {
            return true;
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

    fn createMessageNameForBlock(self: *Block, allocator: Allocator) ![]const u8 {
        var needed_space: usize = 5; // value
        if (self.getArgumentSlotCount() > 0) {
            needed_space += 1; // :
            needed_space += 5 * (self.getArgumentSlotCount() - 1); // Any other With:s needed
        }

        var message_name = try allocator.alloc(u8, needed_space);
        std.mem.copy(u8, message_name, "value");

        if (self.getArgumentSlotCount() > 0) {
            message_name[5] = ':';

            var remaining_buffer = message_name[6..];
            while (remaining_buffer.len > 0) {
                std.mem.copy(u8, remaining_buffer, "With:");
                remaining_buffer = remaining_buffer[5..];
            }
        }

        return message_name;
    }

    /// Creates a block activation object for this block and returns it. Borrows
    /// a ref for `message_script`.
    pub fn activateBlock(
        self: *Block,
        allocator: Allocator,
        heap: *Heap,
        receiver: Value,
        arguments: []Value,
        message_range: Range,
        message_script: Script.Ref,
    ) !*RuntimeActivation {
        const message_name = try self.createMessageNameForBlock(allocator);
        errdefer allocator.free(message_name);

        const activation_object = try Activation.create(heap, .Block, self.slots.header.getMap(), self.getAssignableSlots(), receiver);
        activation_object.setArguments(arguments);
        return try RuntimeActivation.create(allocator, heap, activation_object.asValue(), message_name, message_range, message_script);
    }

    pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
        return Slots.requiredSizeForAllocation(assignable_slot_count);
    }
};
