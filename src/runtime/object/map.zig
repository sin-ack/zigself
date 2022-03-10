// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../../language/ast.zig");
const Heap = @import("../heap.zig");
const Slot = @import("../slot.zig").Slot;
const hash = @import("../../utility/hash.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Script = @import("../../language/script.zig");
// Zig's shadowing rules are annoying.
const ByteArrayTheFirst = @import("../byte_array.zig");
const Activation = @import("../activation.zig");
const MapBuilder = @import("./map_builder.zig").MapBuilder;

var static_map_map: ?Value = null;

pub fn getMapMap(heap: *Heap) !Value {
    if (static_map_map) |m| return m;

    var new_map = try heap.allocateInObjectSegment(@sizeOf(SlotsMap));

    // FIXME: Clean this up
    var header = @ptrCast(*Object.Header, new_map);
    header.object_information = 0b11;
    header.setObjectType(.Map);
    var map = @ptrCast(*Map, header);
    map.setMapType(.Slots);
    var slots_map = map.asSlotsMap();
    slots_map.properties = 0;
    slots_map.slot_count = 0;

    var map_value = Value.fromObjectAddress(new_map);
    header.map_pointer = map_value;
    static_map_map = map_value;
    return map_value;
}

const MapTypeShift = Object.ObjectTypeShift + Object.ObjectTypeBits;
const MapTypeBits = 3;
const MapTypeMask: u64 = ((1 << MapTypeBits) - 1) << MapTypeShift;

pub const MapType = enum(u64) {
    Slots = 0b000 << MapTypeShift,
    Method = 0b001 << MapTypeShift,
    Block = 0b010 << MapTypeShift,
    ByteArray = 0b011 << MapTypeShift,
    Array = 0b100 << MapTypeShift,
};

/// Generates a method for use as an isXMap function.
fn generateIsMap(comptime MapT: type, comptime map_type: MapType) fn (*MapT) bool {
    return struct {
        pub fn func(self: *MapT) bool {
            return self.getMapType() == map_type;
        }
    }.func;
}

/// Generates a method for use as a mustBeXMap function.
fn generateMustBeMap(comptime MapT: type, comptime map_type_name: [*:0]const u8, comptime human_map_type: [*:0]const u8) fn (*MapT) void {
    const is_map_func_name = "is" ++ map_type_name ++ "Map";

    return struct {
        pub fn func(self: *MapT) void {
            if (!@call(.{}, @field(self, is_map_func_name), .{})) {
                std.debug.panic("Expected the object at {*} to be " ++ human_map_type, .{self});
            }
        }
    }.func;
}

/// Generates a method for use as an asXMap function.
fn generateAsMap(comptime MapT: type, comptime SubMapT: type, comptime map_type_name: [*:0]const u8) fn (*MapT) *SubMapT {
    const must_be_map_func_name = "mustBe" ++ map_type_name ++ "Map";

    return struct {
        pub fn func(self: *MapT) *SubMapT {
            if (builtin.mode == .Debug) {
                @call(.{}, @field(self, must_be_map_func_name), .{});
            }

            return @ptrCast(*SubMapT, self);
        }
    }.func;
}

pub const Map = packed struct {
    header: Object.Header,

    pub const Slots = SlotsMap;
    pub const Method = MethodMap;
    pub const Block = BlockMap;
    pub const ByteArray = ByteArrayMap;
    pub const Array = ArrayMap;

    fn init(self: *Map, map_type: MapType, map_map: Value) void {
        self.header.init(.Map, map_map);
        self.setMapType(map_type);
    }

    pub fn getMapType(self: *Map) MapType {
        const raw_map_type = self.header.object_information & MapTypeMask;
        return std.meta.intToEnum(MapType, raw_map_type) catch |err| switch (err) {
            std.meta.IntToEnumError.InvalidEnumTag => std.debug.panic(
                "Unexpected map type {x} on object at {*}\n",
                .{ raw_map_type >> MapTypeShift, self },
            ),
        };
    }

    pub fn setMapType(self: *Map, map_type: MapType) void {
        self.header.object_information = (self.header.object_information & ~MapTypeMask) | @enumToInt(map_type);
    }

    pub fn asValue(self: *Map) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn shouldFinalize(self: *Map) bool {
        return switch (self.getMapType()) {
            .Slots, .Array, .ByteArray => false,
            .Method, .Block => true,
        };
    }

    pub fn finalize(self: *Map, allocator: Allocator) void {
        switch (self.getMapType()) {
            .Slots, .Array, .ByteArray => unreachable,
            .Method => self.asMethodMap().finalize(allocator),
            .Block => self.asBlockMap().finalize(allocator),
        }
    }

    pub fn getSizeInMemory(self: *Map) usize {
        return switch (self.getMapType()) {
            .Slots => self.asSlotsMap().getSizeInMemory(),
            .Method => self.asMethodMap().getSizeInMemory(),
            .Block => self.asBlockMap().getSizeInMemory(),
            .ByteArray => self.asByteArrayMap().getSizeInMemory(),
            .Array => self.asArrayMap().getSizeInMemory(),
        };
    }

    pub const isSlotsMap = generateIsMap(Map, .Slots);
    pub const isMethodMap = generateIsMap(Map, .Method);
    pub const isBlockMap = generateIsMap(Map, .Block);
    pub const isByteArrayMap = generateIsMap(Map, .ByteArray);
    pub const isArrayMap = generateIsMap(Map, .Array);

    pub const mustBeSlotsMap = generateMustBeMap(Map, "Slots", "a slots map");
    pub const mustBeMethodMap = generateMustBeMap(Map, "Method", "a method map");
    pub const mustBeBlockMap = generateMustBeMap(Map, "Block", "a block map");
    pub const mustBeByteArrayMap = generateMustBeMap(Map, "ByteArray", "a byte array map");
    pub const mustBeArrayMap = generateMustBeMap(Map, "Array", "an array map");

    pub const asSlotsMap = generateAsMap(Map, Slots, "Slots");
    pub const asMethodMap = generateAsMap(Map, Method, "Method");
    pub const asBlockMap = generateAsMap(Map, Block, "Block");
    pub const asByteArrayMap = generateAsMap(Map, ByteArray, "ByteArray");
    pub const asArrayMap = generateAsMap(Map, Array, "Array");
};

/// Return a mixin struct which can be added to slots-like maps with pub
/// usingnamespace.
fn SlotsLikeMapBase(comptime MapT: type) type {
    return struct {
        fn getSlotMemory(self: *MapT) []u8 {
            const total_object_size = getSizeInMemory(self);
            const map_memory = @ptrCast([*]u8, self);
            return map_memory[@sizeOf(MapT)..total_object_size];
        }

        pub fn getSlots(self: *MapT) []Slot {
            return std.mem.bytesAsSlice(Slot, getSlotMemory(self));
        }

        pub fn asObjectAddress(self: *MapT) [*]u64 {
            return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
        }

        pub fn asValue(self: *MapT) Value {
            return Value.fromObjectAddress(asObjectAddress(self));
        }

        /// Return the amount of assignable slots that this slot map
        /// contains.
        pub fn getAssignableSlotCount(self: *MapT) u8 {
            // 255 assignable slots ought to be enough for everybody.
            return @intCast(u8, asSlotsMap(self).properties >> 24);
        }

        pub fn setAssignableSlotCount(self: *MapT, count: u8) void {
            const slots_map = asSlotsMap(self);
            slots_map.properties = (slots_map.properties & @as(u32, 0x00FFFFFF)) | (@as(u32, count) << 24);
        }

        pub fn getSizeInMemory(self: *MapT) usize {
            return requiredSizeForAllocation(asSlotsMap(self).slot_count);
        }

        /// Return the size required for the whole map with the given slot count.
        pub fn requiredSizeForAllocation(slot_count: u32) usize {
            return @sizeOf(MapT) + slot_count * @sizeOf(Slot);
        }

        fn asSlotsMap(self: *MapT) *SlotsMap {
            return @ptrCast(*SlotsMap, self);
        }

        pub fn getMapBuilder(self: *MapT, heap: *Heap) !MapBuilder(MapT, MapT.ObjectType) {
            return try MapBuilder(MapT, MapT.ObjectType).init(heap, self);
        }
    };
}

// NOTE: properties comes *before* the slot count in the struct
//       definition, but comes *after* the slot count in the actual bit
//       definitions.
const SlotsMap = packed struct {
    map: Map,
    /// Slots map properties.
    /// The first byte is the amount of assignable slots the map has.
    /// The other bytes are currently reserved for future use.
    /// The last two bits are zero.
    properties: u32,
    /// The amount of slots. The slots begin after the end of this
    /// field.
    slot_count: u32,

    pub usingnamespace SlotsLikeMapBase(SlotsMap);
    pub const ObjectType = Object.Slots;

    /// Create a new slots map. Takes the amount of slots this object will have.
    ///
    /// IMPORTANT: All slots *must* be initialized right after creation.
    pub fn create(heap: *Heap, slot_count: u32) !*SlotsMap {
        const size = SlotsMap.requiredSizeForAllocation(slot_count);
        const map_map = try getMapMap(heap);

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*SlotsMap, memory_area);
        self.init(slot_count, map_map);

        return self;
    }

    fn init(self: *SlotsMap, slot_count: u32, map_map: Value) void {
        self.map.init(.Slots, map_map);
        self.properties = 0;
        self.slot_count = slot_count;
    }
};

/// Common code and fields shared between methods and blocks.
const SlotsAndStatementsMap = packed struct {
    slots_map: SlotsMap,
    /// The address of the ref-counted statement slice.
    statements_ref: Value,
    /// Which script this method or block is defined in.
    script_ref: Value,

    fn init(
        self: *SlotsAndStatementsMap,
        comptime map_type: MapType,
        map_map: Value,
        argument_slot_count: u8,
        total_slot_count: u32,
        statements: AST.StatementList.Ref,
        script: Script.Ref,
    ) void {
        std.debug.assert(argument_slot_count <= total_slot_count);

        self.slots_map.init(total_slot_count, map_map);
        self.slots_map.map.init(map_type, map_map);
        self.setArgumentSlotCount(argument_slot_count);

        self.statements_ref = Value.fromUnsignedInteger(@ptrToInt(statements.value));
        self.script_ref = Value.fromUnsignedInteger(@ptrToInt(script.value));
    }

    /// Finalizes this object. All maps that have a SlotsAndStatementsMap member
    /// must call this function in their finalize.
    pub fn finalize(self: *SlotsAndStatementsMap, allocator: Allocator) void {
        self.getStatements().unrefWithAllocator(allocator);
        self.getDefinitionScript().unref();
    }

    pub fn getDefinitionScript(self: *SlotsAndStatementsMap) Script.Ref {
        return Script.Ref{ .value = @intToPtr(*Script, self.script_ref.asUnsignedInteger()) };
    }

    pub fn getStatementsSlice(self: *SlotsAndStatementsMap) []AST.ExpressionNode {
        return self.getStatements().value.statements;
    }

    pub fn getArgumentSlotCount(self: *SlotsAndStatementsMap) u8 {
        return @intCast(u8, (self.slots_map.properties >> 16) & @as(u64, 0xFF));
    }

    fn setArgumentSlotCount(self: *SlotsAndStatementsMap, count: u8) void {
        self.slots_map.properties = (self.slots_map.properties & @as(u32, 0xFF00FFFF)) | (@as(u32, count) << 16);
    }

    fn getStatements(self: *SlotsAndStatementsMap) AST.StatementList.Ref {
        return AST.StatementList.Ref{ .value = @intToPtr(*AST.StatementList, self.statements_ref.asUnsignedInteger()) };
    }
};

/// A map for a method. A method object is a slots object which has two separate
/// slot sections for argument slots and regular slots defined on the method
/// respectively. It also contains a pointer to the actual set of statements to
/// be executed. Finally, some debug info is stored which is then displayed in
/// stack traces.
const MethodMap = packed struct {
    base_map: SlotsAndStatementsMap,
    /// What the method is called.
    method_name: Value,

    pub usingnamespace SlotsLikeMapBase(MethodMap);
    pub const ObjectType = Object.Method;

    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    pub fn create(
        heap: *Heap,
        argument_slot_count: u8,
        total_slot_count: u32,
        statements: AST.StatementList.Ref,
        method_name: ByteArrayTheFirst,
        script: Script.Ref,
    ) !*MethodMap {
        const size = MethodMap.requiredSizeForAllocation(total_slot_count);
        const map_map = try getMapMap(heap);

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*MethodMap, memory_area);
        self.init(map_map, argument_slot_count, total_slot_count, statements, method_name, script);

        try heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(
        self: *MethodMap,
        map_map: Value,
        argument_slot_count: u8,
        total_slot_count: u32,
        statements: AST.StatementList.Ref,
        method_name: ByteArrayTheFirst,
        script: Script.Ref,
    ) void {
        self.base_map.init(.Method, map_map, argument_slot_count, total_slot_count, statements, script);
        self.method_name = method_name.asValue();
    }

    pub fn finalize(self: *MethodMap, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    pub fn getDefinitionScript(self: *MethodMap) Script.Ref {
        return self.base_map.getDefinitionScript();
    }

    pub fn getStatementsSlice(self: *MethodMap) []AST.ExpressionNode {
        return self.base_map.getStatementsSlice();
    }

    pub fn getArgumentSlotCount(self: *MethodMap) u8 {
        return self.base_map.getArgumentSlotCount();
    }
};

/// A map for a block object. A block object is a slots + statements object
/// which can be defined in a method and then executed later. The block must be
/// executed while the method in which it is created is still on the activation
/// stack.
const BlockMap = packed struct {
    base_map: SlotsAndStatementsMap,
    /// A weak reference to the parent activation of this block. The block must
    /// not be activated if this activation has left the stack.
    parent_activation: Activation.ActivationRef,
    /// A weak reference to the non-local return target activation of this
    /// block. If a non-local return happens inside this block, then it will
    /// target this activation.
    nonlocal_return_target_activation: Activation.ActivationRef,

    pub usingnamespace SlotsLikeMapBase(BlockMap);
    pub const ObjectType = Object.Block;

    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    pub fn create(
        heap: *Heap,
        argument_slot_count: u8,
        total_slot_count: u32,
        statements: AST.StatementList.Ref,
        parent_activation: *Activation,
        nonlocal_return_target_activation: *Activation,
        script: Script.Ref,
    ) !*BlockMap {
        const size = BlockMap.requiredSizeForAllocation(total_slot_count);
        const map_map = try getMapMap(heap);

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*BlockMap, memory_area);
        self.init(map_map, argument_slot_count, total_slot_count, statements, parent_activation, nonlocal_return_target_activation, script);

        try heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(
        self: *BlockMap,
        map_map: Value,
        argument_slot_count: u8,
        total_slot_count: u32,
        statements: AST.StatementList.Ref,
        parent_activation: *Activation,
        nonlocal_return_target_activation: *Activation,
        script: Script.Ref,
    ) void {
        self.base_map.init(.Block, map_map, argument_slot_count, total_slot_count, statements, script);
        self.parent_activation = parent_activation.takeRef();
        self.nonlocal_return_target_activation = nonlocal_return_target_activation.takeRef();
    }

    pub fn finalize(self: *BlockMap, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    pub fn getDefinitionScript(self: *BlockMap) Script.Ref {
        return self.base_map.getDefinitionScript();
    }

    pub fn getStatementsSlice(self: *BlockMap) []AST.ExpressionNode {
        return self.base_map.getStatementsSlice();
    }

    pub fn getArgumentSlotCount(self: *BlockMap) u8 {
        return self.base_map.getArgumentSlotCount();
    }
};

// A byte array map. A simple map holding a reference to the byte array.
const ByteArrayMap = packed struct {
    map: Map,
    /// A reference to the byte array in question.
    byte_array: Value,

    pub fn create(heap: *Heap, byte_array: ByteArrayTheFirst) !*ByteArrayMap {
        const size = requiredSizeForAllocation();
        const map_map = try getMapMap(heap);

        var memory_area = try heap.allocateInObjectSegment(size);
        var self = @ptrCast(*ByteArrayMap, memory_area);
        self.init(map_map, byte_array);

        return self;
    }

    fn init(self: *ByteArrayMap, map_map: Value, byte_array: ByteArrayTheFirst) void {
        self.map.init(.ByteArray, map_map);
        self.byte_array = byte_array.asValue();
    }

    pub fn asValue(self: *ByteArrayMap) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getByteArray(self: *ByteArrayMap) ByteArrayTheFirst {
        return ByteArrayTheFirst.fromAddress(self.byte_array.asObjectAddress());
    }

    pub fn getValues(self: *ByteArrayMap) []u8 {
        return self.getByteArray().getValues();
    }

    pub fn getSizeInMemory(self: *ByteArrayMap) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ByteArrayMap);
    }
};

/// A map for an array object.
const ArrayMap = packed struct {
    map: Map,
    size: Value,

    pub fn create(heap: *Heap, size: usize) !*ArrayMap {
        const memory_size = requiredSizeForAllocation();
        const map_map = try getMapMap(heap);

        var memory_area = try heap.allocateInObjectSegment(memory_size);
        var self = @ptrCast(*ArrayMap, memory_area);
        self.init(map_map, size);

        return self;
    }

    fn init(self: *ArrayMap, map_map: Value, size: usize) void {
        self.map.init(.Array, map_map);
        self.size = Value.fromUnsignedInteger(size);
    }

    pub fn asValue(self: *ArrayMap) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getSize(self: *ArrayMap) usize {
        return self.size.asUnsignedInteger();
    }

    pub fn getSizeInMemory(self: *ArrayMap) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ArrayMap);
    }
};
