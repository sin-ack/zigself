// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../../language/ast.zig");
const Heap = @import("../Heap.zig");
const Slot = @import("../slot.zig").Slot;
const hash = @import("../../utility/hash.zig");
const Value = value_import.Value;
const Object = @import("../Object.zig");
const bytecode = @import("../bytecode.zig");
const ByteArray = @import("../ByteArray.zig");
const Activation = @import("../Activation.zig");
const MapBuilder = @import("./map_builder.zig").MapBuilder;
const IntegerValue = value_import.IntegerValue;
const PointerValue = value_import.PointerValue;
const value_import = @import("../value.zig");
const stage2_compat = @import("../../utility/stage2_compat.zig");
const RefCountedValue = value_import.RefCountedValue;

const MapTypeShift = Object.ObjectTypeShift + Object.ObjectTypeBits;
const MapTypeBits = 3;
const MapTypeMask: u64 = ((1 << MapTypeBits) - 1) << MapTypeShift;

pub const MapType = enum(u64) {
    Slots = 0b000 << MapTypeShift,
    Method = 0b001 << MapTypeShift,
    Block = 0b010 << MapTypeShift,
    Array = 0b100 << MapTypeShift,
};

/// Generates a method for use as an isXMap function.
fn generateIsMap(comptime MapT: type, comptime map_type: MapType) fn (MapT.Ptr) bool {
    return struct {
        pub fn func(self: MapT.Ptr) bool {
            return self.getMapType() == map_type;
        }
    }.func;
}

/// Generates a method for use as a mustBeXMap function.
fn generateMustBeMap(comptime MapT: type, comptime map_type_name: [*:0]const u8, comptime human_map_type: [*:0]const u8) fn (MapT.Ptr) void {
    const is_map_func_name = "is" ++ map_type_name ++ "Map";

    return struct {
        pub fn func(self: MapT.Ptr) void {
            if (!@call(.{}, @field(self, is_map_func_name), .{})) {
                std.debug.panic("Expected the object at {*} to be " ++ human_map_type, .{self});
            }
        }
    }.func;
}

/// Generates a method for use as an asXMap function.
fn generateAsMap(comptime MapT: type, comptime SubMapT: type, comptime map_type_name: [*:0]const u8) fn (MapT.Ptr) SubMapT.Ptr {
    const must_be_map_func_name = "mustBe" ++ map_type_name ++ "Map";

    return struct {
        pub fn func(self: MapT.Ptr) SubMapT.Ptr {
            if (builtin.mode == .Debug) {
                @call(.{}, @field(self, must_be_map_func_name), .{});
            }

            return @ptrCast(SubMapT.Ptr, self);
        }
    }.func;
}

pub const Map = extern struct {
    header: Object.Header align(@alignOf(u64)),

    pub const Slots = SlotsMap;
    pub const Method = MethodMap;
    pub const Block = BlockMap;
    pub const Array = ArrayMap;

    pub const Ptr = stage2_compat.HeapPtr(Map, .Mutable);

    fn init(self: Map.Ptr, map_type: MapType, map_map: Value) void {
        // NOTE: Maps are immutable, so it's fine to consider all of them as being owned by the global actor.
        self.header.init(.Map, 0, map_map);
        self.setMapType(map_type);
    }

    pub fn getMapType(self: Map.Ptr) MapType {
        const raw_map_type = self.header.object_information & MapTypeMask;
        return std.meta.intToEnum(MapType, raw_map_type) catch |err| switch (err) {
            std.meta.IntToEnumError.InvalidEnumTag => std.debug.panic(
                "Unexpected map type {x} on object at {*}\n",
                .{ raw_map_type >> MapTypeShift, self },
            ),
        };
    }

    pub fn setMapType(self: Map.Ptr, map_type: MapType) void {
        self.header.object_information = (self.header.object_information & ~MapTypeMask) | @enumToInt(map_type);
    }

    pub fn asObjectAddress(self: Map.Ptr) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: Map.Ptr) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn shouldFinalize(self: Map.Ptr) bool {
        return switch (self.getMapType()) {
            .Slots, .Array => false,
            .Method, .Block => true,
        };
    }

    pub fn finalize(self: Map.Ptr, allocator: Allocator) void {
        switch (self.getMapType()) {
            .Slots, .Array => unreachable,
            .Method => self.asMethodMap().finalize(allocator),
            .Block => self.asBlockMap().finalize(allocator),
        }
    }

    pub fn getSizeInMemory(self: Map.Ptr) usize {
        return switch (self.getMapType()) {
            .Slots => self.asSlotsMap().getSizeInMemory(),
            .Method => self.asMethodMap().getSizeInMemory(),
            .Block => self.asBlockMap().getSizeInMemory(),
            .Array => self.asArrayMap().getSizeInMemory(),
        };
    }

    pub fn clone(self: Map.Ptr, token: *Heap.AllocationToken) !Object.Map.Ptr {
        const map_map = self.header.map_pointer;

        return switch (self.getMapType()) {
            .Slots => @ptrCast(Map.Ptr, self.asSlotsMap().clone(map_map, token)),
            .Method => @ptrCast(Map.Ptr, try self.asMethodMap().clone(map_map, token)),
            .Block => @ptrCast(Map.Ptr, try self.asBlockMap().clone(map_map, token)),
            .Array => @ptrCast(Map.Ptr, self.asArrayMap().clone(map_map, token)),
        };
    }

    pub const isSlotsMap = generateIsMap(Map, .Slots);
    pub const isMethodMap = generateIsMap(Map, .Method);
    pub const isBlockMap = generateIsMap(Map, .Block);
    pub const isArrayMap = generateIsMap(Map, .Array);

    pub const mustBeSlotsMap = generateMustBeMap(Map, "Slots", "a slots map");
    pub const mustBeMethodMap = generateMustBeMap(Map, "Method", "a method map");
    pub const mustBeBlockMap = generateMustBeMap(Map, "Block", "a block map");
    pub const mustBeArrayMap = generateMustBeMap(Map, "Array", "an array map");

    pub const asSlotsMap = generateAsMap(Map, Slots, "Slots");
    pub const asMethodMap = generateAsMap(Map, Method, "Method");
    pub const asBlockMap = generateAsMap(Map, Block, "Block");
    pub const asArrayMap = generateAsMap(Map, Array, "Array");
};

/// Return a mixin struct which can be added to slots-like maps with pub
/// usingnamespace.
fn SlotsLikeMapBase(comptime MapT: type) type {
    return struct {
        pub const Ptr = stage2_compat.HeapPtr(MapT, .Mutable);

        fn getSlotMemory(self: MapT.Ptr) stage2_compat.HeapSlice(u8, .Mutable) {
            const total_object_size = getSizeInMemory(self);
            const map_memory = @ptrCast([*]align(@alignOf(u64)) u8, self);
            return map_memory[@sizeOf(MapT)..total_object_size];
        }

        pub fn getSlots(self: MapT.Ptr) Slot.Slice {
            return std.mem.bytesAsSlice(Slot, getSlotMemory(self));
        }

        pub fn asObjectAddress(self: MapT.Ptr) [*]u64 {
            return @ptrCast([*]u64, self);
        }

        pub fn asValue(self: MapT.Ptr) Value {
            return Value.fromObjectAddress(asObjectAddress(self));
        }

        /// Return the amount of assignable slots that this slot map
        /// contains.
        pub fn getAssignableSlotCount(self: MapT.Ptr) u8 {
            // 255 assignable slots ought to be enough for everybody.
            return @intCast(u8, asSlotsMap(self).properties >> 24);
        }

        pub fn setAssignableSlotCount(self: MapT.Ptr, count: u8) void {
            const slots_map = asSlotsMap(self);
            slots_map.properties = (slots_map.properties & @as(u32, 0x00FFFFFF)) | (@as(u32, count) << 24);
        }

        pub fn getSizeInMemory(self: MapT.Ptr) usize {
            return requiredSizeForAllocation(asSlotsMap(self).slot_count);
        }

        /// Return the size required for the whole map with the given slot count.
        pub fn requiredSizeForAllocation(slot_count: u32) usize {
            return @sizeOf(MapT) + slot_count * @sizeOf(Slot);
        }

        fn asSlotsMap(self: MapT.Ptr) SlotsMap.Ptr {
            return @ptrCast(SlotsMap.Ptr, self);
        }

        pub fn getMapBuilder(self: MapT.Ptr, token: *Heap.AllocationToken) MapBuilder(MapT, MapT.ObjectType) {
            return MapBuilder(MapT, MapT.ObjectType).init(token, self);
        }
    };
}

// NOTE: properties comes *before* the slot count in the struct
//       definition, but comes *after* the slot count in the actual bit
//       definitions.
const SlotsMap = extern struct {
    map: Map align(@alignOf(u64)),
    /// Slots map properties.
    /// The first byte is the amount of assignable slots the map has.
    /// The other bytes are currently reserved for future use.
    /// The last two bits are zero.
    properties: u32 align(1),
    /// The amount of slots. The slots begin after the end of this
    /// field.
    slot_count: u32 align(1),

    pub usingnamespace SlotsLikeMapBase(SlotsMap);
    pub const ObjectType = Object.Slots;

    /// Create a new slots map. Takes the amount of slots this object will have.
    ///
    /// IMPORTANT: All slots *must* be initialized right after creation.
    pub fn create(map_map: Value, token: *Heap.AllocationToken, slot_count: u32) SlotsMap.Ptr {
        const size = SlotsMap.requiredSizeForAllocation(slot_count);

        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(SlotsMap.Ptr, memory_area);
        self.init(slot_count, map_map);

        return self;
    }

    fn init(self: SlotsMap.Ptr, slot_count: u32, map_map: Value) void {
        self.map.init(.Slots, map_map);
        self.properties = 0;
        self.slot_count = slot_count;
    }

    pub fn clone(self: SlotsMap.Ptr, map_map: Value, token: *Heap.AllocationToken) SlotsMap.Ptr {
        const new_map = create(map_map, token, self.slot_count);

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        std.mem.copy(Slot, new_map.getSlots(), self.getSlots());

        return new_map;
    }

    /// Create the special map-map object.
    pub fn createMapMap(token: *Heap.AllocationToken) Value {
        // Fake it 'til you make it.
        var imaginary_map_pointer = Value.fromObjectAddress(@as([*]u64, undefined));

        var map_map = create(imaginary_map_pointer, token, 0);
        // FIXME: This is kinda crude. Let's give ourselves a way to set this
        //        after-the-fact without reaching into the header of the object.
        map_map.map.header.map_pointer = map_map.asValue();

        return map_map.asValue();
    }
};

/// Common code and fields shared between methods and blocks.
const SlotsAndBytecodeMap = extern struct {
    slots_map: SlotsMap align(@alignOf(u64)),
    /// The address of the bytecode block. Owned by definition_executable_ref.
    block: PointerValue(bytecode.Block) align(@alignOf(u64)),
    /// The executable which this map was created from.
    definition_executable_ref: RefCountedValue(bytecode.Executable) align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(SlotsAndBytecodeMap, .Mutable);

    /// Refs `script`.
    fn init(
        self: SlotsAndBytecodeMap.Ptr,
        comptime map_type: MapType,
        map_map: Value,
        argument_slot_count: u8,
        total_slot_count: u32,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) void {
        std.debug.assert(argument_slot_count <= total_slot_count);

        self.slots_map.init(total_slot_count, map_map);
        self.slots_map.map.init(map_type, map_map);
        self.setArgumentSlotCount(argument_slot_count);

        self.block = PointerValue(bytecode.Block).init(block);
        self.definition_executable_ref = RefCountedValue(bytecode.Executable).init(executable);
    }

    /// Finalizes this object. All maps that have a SlotsAndBytecodeMap member
    /// must call this function in their finalize.
    pub fn finalize(self: SlotsAndBytecodeMap.Ptr, allocator: Allocator) void {
        _ = allocator;
        self.definition_executable_ref.deinit();
    }

    pub fn getArgumentSlotCount(self: SlotsAndBytecodeMap.Ptr) u8 {
        return @intCast(u8, (self.slots_map.properties >> 16) & @as(u64, 0xFF));
    }

    fn setArgumentSlotCount(self: SlotsAndBytecodeMap.Ptr, count: u8) void {
        self.slots_map.properties = (self.slots_map.properties & @as(u32, 0xFF00FFFF)) | (@as(u32, count) << 16);
    }
};

/// A map for a method. A method object is a slots object which has two separate
/// slot sections for argument slots and regular slots defined on the method
/// respectively. It also contains a pointer to the actual set of statements to
/// be executed. Finally, some debug info is stored which is then displayed in
/// stack traces.
const MethodMap = extern struct {
    base_map: SlotsAndBytecodeMap align(@alignOf(u64)),
    /// What the method is called.
    method_name: Value align(@alignOf(u64)),

    pub usingnamespace SlotsLikeMapBase(MethodMap);
    pub const ObjectType = Object.Method;

    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    pub fn create(
        map_map: Value,
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
        map_map: Value,
        argument_slot_count: u8,
        total_slot_count: u32,
        is_inline_method: bool,
        method_name: ByteArray,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) void {
        self.base_map.init(.Method, map_map, argument_slot_count, total_slot_count, block, executable);
        self.method_name = method_name.asValue();
        self.setInlineMethod(is_inline_method);
    }

    const InlineShift = MapTypeShift + MapTypeBits;
    const InlineBit: u64 = 1 << InlineShift;

    fn setInlineMethod(self: MethodMap.Ptr, is_inline_method: bool) void {
        var object_info = self.base_map.slots_map.map.header.object_information;
        if (is_inline_method)
            object_info |= InlineBit
        else
            object_info &= ~InlineBit;
        self.base_map.slots_map.map.header.object_information = object_info;
    }

    fn isInlineMethod(self: MethodMap.Ptr) bool {
        return self.base_map.slots_map.map.header.object_information & InlineBit != 0;
    }

    pub fn expectsActivationObjectAsReceiver(self: MethodMap.Ptr) bool {
        return self.isInlineMethod();
    }

    pub fn finalize(self: MethodMap.Ptr, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    pub fn getArgumentSlotCount(self: MethodMap.Ptr) u8 {
        return self.base_map.getArgumentSlotCount();
    }

    pub fn clone(self: MethodMap.Ptr, map_map: Value, token: *Heap.AllocationToken) !MethodMap.Ptr {
        const new_map = try create(
            map_map,
            token,
            self.getArgumentSlotCount(),
            self.base_map.slots_map.slot_count,
            self.isInlineMethod(),
            self.method_name.asByteArray(),
            self.base_map.block.get(),
            self.base_map.definition_executable_ref.get(),
        );

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        std.mem.copy(Slot, new_map.getSlots(), self.getSlots());

        return new_map;
    }
};

/// A map for a block object. A block object is a slots + statements object
/// which can be defined in a method and then executed later. The block must be
/// executed while the method in which it is created is still on the activation
/// stack.
const BlockMap = extern struct {
    base_map: SlotsAndBytecodeMap align(@alignOf(u64)),
    /// A weak reference to the parent activation of this block. The block must
    /// not be activated if this activation has left the stack.
    parent_activation: Activation.ActivationRef align(@alignOf(u64)),
    /// A weak reference to the non-local return target activation of this
    /// block. If a non-local return happens inside this block, then it will
    /// target this activation.
    nonlocal_return_target_activation: Activation.ActivationRef align(@alignOf(u64)),

    pub usingnamespace SlotsLikeMapBase(BlockMap);
    pub const ObjectType = Object.Block;

    /// Borrows a ref for `script` from the caller. Takes ownership of
    /// `statements`.
    pub fn create(
        map_map: Value,
        token: *Heap.AllocationToken,
        argument_slot_count: u8,
        total_slot_count: u32,
        parent_activation: Activation.ActivationRef,
        nonlocal_return_target_activation: Activation.ActivationRef,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) !BlockMap.Ptr {
        const size = BlockMap.requiredSizeForAllocation(total_slot_count);

        var memory_area = token.allocate(.Object, size);
        var self = @ptrCast(BlockMap.Ptr, memory_area);
        self.init(map_map, argument_slot_count, total_slot_count, parent_activation, nonlocal_return_target_activation, block, executable);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(
        self: BlockMap.Ptr,
        map_map: Value,
        argument_slot_count: u8,
        total_slot_count: u32,
        parent_activation: Activation.ActivationRef,
        nonlocal_return_target_activation: Activation.ActivationRef,
        block: *bytecode.Block,
        executable: bytecode.Executable.Ref,
    ) void {
        self.base_map.init(.Block, map_map, argument_slot_count, total_slot_count, block, executable);
        self.parent_activation = parent_activation;
        self.nonlocal_return_target_activation = nonlocal_return_target_activation;
    }

    pub fn finalize(self: BlockMap.Ptr, allocator: Allocator) void {
        self.base_map.finalize(allocator);
    }

    pub fn getArgumentSlotCount(self: BlockMap.Ptr) u8 {
        return self.base_map.getArgumentSlotCount();
    }

    pub fn clone(self: BlockMap.Ptr, map_map: Value, token: *Heap.AllocationToken) !BlockMap.Ptr {
        const new_map = try create(
            map_map,
            token,
            self.getArgumentSlotCount(),
            self.base_map.slots_map.slot_count,
            self.parent_activation,
            self.nonlocal_return_target_activation,
            self.base_map.block.get(),
            self.base_map.definition_executable_ref.get(),
        );

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        std.mem.copy(Slot, new_map.getSlots(), self.getSlots());

        return new_map;
    }
};

/// A map for an array object.
const ArrayMap = extern struct {
    map: Map align(@alignOf(u64)),
    size: IntegerValue(.Unsigned) align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(ArrayMap, .Mutable);

    pub fn create(map_map: Value, token: *Heap.AllocationToken, size: usize) ArrayMap.Ptr {
        const memory_size = requiredSizeForAllocation();

        var memory_area = token.allocate(.Object, memory_size);
        var self = @ptrCast(ArrayMap.Ptr, memory_area);
        self.init(map_map, size);

        return self;
    }

    fn init(self: ArrayMap.Ptr, map_map: Value, size: usize) void {
        self.map.init(.Array, map_map);
        self.size = IntegerValue(.Unsigned).init(@as(u64, size));
    }

    pub fn asValue(self: ArrayMap.Ptr) Value {
        return Value.fromObjectAddress(@ptrCast([*]u64, @alignCast(@alignOf(u64), self)));
    }

    pub fn getSize(self: ArrayMap.Ptr) usize {
        return @intCast(usize, self.size.get());
    }

    pub fn getSizeInMemory(self: ArrayMap.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ArrayMap);
    }

    pub fn clone(self: ArrayMap.Ptr, map_map: Value, token: *Heap.AllocationToken) ArrayMap.Ptr {
        return create(map_map, token, self.getSize());
    }
};
