// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const hash = @import("../utility/hash.zig");
const Heap = @import("./Heap.zig");
const debug = @import("../debug.zig");
const Object = @import("./Object.zig");
const Range = @import("../language/Range.zig");
const RefPtr = @import("../utility/ref_counted.zig").RefPtr;
const ByteArray = @import("./ByteArray.zig");
const Completion = @import("./Completion.zig");
const object_lookup = @import("./object/lookup.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const InterpreterContext = @import("./interpreter.zig").InterpreterContext;

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

const LookupResult = object_lookup.LookupResult;
const SelectorHash = object_lookup.SelectorHash;
const parent_hash = hash.stringHash("parent");

pub const Value = packed struct {
    data: u64,

    pub const ValueMarkerMask: u64 = 0b11;

    pub const ValueType = enum(u64) {
        Integer = 0b00,
        ObjectReference = 0b01,
        FloatingPoint = 0b10,
        ObjectMarker = 0b11,
    };

    /// Create a new Value object from the given object address.
    pub inline fn fromObjectAddress(address: [*]u64) Value {
        // Must be 8-byte aligned
        std.debug.assert((@ptrToInt(address) & 0b111) == 0);
        return .{ .data = @ptrToInt(address) | @enumToInt(ValueType.ObjectReference) };
    }

    /// Create a new Value object from an integer literal.
    pub inline fn fromInteger(integer: i64) Value {
        std.debug.assert((@as(i64, -1) << 62) < integer and integer < (@as(i64, 1) << 62));
        return .{ .data = @bitCast(u64, integer << 2) | @enumToInt(ValueType.Integer) };
    }

    /// Create a new Value object from an unsigned integer literal.
    pub inline fn fromUnsignedInteger(integer: u64) Value {
        std.debug.assert(integer < (@as(u64, 1) << 62));
        return .{ .data = (integer << 2) | @enumToInt(ValueType.Integer) };
    }

    pub inline fn fromFloatingPoint(floating_point: f64) Value {
        return .{ .data = (@bitCast(u64, floating_point) & ~ValueMarkerMask) | @enumToInt(ValueType.FloatingPoint) };
    }

    /// Return the type of this value.
    pub inline fn getType(self: Value) ValueType {
        return @intToEnum(ValueType, self.data & ValueMarkerMask);
    }

    /// Return whether this value is an integer.
    pub inline fn isInteger(self: Value) bool {
        return self.getType() == .Integer;
    }

    /// Return whether this value is an object reference.
    pub inline fn isObjectReference(self: Value) bool {
        return self.getType() == .ObjectReference;
    }

    /// Return whether this value is a floating point number.
    pub inline fn isFloatingPoint(self: Value) bool {
        return self.getType() == .FloatingPoint;
    }

    /// Return this value as an integer.
    pub inline fn asInteger(self: Value) i64 {
        std.debug.assert(self.isInteger());
        return @bitCast(i64, self.data) >> 2;
    }

    /// Return this value as an unsigned integer.
    pub inline fn asUnsignedInteger(self: Value) u64 {
        std.debug.assert(self.isInteger());
        return self.data >> 2;
    }

    /// Return this value as a floating point number.
    pub inline fn asFloatingPoint(self: Value) f64 {
        std.debug.assert(self.isFloatingPoint());
        return @bitCast(f64, self.data & ~ValueMarkerMask);
    }

    /// Return the object address stored in this object as a pointer.
    pub inline fn asObjectAddress(self: Value) [*]u64 {
        std.debug.assert(self.isObjectReference());
        return @intToPtr([*]u64, self.data & ~ValueMarkerMask);
    }

    /// Return the object the address of which is stored in this value.
    pub inline fn asObject(self: Value) Object {
        std.debug.assert(self.isObjectReference());
        return Object.fromAddress(self.asObjectAddress());
    }

    /// Return the byte vector the address of which is stored in this value.
    pub inline fn asByteArray(self: Value) ByteArray {
        std.debug.assert(self.isObjectReference());
        return ByteArray.fromAddress(self.asObjectAddress());
    }

    pub fn lookup(
        self: Value,
        vm: *VirtualMachine,
        selector: []const u8,
    ) LookupResult {
        const selector_hash = SelectorHash.init(selector);
        if (LOOKUP_DEBUG) std.debug.print("Value.lookup: Looking up \"{s}\" (hash: {x}) on {}\n", .{ selector, selector_hash.regular, self });

        return self.lookupByHash(vm, selector_hash);
    }

    pub fn lookupByHash(
        self: Value,
        vm: *VirtualMachine,
        selector_hash: SelectorHash,
    ) LookupResult {
        return switch (self.getType()) {
            .ObjectMarker => unreachable,
            .ObjectReference => self.asObject().lookupByHash(vm, selector_hash),

            .Integer => {
                if (LOOKUP_DEBUG) std.debug.print("Value.lookupByHash: Looking up on traits integer\n", .{});
                const integer_traits = vm.integer_traits.getValue();
                if (selector_hash.regular == parent_hash)
                    return LookupResult{ .Regular = integer_traits };

                return integer_traits.lookupByHash(vm, selector_hash);
            },
            .FloatingPoint => {
                if (LOOKUP_DEBUG) std.debug.print("Value.lookupByHash: Looking up on traits float\n", .{});

                const float_traits = vm.float_traits.getValue();
                if (selector_hash.regular == parent_hash)
                    return LookupResult{ .Regular = float_traits };

                return float_traits.lookupByHash(vm, selector_hash);
            },
        };
    }

    /// Clones this value and returns a copy of it.
    pub fn clone(self: Value, token: *Heap.AllocationToken, actor_id: u31) Value {
        return switch (self.getType()) {
            .ObjectMarker => unreachable,
            .Integer, .FloatingPoint => Value{ .data = self.data },
            // NOTE: The only error condition that can happen here is during method and block map cloning.
            //       Since user code is unable to do this, there is no reason to propagate a try here.
            .ObjectReference => (self.asObject().clone(token, actor_id) catch unreachable).asValue(),
        };
    }
};

/// A value which stores a pointer to off-heap memory.
pub fn PointerValue(comptime T: type) type {
    return PointerValueAlignment(T, null);
}

pub fn PointerValueAlignment(comptime T: type, comptime alignment: ?u29) type {
    const PointerT = if (alignment) |a| *align(a) T else *T;

    return packed struct {
        value: Value,

        const Self = @This();

        pub fn init(value: PointerT) Self {
            return .{ .value = Value.fromUnsignedInteger(@ptrToInt(value)) };
        }

        pub fn get(self: Self) PointerT {
            return @intToPtr(PointerT, self.value.asUnsignedInteger());
        }
    };
}

/// A value which stores a pointer to off-heap ref-counted memory.
pub fn RefCountedValue(comptime T: type) type {
    const RefT = RefPtr(T);
    const PointerT = PointerValue(T);

    return packed struct {
        value: PointerT,

        const Self = @This();

        pub fn init(ref_counted_value: RefT) Self {
            ref_counted_value.ref();
            return .{ .value = PointerT.init(ref_counted_value.value) };
        }

        /// Use if deinit requires no arguments.
        pub fn deinit(self: Self) void {
            self.get().unref();
        }

        /// Use if deinit requires the allocator.
        pub fn deinitWithAllocator(self: Self, allocator: Allocator) void {
            self.get().unrefWithAllocator(allocator);
        }

        pub fn get(self: Self) RefT {
            return RefT{ .value = self.value.get() };
        }
    };
}

pub const IntegerValueSignedness = enum { Signed, Unsigned };
/// A value which is known to be an integer.
pub fn IntegerValue(comptime signedness: IntegerValueSignedness) type {
    // FIXME: Instead of using a magic value, obtain from Value
    return IntegerValueAdvanced(if (signedness == .Signed) i62 else u62);
}

pub fn IntegerValueAdvanced(comptime IntegerT: type) type {
    const integer_type_info = @typeInfo(IntegerT).Int;
    // FIXME: Instead of using a magic value, obtain from Value
    if (integer_type_info.bits > 62) {
        @compileError("bits > 62 isn't supported by IntegerValueAdvanced");
    }

    const init_function = switch (integer_type_info.signedness) {
        .signed => Value.fromInteger,
        .unsigned => Value.fromUnsignedInteger,
    };
    const conversion_function = switch (integer_type_info.signedness) {
        .signed => Value.asInteger,
        .unsigned => Value.asUnsignedInteger,
    };

    return packed struct {
        value: Value,

        const Self = @This();

        pub fn init(value: IntegerT) Self {
            return .{ .value = init_function(value) };
        }

        pub fn get(self: Self) IntegerT {
            if (builtin.mode == .Debug) {
                if (!self.value.isInteger()) {
                    @panic("!!! IntegerValue does not contain an integer!");
                }
            }

            return @intCast(IntegerT, conversion_function(self.value));
        }
    };
}

/// A value which is of a known object type. Attempting to get the value as an
/// object when something else is stored is a runtime panic in debug, and
/// undefined behavior in release mode.
pub fn ObjectValue(comptime ObjectT: type, comptime is_fn: []const u8, comptime nullable: bool) type {
    return packed struct {
        value: Value,

        const Self = @This();
        const NullValue = Value.fromUnsignedInteger(0);

        pub usingnamespace if (!nullable) struct {
            pub fn init(ptr: ObjectT.Ptr) Self {
                return .{ .value = ptr.asValue() };
            }

            pub fn get(self: Self) ObjectT.Ptr {
                if (builtin.mode == .Debug) {
                    if (!(self.value.isObjectReference() and @call(.{}, @field(self.value.asObject(), is_fn), .{}))) {
                        @panic("!!! " ++ is_fn ++ " check failed on object!");
                    }
                }

                return @ptrCast(ObjectT.Ptr, self.value.asObjectAddress());
            }
        } else struct {
            pub fn init(ptr: ?ObjectT.Ptr) Self {
                return .{ .value = if (ptr) |p| p.asValue() else NullValue };
            }

            pub fn get(self: Self) ?ObjectT.Ptr {
                if (builtin.mode == .Debug) {
                    if (self.value != NullValue and !(self.value.isObjectReference() and @call(.{}, @field(self.value.asObject(), is_fn), .{}))) {
                        @panic("!!! " ++ is_fn ++ " check failed on object!");
                    }
                }

                if (self.value == NullValue)
                    return @as(?ObjectT.Ptr, null);
                return @ptrCast(?ObjectT.Ptr, self.value.asObjectAddress());
            }
        };
    };
}

pub const ActorValue = ObjectValue(Object.Actor, "isActorObject", false);
pub const MethodValue = ObjectValue(Object.Method, "isMethodObject", false);
pub const ManagedValue = ObjectValue(Object.Managed, "isManagedObject", false);
pub const ByteArrayValue = ObjectValue(Object.ByteArray, "isByteArrayObject", false);
pub const ActivationValue = ObjectValue(Object.Activation, "isActivationObject", false);

pub const OptionalActorValue = ObjectValue(Object.Actor, "isActorObject", true);
pub const OptionalManagedValue = ObjectValue(Object.Managed, "isManagedObject", true);
pub const OptionalByteArrayValue = ObjectValue(Object.ByteArray, "isByteArrayObject", true);

/// Type to store an enum as a heap value.
pub fn EnumValue(comptime EnumT: type) type {
    const enum_type_info = @typeInfo(EnumT);
    const backing_type_info = @typeInfo(enum_type_info.Enum.tag_type).Int;

    // FIXME: Instead of using a magic value, obtain from Value
    if (backing_type_info.bits > 62) {
        @compileError("Backing types larger than 62 bits aren't supported by EnumValue");
    }

    return packed struct {
        inner: InnerType,

        const Self = @This();
        const InnerType = IntegerValueAdvanced(enum_type_info.Enum.tag_type);

        pub fn init(value: EnumT) Self {
            return .{ .inner = InnerType.init(@enumToInt(value)) };
        }

        pub fn get(self: Self) EnumT {
            return @intToEnum(EnumT, self.inner.get());
        }
    };
}

// FIXME: This is very inefficient.
/// Type to store a bool as a heap value.
pub const BoolValue = packed struct {
    inner: InnerType,

    const Self = @This();
    const InnerType = IntegerValueAdvanced(u1);

    pub fn init(value: bool) Self {
        return .{ .inner = InnerType.init(@boolToInt(value)) };
    }

    pub fn get(self: Self) bool {
        return self.inner.get() != 0;
    }
};

/// Type to store a Range as a heap value.
pub const RangeValue = packed struct {
    start: InnerType,
    end: InnerType,

    const Self = @This();
    const InnerType = IntegerValue(.Unsigned);

    pub fn init(range: Range) Self {
        return .{ .start = InnerType.init(@intCast(u62, range.start)), .end = InnerType.init(@intCast(u62, range.end)) };
    }

    pub fn get(self: Self) Range {
        return .{ .start = @intCast(usize, self.start.get()), .end = @intCast(usize, self.end.get()) };
    }
};

/// Type to store any ArrayList type as a heap value. The ArrayList's alignment
/// must be 4 or greater.
pub fn ArrayListValue(comptime ArrayListT: type) type {
    const array_list_fields = @typeInfo(ArrayListT).Struct.fields;
    const items_field = items_field: {
        inline for (array_list_fields) |field| {
            if (std.mem.eql(u8, field.name, "items"))
                break :items_field field;
        }
    };
    const is_unmanaged = is_unmanaged: {
        inline for (array_list_fields) |field| {
            if (std.mem.eql(u8, field.name, "allocator"))
                break :is_unmanaged false;
        }

        break :is_unmanaged true;
    };

    const items_alignment = @typeInfo(items_field.field_type).Pointer.alignment;
    if (items_alignment < 4) {
        @compileError("Passed ArrayList's alignment must be 4 or greater");
    }

    if (!is_unmanaged) {
        @compileError("Only unmanaged ArrayLists are supported with ArrayListValue");
    }

    const ChildT = @typeInfo(items_field.field_type).Pointer.child;
    const SliceT = []align(items_alignment) ChildT;
    const PointerT = [*]align(items_alignment) ChildT;

    return extern struct {
        // NOTE: This is safe because we guarantee that the bottom two bits are
        //       always clear with the items alignment check above.
        items_ptr: u64 align(@alignOf(u64)),
        items_len: IntegerValue(.Unsigned) align(@alignOf(u64)),
        capacity: IntegerValue(.Unsigned) align(@alignOf(u64)),

        pub const Self = @This();

        pub fn init(array_list: ArrayListT) Self {
            return .{
                .items_ptr = @intCast(u64, @ptrToInt(array_list.items.ptr)),
                .items_len = IntegerValue(.Unsigned).init(array_list.items.len),
                .capacity = IntegerValue(.Unsigned).init(array_list.capacity),
            };
        }

        pub fn getItems(self: Self) SliceT {
            return @intToPtr(PointerT, self.items_ptr)[0..self.items_len.get()];
        }

        pub fn get(self: Self) ArrayListT {
            return .{ .items = self.getItems(), .capacity = @intCast(usize, self.capacity.get()) };
        }
    };
}
