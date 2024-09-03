// Copyright (c) 2021-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Actor = @import("../Actor.zig");
const debug = @import("../../debug.zig");
const Object = @import("../object.zig").Object;
const context = @import("../context.zig");
const pointer = @import("../../utility/pointer.zig");
const Selector = @import("../Selector.zig");
const VMByteArray = @import("../ByteArray.zig");
const value_import = @import("../value.zig");
const GenericValue = value_import.Value;
const LookupResult = @import("../object_lookup.zig").LookupResult;

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

pub const ByteArray = extern struct {
    object: Object align(@alignOf(u64)),
    byte_array: GenericValue align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(ByteArray, .Mutable);
    pub const Type = .ByteArray;
    pub const Value = value_import.ObjectValue(ByteArray);

    /// Create an initialized byte array object from the given values.
    pub fn createWithValues(token: *Heap.AllocationToken, actor_id: Actor.ActorID, values: []const u8) ByteArray.Ptr {
        const self = createUninitialized(token, actor_id, values.len);
        @memcpy(self.getValues(), values);
        return self;
    }

    /// Create an uninitialized byte array object with the given size.
    pub fn createUninitialized(token: *Heap.AllocationToken, actor_id: Actor.ActorID, size: usize) ByteArray.Ptr {
        const byte_array = VMByteArray.createUninitialized(token, size);
        return create(token, actor_id, byte_array);
    }

    /// Create a byte array object with an existing byte array.
    pub fn create(token: *Heap.AllocationToken, actor_id: Actor.ActorID, byte_array: VMByteArray) ByteArray.Ptr {
        const size = requiredSizeForSelfAllocation();
        const memory_area = token.allocate(.Object, size);
        var self: ByteArray.Ptr = @ptrCast(memory_area);
        self.init(actor_id, byte_array);

        return self;
    }

    fn init(self: ByteArray.Ptr, actor_id: Actor.ActorID, byte_array: VMByteArray) void {
        self.object.init(.ByteArray, actor_id);
        self.byte_array = byte_array.asValue();
    }

    pub fn asObjectAddress(self: ByteArray.Ptr) [*]u64 {
        return @ptrCast(@alignCast(self));
    }

    pub fn asValue(self: ByteArray.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getValues(self: ByteArray.Ptr) []u8 {
        return self.getByteArray().getValues();
    }

    pub fn getLength(self: ByteArray.Ptr) usize {
        return self.getByteArray().getLength();
    }

    pub fn getByteArray(self: ByteArray.Ptr) VMByteArray {
        return self.byte_array.asByteArray().?;
    }

    pub fn clone(self: ByteArray.Ptr, token: *Heap.AllocationToken, actor_id: Actor.ActorID) ByteArray.Ptr {
        return createWithValues(token, actor_id, self.getValues());
    }

    pub fn getSizeInMemory(self: ByteArray.Ptr) usize {
        _ = self;
        return requiredSizeForSelfAllocation();
    }

    pub fn getSizeForCloning(self: ByteArray.Ptr) usize {
        return requiredSizeForAllocation(self.getLength());
    }

    pub fn canFinalize(self: ByteArray.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: ByteArray.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call Array.finalize");
    }

    pub fn lookup(self: ByteArray.Ptr, selector: Selector, previously_visited: ?*const Selector.VisitedValueLink) LookupResult {
        _ = self;
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("ByteArray.lookup: Looking at traits string\n", .{});
        const string_traits = context.getVM().string_traits.getValue();
        if (selector.equals(Selector.well_known.parent))
            return LookupResult{ .Regular = string_traits };

        return string_traits.lookup(selector);
    }

    /// Return the size for allocating just this object.
    pub fn requiredSizeForSelfAllocation() usize {
        return @sizeOf(ByteArray);
    }

    /// Return the size for allocating this object and the byte array it
    /// represents.
    pub fn requiredSizeForAllocation(length: usize) usize {
        return requiredSizeForSelfAllocation() + VMByteArray.requiredSizeForAllocation(length);
    }

    pub fn humanReadableName() []const u8 {
        return "a byte array";
    }
};
