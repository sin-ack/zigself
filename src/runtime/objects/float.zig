// Copyright (c) 2024, sin-ack <sin-ack@protonmail.com>
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
const GenericValue = @import("../value.zig").Value;
const IntegerValue = @import("../value.zig").IntegerValue;
const object_lookup = @import("../object_lookup.zig");

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// An IEEE754 double-precision floating point number. Since zigSelf cannot
/// represent full 64-bit values on the heap, the top bits are stored in a
/// separate integer value.
pub const Float = extern struct {
    /// The object header. The extra bits are used to hold the bottom 16 bits of
    /// the 64-bit float.
    object: Object align(@alignOf(u64)),
    /// The top 48 bits of the 64-bit float.
    top_bits: IntegerValue(.Unsigned) align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Float, .Mutable);

    const BottomBitCount = 16;
    const ExtraBits = Object.ExtraBits.reserve(@Type(.{ .Int = .{ .bits = BottomBitCount, .signedness = .unsigned } }));

    /// Create a new Float from the given 64-bit float.
    pub fn create(token: *Heap.AllocationToken, actor_id: Actor.ActorID, value: f64) Float.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        var self: Float.Ptr = @ptrCast(memory_area);
        self.init(actor_id, value);

        return self;
    }

    fn init(self: Float.Ptr, actor_id: Actor.ActorID, value: f64) void {
        self.object.init(.Float, actor_id);
        self.write(value);
    }

    /// Return the value stored in this object.
    pub fn get(self: Float.Ptr) f64 {
        var value_bits: u64 = Float.ExtraBits.read(self.object.getMetadata().*);
        value_bits |= self.top_bits.get() << BottomBitCount;
        return @bitCast(value_bits);
    }

    /// Write the given float to this object.
    fn write(self: Float.Ptr, value: f64) void {
        const value_bits: u64 = @bitCast(value);
        Float.ExtraBits.write(self.object.getMetadata(), @intCast(value_bits & ((1 << BottomBitCount) - 1)));
        self.top_bits = IntegerValue(.Unsigned).init(value_bits >> BottomBitCount);
    }

    // --- Casting ---

    pub fn asValue(self: Float.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(@ptrCast(self));
    }

    // --- Object interface ---

    /// Return the size of this object in memory.
    pub fn getSizeInMemory(self: Float.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    /// Return the amount of memory required to clone this object.
    pub fn getSizeForCloning(self: Float.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    /// Clone this object.
    pub fn clone(self: Float.Ptr, token: *Heap.AllocationToken, actor_id: Actor.ActorID) !Float.Ptr {
        return create(token, actor_id, self.get());
    }

    /// Return whether this object can finalize.
    pub fn canFinalize(self: Float.Ptr) bool {
        _ = self;
        return false;
    }

    /// Finalize this object.
    pub fn finalize(self: Float.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Float.finalize() should never be called");
    }

    /// Perform a lookup on this object.
    pub fn lookup(self: Float.Ptr, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = self;
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("Float.lookup: Looking at traits float\n", .{});
        const float_traits = context.getVM().float_traits.getValue();
        if (selector_hash.regular == object_lookup.parent_hash)
            return object_lookup.LookupResult{ .Regular = float_traits };

        return float_traits.lookupByHash(selector_hash);
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(Float);
    }
};
