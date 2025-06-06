// Copyright (c) 2024-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("../Actor.zig");
const debug = @import("../../debug.zig");
const Object = @import("../object.zig").Object;
const context = @import("../context.zig");
const pointer = @import("../../utility/pointer.zig");
const Selector = @import("../Selector.zig");
const heap_import = @import("../Heap.zig");
const GenericValue = @import("../value.zig").Value;
const IntegerValue = @import("../value.zig").IntegerValue;
const LookupResult = @import("../object_lookup.zig").LookupResult;
const VirtualMachine = @import("../VirtualMachine.zig");

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
    const ExtraBits = Object.ExtraBits.reserve(@Type(.{ .int = .{ .bits = BottomBitCount, .signedness = .unsigned } }));

    /// Create a new Float from the given 64-bit float.
    pub fn create(token: *heap_import.AllocationToken, actor_id: Actor.ActorID, value: f64) Float.Ptr {
        const memory_area = token.allocate(requiredSizeForAllocation());
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
        self.top_bits = IntegerValue(.Unsigned).init(@intCast(value_bits >> BottomBitCount));
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
    pub fn clone(self: Float.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) !Float.Ptr {
        _ = allocator;
        _ = heap;

        return create(token, actor_id, self.get());
    }

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: Float.Ptr, visitor: anytype) !void {
        _ = self;
        _ = visitor;
    }

    /// Perform a lookup on this object.
    pub fn lookup(self: Float.Ptr, selector: Selector, previously_visited: ?*const Selector.VisitedValueLink) LookupResult {
        _ = self;
        _ = previously_visited;

        if (LOOKUP_DEBUG) std.debug.print("Float.lookup: Looking at traits float\n", .{});
        const float_traits = context.getVM().float_traits;
        if (selector.equals(Selector.well_known.parent))
            return LookupResult{ .Regular = float_traits };

        return float_traits.lookup(selector);
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(Float);
    }
};
