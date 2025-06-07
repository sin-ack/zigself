// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("../map.zig").Map;
const Slot = @import("../slot.zig").Slot;
const Actor = @import("../Actor.zig");
const debug = @import("../../debug.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig").Object;
const pointer = @import("../../utility/pointer.zig");
const Selector = @import("../Selector.zig");
const traversal = @import("../object_traversal.zig");
const MapObject = @import("../object.zig").MapObject;
const ValueSlot = @import("../object_lookup.zig").ValueSlot;
const BaseObject = @import("../base_object.zig").BaseObject;
const mapbuilder = @import("../map_builder.zig");
const heap_import = @import("../Heap.zig");
const LookupResult = @import("../object_lookup.zig").LookupResult;
const VirtualMachine = @import("../VirtualMachine.zig");

const SLOTS_LOOKUP_DEBUG = debug.SLOTS_LOOKUP_DEBUG;

/// Information about added/changed slots when an object is merged into another.
const MergeInfo = struct {
    previous_slots: usize,
    previous_assignable_slots: usize,
    slots: usize,
    assignable_slots: usize,
    has_updated_slots: bool,

    pub fn hasChanges(self: MergeInfo) bool {
        return self.has_updated_slots or
            self.previous_slots != self.slots or
            self.previous_assignable_slots != self.assignable_slots;
    }

    pub fn assignableSlotCountChanged(self: MergeInfo) bool {
        return self.previous_assignable_slots != self.assignable_slots;
    }
};

/// A mixin struct which can be added to a slots-like object through `pub
/// usingnamespace`. Adds common functions expected from slots-like objects.
pub fn SlotsLikeObjectBase(comptime ObjectT: type) type {
    return struct {
        /// Return the address of the current object.
        pub fn asObjectAddress(self: ObjectT.Ptr) [*]u64 {
            return @ptrCast(@alignCast(self));
        }

        /// Return this object as a value.
        pub fn asValue(self: ObjectT.Ptr) Value {
            return Value.fromObjectAddress(asObjectAddress(self));
        }
    };
}

/// A mixin that provides the common implementation of assignable slots in
/// and assignable slots-dependent methods for slots-like objects.
pub fn AssignableSlotsMixin(comptime ObjectT: type) type {
    return struct {
        /// Return the amount of bytes that this object takes up in the
        /// heap.
        pub fn getSizeInMemory(self: ObjectT.Ptr) usize {
            return requiredSizeForAllocation(self.getMap().getAssignableSlotCount());
        }

        pub fn getSizeForCloning(self: ObjectT.Ptr) usize {
            return self.getSizeInMemory();
        }

        /// Return the amount of bytes required to create this object.
        pub fn requiredSizeForAllocation(assignable_slot_count: u15) usize {
            return @sizeOf(ObjectT) + assignable_slot_count * @sizeOf(Value);
        }

        /// Return a slice of `Value`s for the assignable slots that are
        /// after the Slots object header. Should not be called from
        /// outside; use getAssignableSlotValue instead.
        pub fn getAssignableSlots(self: ObjectT.Ptr) pointer.HeapSlice(Value, .Mutable) {
            const object_size = @sizeOf(ObjectT);
            const object_memory: [*]u8 = @ptrCast(self);
            const assignable_slot_count = ObjectT.getMap(self).getAssignableSlotCount();

            return @alignCast(std.mem.bytesAsSlice(
                Value,
                object_memory[object_size .. object_size + assignable_slot_count * @sizeOf(Value)],
            ));
        }

        /// Return the assignable slot value for this slot.
        pub fn getAssignableSlotValue(self: ObjectT.Ptr, slot: Slot) pointer.HeapPtr(Value, .Mutable) {
            std.debug.assert(slot.isAssignable());
            std.debug.assert(!slot.isArgument());

            return &getAssignableSlots(self)[@intCast(slot.value.unsafeAsUnsignedInteger())];
        }

        /// Return a shallow copy of this object.
        pub fn clone(self: ObjectT.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) ObjectT.Ptr {
            _ = allocator;
            _ = heap;
            return ObjectT.create(token, actor_id, self.getMap(), getAssignableSlots(self));
        }

        /// Visit the assignable slot values in this object with the given
        /// visitor.
        pub fn visitAssignableSlotValues(self: ObjectT.Ptr, visitor: anytype) !void {
            for (self.getAssignableSlots()) |*slot| {
                try visitor.visit(slot, @ptrCast(self));
            }
        }
    };
}

/// Self Handbook, §3.3.8 The lookup algorithm
pub fn slotsLookup(
    comptime ObjectType: type,
    object: ObjectType.Ptr,
    selector: Selector,
    previously_visited: ?*const Selector.VisitedValueLink,
) LookupResult {
    if (previously_visited) |visited| {
        var link: ?*const Selector.VisitedValueLink = visited;
        while (link) |l| {
            if (l.value.data == object.asValue().data) {
                // Cyclic reference
                return LookupResult.nothing;
            }

            link = l.previous;
        }
    }

    const currently_visited = Selector.VisitedValueLink{ .previous = previously_visited, .value = object.asValue() };

    // Direct lookup
    for (object.getSlots(), 0..) |slot, slot_index| {
        const slot_selector = Selector.fromSlot(slot);
        if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Comparing selector {} vs. slot {}\n", .{ selector, slot_selector });

        if (selector.equals(slot_selector) or selector.canAssignTo(slot_selector)) {
            return .{
                .Found = .{
                    .object = @ptrCast(object),
                    .value_slot_index = slot_index,
                },
            };
        }
    }

    if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Could not find the slot on this object, looking at parents\n", .{});

    // Parent lookup
    for (object.getSlots()) |slot| {
        if (slot.isParent()) {
            const slot_value = if (slot.isAssignable())
                object.getAssignableSlotValue(slot).*
            else
                slot.value;

            if (slot_value.asObject()) |slot_object| {
                const parent_lookup_result = selector.chainedLookupObject(slot_object, &currently_visited);
                if (slot.isAssignable()) {
                    // FIXME: We currently cannot cache lookups if they went
                    //        through an assignable slot as the cache could
                    //        become stale if the parent slot is modified.
                    //        This needs to be solved by introducing
                    //        dependency links so that inline caches can be
                    //        invalidated via external events.
                    switch (parent_lookup_result) {
                        // We didn't find anything so move on to the next parent.
                        .Nothing => {},

                        .ActorMessage, .FoundUncacheable => return parent_lookup_result,
                        .Found => |lt| {
                            // "Kill" the cache by getting the direct value.
                            const lookup_result_value_slot = lt.object.getValueSlot(lt.value_slot_index);
                            return .{ .FoundUncacheable = lookup_result_value_slot };
                        },
                    }
                } else {
                    if (parent_lookup_result != .Nothing) return parent_lookup_result;
                }
            } else {
                @panic("FIXME: Allow integers to be parent slot values (let me know of your usecase!)");
            }
        }
    }

    // Nope, not here
    return LookupResult.nothing;
}

/// Return the corresponding value slot for the given value slot index in the
/// object.
pub fn getValueSlotGeneric(comptime ObjectType: type, object: ObjectType.Ptr, value_slot_index: usize) ValueSlot {
    const slots = object.getSlots();
    if (value_slot_index >= slots.len) {
        @branchHint(.cold);
        std.debug.panic("Value slot index {} is out of bounds for object with {} slots", .{ value_slot_index, slots.len });
    }

    const slot = slots[value_slot_index];
    if (slot.isAssignable()) {
        return .{
            .Assignable = .{
                .object = @ptrCast(object),
                .value_ptr = object.getAssignableSlotValue(slot),
                .selector = Selector.fromSlot(slot),
            },
        };
    } else {
        return .{ .Constant = slot.value };
    }
}

/// An object consisting of constant or mutable slots. The constant slot's value
/// is stored on the map, while the mutable slot values are stored on the object
/// itself.
pub const Slots = extern struct {
    object: MapObject align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Slots, .Mutable);

    pub usingnamespace SlotsLikeObjectBase(Slots);
    pub usingnamespace AssignableSlotsMixin(Slots);

    pub fn create(token: *heap_import.AllocationToken, actor_id: Actor.ActorID, map: SlotsMap.Ptr, assignable_slots: []const Value) Slots.Ptr {
        if (assignable_slots.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slots.len },
            );
        }

        const size = Slots.requiredSizeForAllocation(@intCast(assignable_slots.len));

        const memory_area = token.allocate(size);
        var self: Slots.Ptr = @ptrCast(memory_area);
        self.init(actor_id, map);
        @memcpy(self.getAssignableSlots(), assignable_slots);

        return self;
    }

    fn init(self: Slots.Ptr, actor_id: Actor.ActorID, map: SlotsMap.Ptr) void {
        self.object.init(.Slots, actor_id, map.asValue());
    }

    pub fn getMap(self: Slots.Ptr) SlotsMap.Ptr {
        return self.object.getMap().unsafeAsType(.Slots);
    }

    pub fn getSlots(self: Slots.Ptr) Slot.Slice {
        return self.getMap().getSlots();
    }

    // --- Adding slots ---

    /// Calls the given function with each slot in merge order.
    fn forSlotsInMergeOrder(
        target_object: Slots.Ptr,
        source_object: Slots.Ptr,
        // TODO: Write interfaces proposal for Zig
        visitor: anytype,
    ) !void {
        // We go through all of the target object's slots first, seeing if there
        // is any slot that should override ours. Any slot in the source object
        // with the same name should override ours.
        next_target_slot: for (target_object.getSlots()) |target_slot| {
            for (source_object.getSlots()) |source_slot| {
                if (source_slot.getHash() == target_slot.getHash()) {
                    try visitor.visit(source_object, source_slot);
                    continue :next_target_slot;
                }
            }

            try visitor.visit(target_object, target_slot);
        }

        // We then go through all of the source object's slots, adding slots
        // that we haven't seen before to the map builder.
        next_source_slot: for (source_object.getSlots()) |source_slot| {
            for (target_object.getSlots()) |target_slot| {
                if (source_slot.getHash() == target_slot.getHash()) {
                    continue :next_source_slot;
                }
            }

            try visitor.visit(source_object, source_slot);
        }
    }

    /// Add the source slots into the target object. This operation may cause a
    /// move of this object if the number of assignable slots changes. A pointer
    /// to the new location of the slots object is returned in this case.
    /// Otherwise, the original location is returned as a pointer.
    ///
    /// The token must have at least `self.requiredSizeForMerging(source_object)`
    /// bytes available.
    pub fn addSlotsFrom(self: Slots.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, source_object: Slots.Ptr) !Slots.Ptr {
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
        const new_map = SlotsMap.create(token, @intCast(merge_info.slots));
        var map_builder: SlotsMap.MapBuilder = undefined;
        map_builder.initInPlace(token, new_map);

        forSlotsInMergeOrder(self, source_object, struct {
            map_builder: *SlotsMap.MapBuilder,
            target_object_reachability: @TypeOf(self.object.getMetadata().reachability),

            pub fn visit(context: @This(), object: Slots.Ptr, slot: Slot) !void {
                const slot_copy = slot.copy(object);
                context.map_builder.addSlot(slot_copy);

                // If the object we're adding slots to is globally reachable
                // then the objects pointed to in the slots must become globally
                // reachable as well.
                if (context.target_object_reachability == .Global) {
                    _ = traversal.traverseNonGloballyReachableObjectGraph(slot_copy.value, struct {
                        pub fn visit(s: @This(), base_object: BaseObject.Ptr) error{}!BaseObject.Ptr {
                            _ = s;
                            base_object.metadata.reachability = .Global;
                            return base_object;
                        }
                    }{}) catch unreachable;
                }
            }
        }{ .map_builder = &map_builder, .target_object_reachability = self.object.getMetadata().reachability }) catch unreachable;

        // At this point, we have a map builder which has initialized our new
        // map with all the constant slots.
        // We now need to determine whether we need to create a new object on
        // the heap, or whether we can cheese it and change just the current
        // object on the heap instead.
        if (object_needs_change) {
            // We do need to create a new object, and then update all the heap
            // references to it.
            const new_object = map_builder.createObject();
            new_object.object.getMetadata().reachability = self.object.getMetadata().reachability;
            try heap.updateAllReferencesTo(self.asValue(), new_object.asValue());
            return new_object;
        }

        // We can simply update the assignable slots and map pointer of the
        // object.
        self.object.map = map_builder.map.asValue();
        _ = try heap.rememberObjectReference(self.asValue(), self.object.map);
        const assignable_values = self.getAssignableSlots();
        map_builder.writeAssignableSlotValuesTo(assignable_values);
        return self;
    }

    /// Return the amount of bytes that must be available on the heap in order
    /// to merge `source_object` into `target_object`.
    pub fn requiredSizeForMerging(target_object: Slots.Ptr, source_object: Slots.Ptr, allocator: Allocator) !usize {
        const merge_info = try target_object.calculateMergeOf(source_object, allocator);

        // If there is anything that could change any of the slots, then we need
        // to create a new copy of the map.
        const map_needs_change = merge_info.hasChanges();
        // We only create a new object if the assignable slot count changes;
        // otherwise we just overwrite the current object with the new slots.
        const object_needs_change = merge_info.assignableSlotCountChanged();

        var required_size: usize = 0;
        if (map_needs_change)
            required_size += SlotsMap.requiredSizeForAllocation(@intCast(merge_info.slots));
        if (object_needs_change)
            required_size += Slots.requiredSizeForAllocation(@intCast(merge_info.assignable_slots));

        return required_size;
    }

    /// Finds out how many new constant and assignable slots are required if the
    /// source object were to be merged into the target object, and how many
    /// slots would change from a constant to assignable slot and vice versa.
    ///
    /// Returns a MergeInfo.
    fn calculateMergeOf(target_object: Slots.Ptr, source_object: Slots.Ptr, allocator: Allocator) !MergeInfo {
        var slots: usize = 0;
        // usize despite requiredAssignableSlotValueSpace returning isize, as
        // this should never go negative (that'd be a bug).
        var assignable_slots: usize = 0;
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

        const MergeCalculator = struct {
            merged_slots: *std.ArrayList(Slot),
            slots: *usize,
            assignable_slots: *usize,
            has_updated_slots: *bool,
            source_object: Slots.Ptr,

            fn visit(context: @This(), object: Slots.Ptr, slot: Slot) !void {
                context.slots.* += slot.requiredSlotSpace(context.merged_slots.items);
                const context_assignable_slots: isize = @intCast(context.assignable_slots.*);
                context.assignable_slots.* = @intCast(context_assignable_slots +
                    slot.requiredAssignableSlotValueSpace(context.merged_slots.items));
                try context.merged_slots.append(slot);

                if (object == context.source_object)
                    context.has_updated_slots.* = true;
            }
        };

        try forSlotsInMergeOrder(target_object, source_object, MergeCalculator{
            .merged_slots = &merged_slots,
            .slots = &slots,
            .assignable_slots = &assignable_slots,
            .has_updated_slots = &has_updated_slots,
            .source_object = source_object,
        });

        return MergeInfo{
            .previous_slots = target_object.getSlots().len,
            .previous_assignable_slots = target_object.getMap().getAssignableSlotCount(),
            .slots = slots,
            .assignable_slots = assignable_slots,
            .has_updated_slots = has_updated_slots,
        };
    }

    /// Visit the edges on this object with the given visitor.
    pub fn visitEdges(self: Slots.Ptr, visitor: anytype) !void {
        try self.object.visitEdges(visitor);
        try self.visitAssignableSlotValues(visitor);
    }

    pub fn lookup(self: Slots.Ptr, selector: Selector, previously_visited: ?*const Selector.VisitedValueLink) LookupResult {
        return slotsLookup(Slots, self, selector, previously_visited);
    }

    /// Get the value slot at the given index.
    pub fn getValueSlot(self: Slots.Ptr, index: usize) ValueSlot {
        return getValueSlotGeneric(Slots, self, index);
    }

    pub fn humanReadableName() []const u8 {
        return "a slots object";
    }
};

/// Return a mixin struct which can be added to slots-like maps with pub
/// usingnamespace.
pub fn SlotsLikeMapBase(comptime MapT: type) type {
    return struct {
        pub const MapBuilder = mapbuilder.MapBuilder(MapT, MapT.ObjectType);

        fn getSlotMemory(self: MapT.Ptr) pointer.HeapSlice(u8, .Mutable) {
            const total_object_size = MapT.getSizeInMemory(self);
            const map_memory: [*]align(@alignOf(u64)) u8 = @ptrCast(self);
            return map_memory[@sizeOf(MapT)..total_object_size];
        }

        pub fn getSlots(self: MapT.Ptr) Slot.Slice {
            return std.mem.bytesAsSlice(Slot, getSlotMemory(self));
        }

        pub fn asObjectAddress(self: MapT.Ptr) [*]u64 {
            return @ptrCast(self);
        }

        pub fn asValue(self: MapT.Ptr) Value {
            return Value.fromObjectAddress(asObjectAddress(self));
        }

        pub fn getSlotCount(self: MapT.Ptr) u16 {
            return asSlotsMap(self).map.getMetadata().slots;
        }

        /// Return the amount of assignable slots that this slot map
        /// contains.
        pub fn getAssignableSlotCount(self: MapT.Ptr) u15 {
            return asSlotsMap(self).map.getMetadata().assignable_slots;
        }

        pub fn setAssignableSlotCount(self: MapT.Ptr, count: u15) void {
            asSlotsMap(self).map.getMetadata().assignable_slots = count;
        }

        fn asSlotsMap(self: MapT.Ptr) SlotsMap.Ptr {
            return @ptrCast(self);
        }

        /// Visit the slots in this map with the given visitor. Call this
        /// from the `visitEdges` in the map implementation.
        pub fn visitSlots(self: MapT.Ptr, visitor: anytype) !void {
            for (self.getSlots()) |*slot| {
                try visitor.visit(&slot.name.value, @ptrCast(self));
                try visitor.visit(&slot.value, @ptrCast(self));
            }
        }
    };
}

/// The map of a slots object, consisting of a series of Slots.
pub const SlotsMap = extern struct {
    map: Map align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(SlotsMap, .Mutable);
    pub const ObjectType = Slots;

    pub usingnamespace SlotsLikeMapBase(SlotsMap);

    /// Create a new slots map. Takes the amount of slots this object will have.
    ///
    /// IMPORTANT: All slots *must* be initialized right after creation.
    pub fn create(token: *heap_import.AllocationToken, slot_count: u16) SlotsMap.Ptr {
        const size = SlotsMap.requiredSizeForAllocation(slot_count);

        const memory_area = token.allocate(size);
        var self: SlotsMap.Ptr = @ptrCast(memory_area);
        self.init(slot_count);

        return self;
    }

    pub fn init(self: SlotsMap.Ptr, slot_count: u16) void {
        self.map.init(.Slots);
        self.map.getMetadata().slots = slot_count;
    }

    pub fn clone(self: SlotsMap.Ptr, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken) SlotsMap.Ptr {
        _ = heap;
        const new_map = create(token, self.map.getMetadata().slots);

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        @memcpy(new_map.getSlots(), self.getSlots());

        return new_map;
    }

    /// Visit the edges on this map with the given visitor.
    pub fn visitEdges(self: SlotsMap.Ptr, visitor: anytype) !void {
        try self.visitSlots(visitor);
    }

    pub fn getSizeInMemory(self: SlotsMap.Ptr) usize {
        return requiredSizeForAllocation(self.map.getMetadata().slots);
    }

    pub fn getSizeForCloning(self: SlotsMap.Ptr) usize {
        return self.getSizeInMemory();
    }

    /// Return the size required for the whole map with the given slot count.
    pub fn requiredSizeForAllocation(slot_count: u16) usize {
        return @sizeOf(SlotsMap) + slot_count * @sizeOf(Slot);
    }
};
