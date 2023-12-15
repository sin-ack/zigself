// Copyright (c) 2021-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("../Heap.zig");
const Slot = @import("../slot.zig").Slot;
const debug = @import("../../debug.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig").Object;
const traversal = @import("../object_traversal.zig");
const MapBuilder = @import("../map_builder.zig").MapBuilder;
const pointer = @import("../../utility/pointer.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");

const SLOTS_LOOKUP_DEBUG = debug.SLOTS_LOOKUP_DEBUG;

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
        pub fn requiredSizeForAllocation(assignable_slot_count: u8) usize {
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

            return &getAssignableSlots(self)[@intCast(slot.value.asUnsignedInteger())];
        }

        /// Return a shallow copy of this object.
        pub fn clone(self: ObjectT.Ptr, token: *Heap.AllocationToken, actor_id: u31) ObjectT.Ptr {
            return ObjectT.create(token, actor_id, self.getMap(), getAssignableSlots(self));
        }
    };
}

/// Self Handbook, §3.3.8 The lookup algorithm
pub fn slotsLookup(
    comptime ObjectType: type,
    object: *ObjectType,
    selector_hash: object_lookup.SelectorHash,
    previously_visited: ?*const object_lookup.VisitedValueLink,
) object_lookup.LookupResult {
    if (previously_visited) |visited| {
        var link: ?*const object_lookup.VisitedValueLink = visited;
        while (link) |l| {
            if (l.value.data == object.asValue().data) {
                // Cyclic reference
                return object_lookup.LookupResult.nothing;
            }

            link = l.previous;
        }
    }

    const currently_visited = object_lookup.VisitedValueLink{ .previous = previously_visited, .value = object.asValue() };

    // Direct lookup
    for (object.getSlots()) |slot| {
        if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Comparing slot \"{s}\" (hash {x}) vs. our hash {}\n", .{ slot.name.asByteArray().getValues(), slot.getHash(), selector_hash });
        const slot_hash = slot.getHash();
        if (slot.isAssignable()) {
            if (selector_hash.assignment_target) |assignment_target_hash| {
                if (slot_hash == assignment_target_hash)
                    return object_lookup.LookupResult{
                        .Assignment = .{
                            .object = Object.fromAddress(object.asObjectAddress()),
                            .value_ptr = object.getAssignableSlotValue(slot),
                        },
                    };
            }

            if (slot_hash == selector_hash.regular)
                return object_lookup.LookupResult{ .Regular = object.getAssignableSlotValue(slot).* };
        }

        if (slot_hash == selector_hash.regular)
            return object_lookup.LookupResult{ .Regular = slot.value };
    }

    if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Could not find the slot on this object, looking at parents\n", .{});

    // Parent lookup
    for (object.getSlots()) |slot| {
        if (slot.isParent()) {
            const slot_value = if (slot.isAssignable())
                object.getAssignableSlotValue(slot).*
            else
                slot.value;

            if (slot_value.isObjectReference()) {
                const parent_lookup_result = selector_hash.chainedLookupObject(slot_value.asObject(), &currently_visited);
                if (parent_lookup_result != .Nothing) return parent_lookup_result;
            } else {
                @panic("FIXME: Allow integers and floating point numbers to be parent slot values (let me know of your usecase!)");
            }
        }
    }

    // Nope, not here
    return object_lookup.LookupResult.nothing;
}

/// An object consisting of constant or mutable slots. The constant slot's value
/// is stored on the map, while the mutable slot values are stored on the object
/// itself.
pub const Slots = extern struct {
    object: Object align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(Slots, .Mutable);

    pub usingnamespace SlotsLikeObjectBase(Slots);
    pub usingnamespace AssignableSlotsMixin(Slots);

    pub fn create(token: *Heap.AllocationToken, actor_id: u31, map: SlotsMap.Ptr, assignable_slot_values: []const Value) Slots.Ptr {
        if (assignable_slot_values.len != map.getAssignableSlotCount()) {
            std.debug.panic(
                "Passed assignable slot slice does not match slot count in map (expected {}, got {})",
                .{ map.getAssignableSlotCount(), assignable_slot_values.len },
            );
        }

        const size = Slots.requiredSizeForAllocation(@intCast(assignable_slot_values.len));

        const memory_area = token.allocate(.Object, size);
        var self: Slots.Ptr = @ptrCast(memory_area);
        self.init(actor_id, map);
        @memcpy(self.getAssignableSlots(), assignable_slot_values);

        return self;
    }

    fn init(self: Slots.Ptr, actor_id: Object.ActorID, map: SlotsMap.Ptr) void {
        self.object = .{
            .object_information = .{ .object_type = .Slots, .actor_id = actor_id },
            .map = map.asValue(),
        };
    }

    pub fn getMap(self: Slots.Ptr) SlotsMap.Ptr {
        return self.object.getMap().mustBeType(.Slots);
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
    pub fn addSlotsFrom(self: Slots.Ptr, source_object: Slots.Ptr, allocator: Allocator, token: *Heap.AllocationToken) !Slots.Ptr {
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
        var new_map = SlotsMap.create(token, @intCast(merge_info.slots));
        var map_builder = new_map.getMapBuilder(token);

        forSlotsInMergeOrder(self, source_object, struct {
            map_builder: *@TypeOf(map_builder),
            target_object_reachability: @TypeOf(self.object.object_information.reachability),

            pub fn visit(context: @This(), object: Slots.Ptr, slot: Slot) !void {
                const slot_copy = slot.copy(object);
                context.map_builder.addSlot(slot_copy);

                // If the object we're adding slots to is globally reachable
                // then the objects pointed to in the slots must become globally
                // reachable as well.
                if (context.target_object_reachability == .Global) {
                    _ = traversal.traverseNonGloballyReachableObjectGraph(slot_copy.value, struct {
                        pub fn visit(s: @This(), obj: Object.Ptr) error{}!Object.Ptr {
                            _ = s;
                            obj.object_information.reachability = .Global;
                            return obj;
                        }
                    }{}) catch unreachable;
                }
            }
        }{ .map_builder = &map_builder, .target_object_reachability = self.object.object_information.reachability }) catch unreachable;

        // At this point, we have a map builder which has initialized our new
        // map with all the constant slots.
        // We now need to determine whether we need to create a new object on
        // the heap, or whether we can cheese it and change just the current
        // object on the heap instead.
        if (object_needs_change) {
            // We do need to create a new object, and then update all the heap
            // references to it.
            const new_object = map_builder.createObject();
            new_object.object.object_information.reachability = self.object.object_information.reachability;
            try token.heap.updateAllReferencesTo(self.asValue(), new_object.asValue());
            return new_object;
        }

        // We can simply update the assignable slots and map pointer of the
        // object.
        self.object.map = map_builder.map.asValue();
        try token.heap.rememberObjectReference(self.asValue(), self.object.map);
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
            required_size += Slots.requiredSizeForAllocation(@intCast(merge_info.assignable_slot_values));

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

        const MergeCalculator = struct {
            merged_slots: *std.ArrayList(Slot),
            slots: *usize,
            assignable_slot_values: *usize,
            has_updated_slots: *bool,
            source_object: Slots.Ptr,

            fn visit(context: @This(), object: Slots.Ptr, slot: Slot) !void {
                context.slots.* += slot.requiredSlotSpace(context.merged_slots.items);
                const context_assignable_slot_values: isize = @intCast(context.assignable_slot_values.*);
                context.assignable_slot_values.* = @intCast(context_assignable_slot_values +
                    slot.requiredAssignableSlotValueSpace(context.merged_slots.items));
                try context.merged_slots.append(slot);

                if (object == context.source_object)
                    context.has_updated_slots.* = true;
            }
        };

        try forSlotsInMergeOrder(target_object, source_object, MergeCalculator{
            .merged_slots = &merged_slots,
            .slots = &slots,
            .assignable_slot_values = &assignable_slot_values,
            .has_updated_slots = &has_updated_slots,
            .source_object = source_object,
        });

        return MergeInfo{
            .previous_slots = target_object.getSlots().len,
            .previous_assignable_slot_values = target_object.getMap().getAssignableSlotCount(),
            .slots = slots,
            .assignable_slot_values = assignable_slot_values,
            .has_updated_slots = has_updated_slots,
        };
    }

    pub fn canFinalize(self: Slots.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: Slots.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call Slots.finalize");
    }

    pub fn lookup(self: Slots.Ptr, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        return slotsLookup(Slots, self, selector_hash, previously_visited);
    }

    pub fn humanReadableName() []const u8 {
        return "a slots object";
    }
};

/// Return a mixin struct which can be added to slots-like maps with pub
/// usingnamespace.
pub fn SlotsLikeMapBase(comptime MapT: type) type {
    return struct {
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

        /// Return the amount of assignable slots that this slot map
        /// contains.
        pub fn getAssignableSlotCount(self: MapT.Ptr) u8 {
            // 255 assignable slots ought to be enough for everybody.
            return asSlotsMap(self).information.assignable_slot_count;
        }

        pub fn setAssignableSlotCount(self: MapT.Ptr, count: u8) void {
            asSlotsMap(self).information.assignable_slot_count = count;
        }

        fn asSlotsMap(self: MapT.Ptr) SlotsMap.Ptr {
            return @ptrCast(self);
        }

        pub fn getMapBuilder(self: MapT.Ptr, token: *Heap.AllocationToken) MapBuilder(MapT, MapT.ObjectType) {
            return MapBuilder(MapT, MapT.ObjectType).init(token, self);
        }
    };
}

/// The map of a slots object, consisting of a series of Slots.
pub const SlotsMap = extern struct {
    map: Map align(@alignOf(u64)),
    // Information about the slots within this map. Slots begin after this word.
    information: SlotInformation align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(SlotsMap, .Mutable);
    pub const ObjectType = Slots;
    const SlotInformation = packed struct(u64) {
        marker: u2 = @intFromEnum(Value.ValueType.Integer),
        padding: u6 = 0,
        assignable_slot_count: u8 = 0,
        // Maps inheriting from this map can use this area for extra information.
        extra: u16 = 0,
        slot_count: u32,
    };

    pub usingnamespace SlotsLikeMapBase(SlotsMap);

    /// Create a new slots map. Takes the amount of slots this object will have.
    ///
    /// IMPORTANT: All slots *must* be initialized right after creation.
    pub fn create(token: *Heap.AllocationToken, slot_count: u32) SlotsMap.Ptr {
        const size = SlotsMap.requiredSizeForAllocation(slot_count);

        const memory_area = token.allocate(.Object, size);
        var self: SlotsMap.Ptr = @ptrCast(memory_area);
        self.init(slot_count);

        return self;
    }

    pub fn createWithMapMap(map_map: Map.Ptr, token: *Heap.AllocationToken, slot_count: u32) SlotsMap.Ptr {
        const size = SlotsMap.requiredSizeForAllocation(slot_count);

        const memory_area = token.allocate(.Object, size);
        var self: SlotsMap.Ptr = @ptrCast(memory_area);
        self.initWithMapMap(map_map, slot_count);

        return self;
    }

    pub fn init(self: SlotsMap.Ptr, slot_count: u32) void {
        self.map.init(.Slots);
        self.information = .{
            .slot_count = slot_count,
        };
    }

    fn initWithMapMap(self: SlotsMap.Ptr, map_map: Map.Ptr, slot_count: u32) void {
        self.map.initWithMapMap(.Slots, map_map);
        self.information = .{
            .slot_count = slot_count,
        };
    }

    pub fn clone(self: SlotsMap.Ptr, token: *Heap.AllocationToken) SlotsMap.Ptr {
        const new_map = create(token, self.information.slot_count);

        new_map.setAssignableSlotCount(self.getAssignableSlotCount());
        @memcpy(new_map.getSlots(), self.getSlots());

        return new_map;
    }

    pub fn canFinalize(self: SlotsMap.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: SlotsMap.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call SlotsMap.finalize");
    }

    pub fn getSizeInMemory(self: SlotsMap.Ptr) usize {
        return requiredSizeForAllocation(self.information.slot_count);
    }

    pub fn getSizeForCloning(self: SlotsMap.Ptr) usize {
        return self.getSizeInMemory();
    }

    /// Return the size required for the whole map with the given slot count.
    pub fn requiredSizeForAllocation(slot_count: u32) usize {
        return @sizeOf(SlotsMap) + slot_count * @sizeOf(Slot);
    }
};
