// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("../Heap.zig");
const debug = @import("../../debug.zig");
const Object = @import("../object.zig").Object;
const VMActor = @import("../Actor.zig");
const GenericValue = value_import.Value;
const PointerValue = value_import.PointerValue;
const value_import = @import("../value.zig");
const stage2_compat = @import("../../utility/stage2_compat.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// An actor object which is the object that the genesis actor interacts with in
/// Self code.
pub const Actor = extern struct {
    object: Object align(@alignOf(u64)),
    /// The VM Actor that is owned by this actor object.
    actor: PointerValue(VMActor) align(@alignOf(u64)),
    /// The actor context, which is the object the actor spawn activation
    /// returns.
    context: GenericValue align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(Actor, .Mutable);
    pub const Type = .Actor;
    pub const Value = value_import.ObjectValue(Actor);

    pub fn create(map_map: Map.Ptr, token: *Heap.AllocationToken, genesis_actor_id: u31, actor: *VMActor, context: GenericValue) !Actor.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self: Actor.Ptr = @ptrCast(memory_area);
        self.init(genesis_actor_id, map_map, actor, context);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(self: Actor.Ptr, genesis_actor_id: u31, actor_map: Map.Ptr, actor: *VMActor, context: GenericValue) void {
        self.object = .{
            .object_information = .{
                .object_type = Type,
                .actor_id = genesis_actor_id,
            },
            .map = actor_map.asValue(),
        };
        self.actor = PointerValue(VMActor).init(actor);
        self.context = context;
    }

    pub fn asObjectAddress(self: Actor.Ptr) [*]u64 {
        return @ptrCast(@alignCast(self));
    }

    pub fn asValue(self: Actor.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getActor(self: Actor.Ptr) *VMActor {
        return self.actor.get();
    }

    pub fn getSizeInMemory(self: Actor.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn getSizeForCloning(self: Actor.Ptr) usize {
        return self.getSizeInMemory();
    }

    /// Return the amount of bytes that this actor needs to spawn itself. Note
    /// that if spawning this actor, the activation's cost should be accounted
    /// for.
    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(Actor);
    }

    pub fn canFinalize(self: Actor.Ptr) bool {
        _ = self;
        return true;
    }

    pub fn finalize(self: Actor.Ptr, allocator: Allocator) void {
        // FIXME: Ask the VM whether the actor has been quit by the time we
        //        reach here.
        self.actor.get().destroy(allocator);
    }

    pub fn lookup(self: Actor.Ptr, vm: *VirtualMachine, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = self;
        _ = vm;
        _ = selector_hash;
        _ = previously_visited;

        @panic("TODO: What should lookups on actor objects do?");
    }

    pub fn humanReadableName() []const u8 {
        return "an actor";
    }
};
