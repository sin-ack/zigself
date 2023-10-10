// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("map.zig").Map;
const Heap = @import("../Heap.zig");
const debug = @import("../../debug.zig");
const Actor = @import("actor.zig").Actor;
const Object = @import("../object.zig").Object;
const GenericValue = value_import.Value;
const value_import = @import("../value.zig");
const pointer = @import("../../utility/pointer.zig");
const object_lookup = @import("../object_lookup.zig");
const VirtualMachine = @import("../VirtualMachine.zig");

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// An actor proxy which relays messages to the mailbox of an actor.
pub const ActorProxy = extern struct {
    object: Object align(@alignOf(u64)),
    /// The Actor object to proxy messages to.
    actor_object: Actor.Value align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(ActorProxy, .Mutable);
    pub const Type = .ActorProxy;
    pub const Value = value_import.ObjectValue(ActorProxy);

    /// Create the Actor object without sending a message to it.
    pub fn create(map_map: Map.Ptr, token: *Heap.AllocationToken, current_actor_id: u31, actor_object: Actor.Ptr) ActorProxy.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self: ActorProxy.Ptr = @ptrCast(memory_area);
        self.init(current_actor_id, map_map, actor_object);
        return self;
    }

    fn init(self: ActorProxy.Ptr, current_actor_id: u31, map_map: Map.Ptr, actor_object: Actor.Ptr) void {
        self.object = .{
            .object_information = .{
                .object_type = .ActorProxy,
                .actor_id = current_actor_id,
            },
            .map = map_map.asValue(),
        };
        self.actor_object = Actor.Value.init(actor_object);
    }

    pub fn asObjectAddress(self: ActorProxy.Ptr) [*]u64 {
        return @ptrCast(@alignCast(self));
    }

    pub fn asValue(self: ActorProxy.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getActor(self: ActorProxy.Ptr) *Actor {
        return self.actor_object.get();
    }

    pub fn clone(self: ActorProxy.Ptr, vm: *VirtualMachine, token: *Heap.AllocationToken, actor_id: u31) ActorProxy.Ptr {
        const map_map = vm.getMapMap();
        return create(map_map, token, actor_id, self.getActor());
    }

    pub fn getSizeInMemory(self: ActorProxy.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn getSizeForCloning(self: ActorProxy.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ActorProxy);
    }

    pub fn canFinalize(self: ActorProxy.Ptr) bool {
        _ = self;
        return false;
    }

    pub fn finalize(self: ActorProxy.Ptr, allocator: Allocator) void {
        _ = self;
        _ = allocator;
        @panic("Attempted to call ActorProxy.finalize");
    }

    pub fn lookup(self: ActorProxy.Ptr, vm: *VirtualMachine, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = previously_visited;

        // FIXME: Refactor this to not perform a method lookup in preparation of multi-threading.
        if (LOOKUP_DEBUG) std.debug.print("ActorProxy.lookup: Looking at an actor proxy object\n", .{});

        const target_actor = self.actor_object.get();
        return switch (target_actor.context.lookupByHash(vm, selector_hash)) {
            .Nothing => object_lookup.LookupResult.nothing,
            // FIXME: This should probably cause a different kind of error.
            .Assignment => object_lookup.LookupResult.nothing,
            .Regular => |lookup_result| blk: {
                if (!(lookup_result.isObjectReference() and lookup_result.asObject().object_information.object_type == .Method)) {
                    // NOTE: In zigSelf, all messages are async. Therefore
                    //       sending a message to a non-method slot will not
                    //       return any meaningful value to the user.
                    //       However it should also still be valid, so we
                    //       cannot return nothing here.
                    break :blk object_lookup.LookupResult{ .Regular = vm.nil() };
                }

                break :blk object_lookup.LookupResult{
                    .ActorMessage = .{
                        .target_actor = target_actor,
                        .method = lookup_result.asObject().mustBeType(.Method),
                    },
                };
            },
            .ActorMessage => unreachable,
        };
    }
};
