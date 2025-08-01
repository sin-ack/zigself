// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("../map.zig").Map;
const Actor = @import("../Actor.zig");
const debug = @import("../../debug.zig");
const Object = @import("../object.zig").Object;
const context = @import("../context.zig");
const pointer = @import("../../utility/pointer.zig");
const Selector = @import("../Selector.zig");
const ActorObject = @import("actor.zig").Actor;
const heap_import = @import("../Heap.zig");
const GenericValue = value_import.Value;
const value_import = @import("../value.zig");
const LookupResult = @import("../object_lookup.zig").LookupResult;
const VirtualMachine = @import("../VirtualMachine.zig");

const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

/// An actor proxy which relays messages to the mailbox of an actor.
pub const ActorProxy = extern struct {
    object: Object align(@alignOf(u64)),
    /// The Actor object to proxy messages to.
    actor_object: ActorObject.Value align(@alignOf(u64)),

    pub const Ptr = pointer.HeapPtr(ActorProxy, .Mutable);
    pub const Type = .ActorProxy;
    pub const Value = value_import.ObjectValue(ActorProxy);

    /// Create the Actor object without sending a message to it.
    pub fn create(token: *heap_import.AllocationToken, current_actor_id: Actor.ActorID, actor_object: ActorObject.Ptr) ActorProxy.Ptr {
        const memory_area = token.allocate(requiredSizeForAllocation());
        const self: ActorProxy.Ptr = @ptrCast(memory_area);
        self.init(current_actor_id, actor_object);
        return self;
    }

    fn init(self: ActorProxy.Ptr, current_actor_id: Actor.ActorID, actor_object: ActorObject.Ptr) void {
        self.object.init(.ActorProxy, current_actor_id);
        self.actor_object = ActorObject.Value.init(actor_object);
    }

    fn asObjectAddress(self: ActorProxy.Ptr) [*]u64 {
        return @ptrCast(@alignCast(self));
    }

    pub fn asValue(self: ActorProxy.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getActor(self: ActorProxy.Ptr) *ActorObject {
        return self.actor_object.get();
    }

    pub fn clone(self: ActorProxy.Ptr, allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID) ActorProxy.Ptr {
        _ = allocator;
        _ = heap;

        return create(token, actor_id, self.getActor());
    }

    pub fn getSizeInMemory(self: ActorProxy.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn getSizeForCloning(self: ActorProxy.Ptr) usize {
        return self.getSizeInMemory();
    }

    pub fn getMapForCaching(self: ActorProxy.Ptr, vm: *const VirtualMachine) ?Map.Ptr {
        _ = self;
        _ = vm;
        return null;
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ActorProxy);
    }

    /// Visit edges of this object using the given visitor.
    pub fn visitEdges(self: ActorProxy.Ptr, visitor: anytype) !void {
        try visitor.visit(&self.actor_object.value, @ptrCast(self));
    }

    pub fn lookup(self: ActorProxy.Ptr, selector: Selector, previously_visited: ?*const Selector.VisitedValueLink) LookupResult {
        _ = previously_visited;

        // FIXME: Refactor this to not perform a method lookup in preparation of multi-threading.
        if (LOOKUP_DEBUG) std.debug.print("ActorProxy.lookup: Looking at an actor proxy object\n", .{});

        const target_actor = self.actor_object.get();
        return lookup: switch (target_actor.context.lookup(selector)) {
            .Nothing => LookupResult.nothing,
            .ActorMessage => unreachable,
            .FoundUncacheable => |value_slot| switch (value_slot) {
                .Constant => |target| self.trySendActorMessage(target),
                // FIXME: This should probably cause a different kind of error.
                .Assignable => LookupResult.nothing,
            },
            .Found => |lookup_target| {
                const value_slot = lookup_target.object.getValueSlot(lookup_target.value_slot_index);
                continue :lookup .{ .FoundUncacheable = value_slot };
            },
        };
    }

    fn trySendActorMessage(self: ActorProxy.Ptr, target: GenericValue) LookupResult {
        const target_actor = self.actor_object.get();

        if (target.asObject()) |object| {
            if (object.asType(.Method)) |method_object| {
                return LookupResult{
                    .ActorMessage = .{
                        .target_actor = target_actor,
                        .method = method_object,
                    },
                };
            }
        }

        // NOTE: In zigSelf, all messages are async. Therefore sending a
        //       message to a non-method slot will not return any
        //       meaningful value to the user. However it should also
        //       still be valid, so we cannot return nothing here.
        return LookupResult{ .FoundUncacheable = .{ .Constant = context.getVM().global_nil } };
    }
};
