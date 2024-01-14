// Copyright (c) 2022-2024, sin-ack <sin-ack@protonmail.com>
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
const ActorObject = @import("actor.zig").Actor;
const GenericValue = value_import.Value;
const value_import = @import("../value.zig");
const object_lookup = @import("../object_lookup.zig");
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
    pub fn create(token: *Heap.AllocationToken, current_actor_id: Actor.ActorID, actor_object: ActorObject.Ptr) ActorProxy.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self: ActorProxy.Ptr = @ptrCast(memory_area);
        self.init(current_actor_id, actor_object);
        return self;
    }

    fn init(self: ActorProxy.Ptr, current_actor_id: Actor.ActorID, actor_object: ActorObject.Ptr) void {
        self.object.init(.ActorProxy, current_actor_id);
        self.actor_object = ActorObject.Value.init(actor_object);
    }

    pub fn asObjectAddress(self: ActorProxy.Ptr) [*]u64 {
        return @ptrCast(@alignCast(self));
    }

    pub fn asValue(self: ActorProxy.Ptr) GenericValue {
        return GenericValue.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getActor(self: ActorProxy.Ptr) *ActorObject {
        return self.actor_object.get();
    }

    pub fn clone(self: ActorProxy.Ptr, token: *Heap.AllocationToken, actor_id: Actor.ActorID) ActorProxy.Ptr {
        return create(token, actor_id, self.getActor());
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

    pub fn lookup(self: ActorProxy.Ptr, selector_hash: object_lookup.SelectorHash, previously_visited: ?*const object_lookup.VisitedValueLink) object_lookup.LookupResult {
        _ = previously_visited;

        // FIXME: Refactor this to not perform a method lookup in preparation of multi-threading.
        if (LOOKUP_DEBUG) std.debug.print("ActorProxy.lookup: Looking at an actor proxy object\n", .{});

        const target_actor = self.actor_object.get();
        return switch (target_actor.context.lookupByHash(selector_hash)) {
            .Nothing => object_lookup.LookupResult.nothing,
            // FIXME: This should probably cause a different kind of error.
            .Assignment => object_lookup.LookupResult.nothing,
            .Regular => |lookup_result| blk: {
                if (lookup_result.asObject()) |object| {
                    if (object.asType(.Method)) |method_object| {
                        break :blk object_lookup.LookupResult{
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
                break :blk object_lookup.LookupResult{ .Regular = context.getVM().nil() };
            },
            .ActorMessage => unreachable,
        };
    }
};
