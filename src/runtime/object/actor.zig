// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Actor = @import("../Actor.zig");
const value = @import("../value.zig");
const Value = value.Value;
const Object = @import("../Object.zig");
const ActorValue = value.ActorValue;
const SourceRange = @import("../SourceRange.zig");
const PointerValue = value.PointerValue;
const VirtualMachine = @import("../VirtualMachine.zig");
const RegisterLocation = @import("../lowcode/register_location.zig").RegisterLocation;

// FIXME: This isn't thread safe!
var singleton_actor_map: ?Heap.Tracked = null;

fn getOrCreateActorMap(token: *Heap.AllocationToken) !Value {
    if (singleton_actor_map) |map| return map.getValue();

    const map = Object.Map.Slots.create(token, 0);
    singleton_actor_map = try token.heap.track(map.asValue());
    return map.asValue();
}

fn requiredSizeForActorMap() usize {
    return if (singleton_actor_map != null) 0 else Object.Map.Slots.requiredSizeForAllocation(0);
}

/// An actor object which is the object that the genesis actor interacts with in
/// Self code.
pub const ActorObject = packed struct {
    header: Object.Header,
    /// The VM Actor that is owned by this actor object.
    actor: PointerValue(Actor),
    /// The actor context, which is the object the actor spawn activation
    /// returns.
    context: Value,

    pub fn create(token: *Heap.AllocationToken, genesis_actor_id: u31, actor: *Actor, context: Value) !*ActorObject {
        const actor_map = try getOrCreateActorMap(token);

        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self = @ptrCast(*ActorObject, memory_area);
        self.init(genesis_actor_id, actor_map, actor, context);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(self: *ActorObject, genesis_actor_id: u31, actor_map: Value, actor: *Actor, context: Value) void {
        self.header.init(.Actor, genesis_actor_id, actor_map);
        self.actor = PointerValue(Actor).init(actor);
        self.context = context;
    }

    pub fn asObjectAddress(self: *ActorObject) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *ActorObject) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getActor(self: *ActorObject) *Actor {
        return self.actor.get();
    }

    pub fn getSizeInMemory(self: *ActorObject) usize {
        _ = self;
        // NOTE: Actor map will have been created at this point.
        return requiredSizeForAllocation();
    }

    /// Return the amount of bytes that this actor needs to spawn itself. Note
    /// that if spawning this actor, the activation's cost should be accounted
    /// for.
    pub fn requiredSizeForAllocation() usize {
        return requiredSizeForActorMap() + @sizeOf(ActorObject);
    }

    pub fn finalize(self: *ActorObject, allocator: Allocator) void {
        // FIXME: Ask the VM whether the actor has been quit by the time we
        //        reach here.
        self.actor.get().destroy(allocator);
    }
};

/// An actor proxy which relays message to the mailbox of an actor.
pub const ActorProxyObject = packed struct {
    header: Object.Header,
    /// The Actor object to proxy messages to.
    actor_object: ActorValue,

    /// Create the Actor object without sending a message to it.
    pub fn create(token: *Heap.AllocationToken, current_actor_id: u31, actor_object: *ActorObject) !*ActorProxyObject {
        const actor_map = try getOrCreateActorMap(token);

        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self = @ptrCast(*ActorProxyObject, memory_area);
        self.init(current_actor_id, actor_map, actor_object);
        return self;
    }

    fn init(self: *ActorProxyObject, current_actor_id: u31, actor_map: Value, actor_object: *ActorObject) void {
        self.header.init(.ActorProxy, current_actor_id, actor_map);
        self.actor_object = ActorValue.init(actor_object);
    }

    pub fn asObjectAddress(self: *ActorProxyObject) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: *ActorProxyObject) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getActorObject(self: *ActorProxyObject) *ActorObject {
        return self.actor_object.get();
    }

    pub fn clone(self: *ActorProxyObject, token: *Heap.AllocationToken, actor_id: u31) *ActorProxyObject {
        return create(token, actor_id, self.getActorObject()) catch unreachable;
    }

    pub fn getSizeInMemory(self: *ActorProxyObject) usize {
        _ = self;
        // NOTE: Actor map will have been created at this point.
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ActorProxyObject);
    }
};
