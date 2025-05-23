// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("Actor.zig");
const Value = @import("value.zig").Value;
const context = @import("context.zig");
const traversal = @import("object_traversal.zig");
const BaseObject = @import("base_object.zig").BaseObject;
const heap_import = @import("Heap.zig");
const VirtualMachine = @import("VirtualMachine.zig");

const SeenObjectsSet = std.AutoHashMap([*]u64, void);
const RequiredMemoryCalculator = struct {
    seen_objects_set: *SeenObjectsSet,
    required_memory: *usize,

    pub fn visit(self: @This(), base_object: BaseObject.Ptr) Allocator.Error!BaseObject.Ptr {
        const gop = try self.seen_objects_set.getOrPut(base_object.getAddress());
        if (!gop.found_existing) {
            self.required_memory.* += base_object.getSizeForCloning();
        }

        return base_object;
    }
};

fn calculateRequiredMemoryForBlessing(allocator: Allocator, value: Value) Allocator.Error!usize {
    var required_memory: usize = 0;
    var seen_objects_set = SeenObjectsSet.init(allocator);
    defer seen_objects_set.deinit();

    _ = traversal.traverseNonGloballyReachableObjectGraph(
        value,
        RequiredMemoryCalculator{ .seen_objects_set = &seen_objects_set, .required_memory = &required_memory },
    ) catch |err| return @as(Allocator.Error, @errorCast(err));

    return required_memory;
}

const CopiedObjectsMap = std.AutoHashMap([*]u64, [*]u64);
const ObjectGraphCopier = struct {
    copied_objects_map: *CopiedObjectsMap,
    allocator: Allocator,
    heap: *VirtualMachine.Heap,
    token: *heap_import.AllocationToken,
    actor_id: Actor.ActorID,

    pub fn visit(self: @This(), old_base_object: BaseObject.Ptr) Allocator.Error!BaseObject.Ptr {
        const gop = try self.copied_objects_map.getOrPut(old_base_object.getAddress());
        if (gop.found_existing) {
            return BaseObject.fromAddress(gop.value_ptr.*);
        }

        const new_object = try old_base_object.clone(self.allocator, self.heap, self.token, self.actor_id);
        gop.value_ptr.* = new_object.getAddress();
        return @ptrCast(new_object);
    }
};

fn copyObjectGraphForNewActor(allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, target_actor_id: Actor.ActorID, value: Value) Allocator.Error!Value {
    var copied_objects_map = CopiedObjectsMap.init(allocator);
    defer copied_objects_map.deinit();

    return traversal.traverseNonGloballyReachableObjectGraph(
        value,
        ObjectGraphCopier{
            .copied_objects_map = &copied_objects_map,
            .allocator = allocator,
            .heap = heap,
            .token = token,
            .actor_id = target_actor_id,
        },
    ) catch |err| return @as(Allocator.Error, @errorCast(err));
}

pub fn bless(target_actor_id: Actor.ActorID, const_value: Value) !Value {
    if (const_value.type != .Object)
        return const_value;

    const allocator = context.getVM().allocator;
    const heap = context.getHeap();

    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(heap);
    defer handles.deinit(heap);

    var value = const_value;
    handles.trackValue(&value);

    // Pass 1: Figure out the required memory for blessing this object graph.
    const required_memory = try calculateRequiredMemoryForBlessing(allocator, value);
    var token = try heap.allocate(required_memory);
    defer token.deinit();

    // Pass 2: Actually copy the objects.
    const new_value = try copyObjectGraphForNewActor(allocator, heap, &token, target_actor_id, value);

    return new_value;
}
