// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const Heap = @import("../Heap.zig");
const traversal = @import("./traversal.zig");

const SeenObjectsSet = std.AutoHashMap([*]u64, void);

fn calculateRequiredMemoryForBlessing(allocator: Allocator, value: Value) Allocator.Error!usize {
    var required_memory: usize = 0;
    var seen_objects_set = SeenObjectsSet.init(allocator);
    defer seen_objects_set.deinit();

    const context = .{ .seen_objects_set = &seen_objects_set, .required_memory = &required_memory };
    const Context = @TypeOf(context);
    _ = traversal.traverseNonGloballyReachableObjectGraph(value, context, struct {
        fn f(ctx: Context, object: Object) Allocator.Error!Object {
            const gop = try ctx.seen_objects_set.getOrPut(object.getAddress());
            if (!gop.found_existing) {
                ctx.required_memory.* += object.getSizeInMemory();
                // XXX: If we have a ByteArray object, we also need to take its byte array memory into
                //      consideration.
                if (object.asType(.ByteArray)) |byte_array| {
                    ctx.required_memory.* += byte_array.getByteArray().getSizeInMemory();
                }
            }

            return object;
        }
        }.f) catch |err| return @errSetCast(Allocator.Error, err);

    return required_memory;
}

const CopiedObjectsMap = std.AutoHashMap([*]u64, [*]u64);
fn copyObjectGraphForNewActor(token: *Heap.AllocationToken, actor_id: u31, value: Value) Allocator.Error!Value {
    var copied_objects_map = CopiedObjectsMap.init(token.heap.allocator);
    defer copied_objects_map.deinit();

    const context = .{ .copied_objects_map = &copied_objects_map, .token = token, .actor_id = actor_id };
    const Context = @TypeOf(context);
    return traversal.traverseNonGloballyReachableObjectGraph(value, context, struct {
        fn f(ctx: Context, old_object: Object) Allocator.Error!Object {
            const gop = try ctx.copied_objects_map.getOrPut(old_object.getAddress());
            if (gop.found_existing) {
                return Object.fromAddress(gop.value_ptr.*);
            }

            const new_object = try old_object.clone(ctx.token, ctx.actor_id);
            gop.value_ptr.* = new_object.getAddress();
            return new_object;
        }
        }.f) catch |err| return @errSetCast(Allocator.Error, err);
}

pub fn bless(heap: *Heap, actor_id: u31, const_value: Value) !Value {
    var value = const_value;

    // Pass 1: Figure out the required memory for blessing this object graph.
    const required_memory = try calculateRequiredMemoryForBlessing(heap.allocator, value);

    const tracked_value = try heap.track(value);

    var token = token: {
        defer tracked_value.untrack(heap);

        var token = try heap.getAllocation(required_memory);

        value = tracked_value.getValue();
        break :token token;
    };
    defer token.deinit();

    // Pass 2: Actually copy the objects.
    const new_value = try copyObjectGraphForNewActor(&token, actor_id, value);

    return new_value;
}
