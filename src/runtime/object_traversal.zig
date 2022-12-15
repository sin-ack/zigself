// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const Map = @import("objects/map.zig").Map;
const Slot = @import("slot.zig").Slot;
const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const stage2_compat = @import("../utility/stage2_compat.zig");

fn TraverseObjectGraphCallback(comptime ContextT: type) type {
    return stage2_compat.FnPtr(fn (ctx: ContextT, object: Object.Ptr) anyerror!Object.Ptr);
}

const TraverseObjectGraphLink = struct {
    previous: ?*const @This(),
    address: [*]u64,
};

fn traverseObjectGraphInner(
    value: Value,
    context: anytype,
    callback: TraverseObjectGraphCallback(@TypeOf(context)),
    previous_link: ?*const TraverseObjectGraphLink,
) anyerror!Value {
    return switch (value.getType()) {
        .ObjectMarker => unreachable,
        .Integer, .FloatingPoint => value,
        .ObjectReference => value: {
            const old_object = value.asObject();
            const old_object_address = value.asObjectAddress();
            const old_object_type = old_object.object_information.object_type;

            // Globally reachable objects are never traversed.
            if (old_object.object_information.reachability == .Global)
                break :value value;

            {
                // Check for cycles
                var it = previous_link;
                while (it) |link| : (it = link.previous) {
                    if (old_object_address == link.address) {
                        // NOTE: Callbacks must return the same new object for
                        //       the same existing object in order to preserve
                        //       the graph structure. We cannot do this on the
                        //       traverse side, because cycles are not the only
                        //       time the same object is referenced multiple
                        //       times. Consider the following:
                        //
                        //       A ---> B
                        //       |      |
                        //       v      v
                        //       C ---> D
                        //
                        //       There are no cycles in this object graph, but
                        //       D is still referenced twice. Therefore D's new
                        //       address must be the same on both callback runs.
                        const new_object = try callback(context, old_object);
                        break :value new_object.asValue();
                    }
                }
            }

            const current_link = TraverseObjectGraphLink{ .previous = previous_link, .address = old_object_address };

            const new_map = try traverseObjectGraphInner(old_object.map, context, callback, &current_link);
            // FIXME: Move this switch into object delegation.
            switch (old_object_type) {
                .ForwardedObject, .Activation, .Actor => unreachable,
                .Map => {
                    const old_map: Map.Ptr = old_object.mustBeType(.Map);
                    const map_type = old_map.map_information.map_type;
                    // Copy the map itself if necessary
                    const new_object = switch (map_type) {
                        // The map-map will always be immutable.
                        .MapMap => old_object,
                        .Slots, .Method, .Block => try callback(context, old_object),
                        // Array maps will always be immutable, because they
                        // don't point to anything that's not globally
                        // reachable.
                        .Array => old_object,
                    };
                    const map: Map.Ptr = new_object.mustBeType(.Map);

                    // Traverse the map contents and update anything non-globally reachable
                    switch (map_type) {
                        .Slots, .Method, .Block => {
                            const slots: Slot.Slice = switch (map_type) {
                                .MapMap, .Array => unreachable,
                                inline else => |t| map.mustBeType(t).getSlots(),
                            };

                            for (slots) |*slot| {
                                if (!slot.isAssignable()) {
                                    slot.value = try traverseObjectGraphInner(slot.value, context, callback, &current_link);
                                }
                            }
                        },
                        .MapMap, .Array => {},
                    }

                    break :value new_object.asValue();
                },
                .Slots, .Method, .Block => {
                    const new_object = try callback(context, old_object);
                    new_object.map = new_map;

                    const assignable_slots = switch (old_object_type) {
                        // FIXME: Move this to something like Object.getSlots().
                        .Slots => new_object.asType(.Slots).?.getAssignableSlots(),
                        .Method => new_object.asType(.Method).?.getAssignableSlots(),
                        .Block => new_object.asType(.Block).?.getAssignableSlots(),
                        else => unreachable,
                    };

                    for (assignable_slots) |*v| {
                        v.* = try traverseObjectGraphInner(v.*, context, callback, &current_link);
                    }

                    break :value new_object.asValue();
                },
                .Array => {
                    const new_object = try callback(context, old_object);
                    new_object.map = new_map;
                    const array = new_object.mustBeType(.Array);

                    for (array.getValues()) |*v| {
                        v.* = try traverseObjectGraphInner(value, context, callback, &current_link);
                    }

                    break :value new_object.asValue();
                },
                .ByteArray, .ActorProxy, .Managed => {
                    const new_object = try callback(context, old_object);
                    new_object.map = new_map;
                    break :value new_object.asValue();
                },
            }
        },
    };
}

pub fn traverseNonGloballyReachableObjectGraph(
    value: Value,
    context: anytype,
    callback: TraverseObjectGraphCallback(@TypeOf(context)),
) anyerror!Value {
    return try traverseObjectGraphInner(value, context, callback, null);
}
