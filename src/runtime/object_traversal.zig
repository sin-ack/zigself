// Copyright (c) 2022-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const Map = @import("map.zig").Map;
const Slot = @import("slot.zig").Slot;
const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const MapObject = @import("object.zig").MapObject;

const TraverseObjectGraphLink = struct {
    previous: ?*const @This(),
    address: [*]u64,
};

fn traverseObjectGraphInner(
    value: Value,
    // TODO: Write interfaces proposal for Zig
    visitor: anytype,
    previous_link: ?*const TraverseObjectGraphLink,
) anyerror!Value {
    return switch (value.getType()) {
        .ObjectMarker => unreachable,
        .Integer, .FloatingPoint => value,
        .ObjectReference => value: {
            const old_base_object = value.asBaseObject();
            const old_object_address = value.asObjectAddress();

            // Globally reachable objects are never traversed.
            if (old_base_object.metadata.reachability == .Global)
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
                        const new_base_object = try visitor.visit(old_base_object);
                        break :value new_base_object.asValue();
                    }
                }
            }

            const current_link = TraverseObjectGraphLink{ .previous = previous_link, .address = old_object_address };

            switch (old_base_object.metadata.type) {
                .Object => {
                    const old_object = old_base_object.asObject().?;
                    const old_object_type = old_object.getMetadata().type;

                    // TODO: Have a more robust way of knowing which object
                    //       types contain a map. Maybe MapObject could hold
                    //       a register of which object types contain a map.
                    const new_map = switch (old_object_type) {
                        .Slots,
                        .Method,
                        .Block,
                        .Array,
                        .AddrInfo,
                        => try traverseObjectGraphInner(@as(MapObject.Ptr, @ptrCast(old_object)).map, visitor, &current_link),
                        else => null,
                    };
                    // FIXME: Move this switch into object delegation.
                    switch (old_object_type) {
                        .ForwardedObject, .Activation, .Actor => unreachable,
                        .Slots, .Method, .Block => {
                            const new_base_object = (try visitor.visit(old_base_object)).asObject().?;
                            @as(MapObject.Ptr, @ptrCast(new_base_object)).map = new_map.?;

                            const assignable_slots = switch (old_object_type) {
                                // FIXME: Move this to something like Object.getSlots().
                                .Slots => new_base_object.asType(.Slots).?.getAssignableSlots(),
                                .Method => new_base_object.asType(.Method).?.getAssignableSlots(),
                                .Block => new_base_object.asType(.Block).?.getAssignableSlots(),
                                else => unreachable,
                            };

                            for (assignable_slots) |*v| {
                                v.* = try traverseObjectGraphInner(v.*, visitor, &current_link);
                            }

                            break :value new_base_object.asValue();
                        },
                        .Array => {
                            const array = (try visitor.visit(old_base_object)).asObject().?.asType(.Array).?;
                            array.object.map = new_map.?;

                            for (array.getValues()) |*v| {
                                v.* = try traverseObjectGraphInner(value, visitor, &current_link);
                            }

                            break :value array.asValue();
                        },
                        .AddrInfo => {
                            const new_object = (try visitor.visit(old_base_object)).asObject().?.asType(.AddrInfo).?;
                            new_object.object.map = new_map.?;
                            break :value new_object.asValue();
                        },
                        .ByteArray, .ActorProxy, .Managed, .Float => {
                            const new_base_object = try visitor.visit(old_base_object);
                            break :value new_base_object.asValue();
                        },
                    }
                },
                .Map => {
                    const old_map = old_base_object.asMap().?;
                    const map_type = old_map.getMetadata().type;
                    // Copy the map itself if necessary
                    const new_map = switch (map_type) {
                        .Slots, .Method, .Block => (try visitor.visit(old_base_object)).asMap().?,
                        // Array maps will always be immutable, because they
                        // don't point to anything that's not globally
                        // reachable.
                        .Array => old_map,
                        // TODO: This is incorrect, move object traversal
                        //       responsibility to the object type.
                        .AddrInfo => old_map,
                    };

                    // Traverse the map contents and update anything non-globally reachable
                    switch (map_type) {
                        .Slots, .Method, .Block => {
                            const slots: Slot.Slice = switch (map_type) {
                                // TODO: This is incorrect, move object
                                //       traversal responsibility to the object
                                //       type.
                                .AddrInfo => unreachable,
                                .Array => unreachable,
                                inline else => |t| new_map.asType(t).?.getSlots(),
                            };

                            for (slots) |*slot| {
                                if (!slot.isAssignable()) {
                                    slot.value = try traverseObjectGraphInner(slot.value, visitor, &current_link);
                                }
                            }
                        },
                        .Array, .AddrInfo => {},
                    }

                    break :value new_map.asValue();
                },
            }
        },
    };
}

pub fn traverseNonGloballyReachableObjectGraph(
    value: Value,
    // TODO: Write interfaces proposal for Zig
    visitor: anytype,
) anyerror!Value {
    return try traverseObjectGraphInner(value, visitor, null);
}
