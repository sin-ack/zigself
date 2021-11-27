// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("./object.zig");
const Slot = @import("./slot.zig");

var global_nil: ?Object.Ref = null;
var global_true: ?Object.Ref = null;
var global_false: ?Object.Ref = null;
var has_been_torn_down = true;

/// Prepares important objects in the runtime. Note that these are the bare
/// essentials, beyond even those which can be set up by the world script; i.e.
/// required for basic objects and literal evaluation to function.
pub fn prepareRuntimeEnvironment(allocator: *Allocator) !Object.Ref {
    global_nil = try Object.createEmpty(allocator);
    errdefer global_nil.?.unrefWithAllocator(allocator);
    global_true = try Object.createEmpty(allocator);
    errdefer global_true.?.unrefWithAllocator(allocator);
    global_false = try Object.createEmpty(allocator);
    errdefer global_false.?.unrefWithAllocator(allocator);

    var lobby_slots = try makeLobbySlots(allocator);
    errdefer {
        for (lobby_slots) |*slot| {
            slot.deinit(allocator);
        }
        allocator.free(lobby_slots);
    }

    has_been_torn_down = false;
    return try Object.createSlots(allocator, lobby_slots);
}

fn makeLobbySlots(allocator: *Allocator) ![]Slot {
    var lobby_slots = try std.ArrayList(Slot).initCapacity(allocator, 1);
    errdefer lobby_slots.deinit();

    var traits_object = try makeTraitsObject(allocator);
    errdefer traits_object.unrefWithAllocator(allocator);

    var traits_slot = try Slot.init(allocator, false, false, "traits", traits_object);
    errdefer traits_slot.deinit(allocator);

    try lobby_slots.append(traits_slot);

    return lobby_slots.toOwnedSlice();
}

fn makeTraitsObject(allocator: *Allocator) !Object.Ref {
    var traits_slots = try makeTraitsSlots(allocator);
    errdefer {
        for (traits_slots) |*slot| {
            slot.deinit(allocator);
        }
        allocator.free(traits_slots);
    }

    return try Object.createSlots(allocator, traits_slots);
}

fn makeTraitsSlots(allocator: *Allocator) ![]Slot {
    var traits_slots = try std.ArrayList(Slot).initCapacity(allocator, 5);
    errdefer traits_slots.deinit();

    var integer_object = try Object.createEmpty(allocator);
    errdefer integer_object.unrefWithAllocator(allocator);
    var integer_slot = try Slot.init(allocator, false, false, "integer", integer_object);
    errdefer integer_slot.deinit(allocator);

    var float_object = try Object.createEmpty(allocator);
    errdefer float_object.unrefWithAllocator(allocator);
    var float_slot = try Slot.init(allocator, false, false, "float", float_object);
    errdefer float_slot.deinit(allocator);

    var string_object = try Object.createEmpty(allocator);
    errdefer string_object.unrefWithAllocator(allocator);
    var string_slot = try Slot.init(allocator, false, false, "string", string_object);
    errdefer string_slot.deinit(allocator);

    var block_object = try Object.createEmpty(allocator);
    errdefer block_object.unrefWithAllocator(allocator);
    var block_slot = try Slot.init(allocator, false, false, "block", block_object);
    errdefer block_slot.deinit(allocator);

    var vector_object = try Object.createEmpty(allocator);
    errdefer vector_object.unrefWithAllocator(allocator);
    var vector_slot = try Slot.init(allocator, false, false, "vector", vector_object);
    errdefer vector_slot.deinit(allocator);

    try traits_slots.append(integer_slot);
    try traits_slots.append(float_slot);
    try traits_slots.append(string_slot);
    try traits_slots.append(block_slot);
    try traits_slots.append(vector_slot);

    return traits_slots.toOwnedSlice();
}

/// Return the global nil object. The nil object is ref'd before returning.
pub fn globalNil() Object.Ref {
    const nil_object = global_nil.?;
    nil_object.ref();
    return nil_object;
}

/// Return the global true object. The true object is ref'd before returning.
pub fn globalTrue() Object.Ref {
    const true_object = global_true.?;
    true_object.ref();
    return true_object;
}

/// Return the global false object. The false object is ref'd before returning.
pub fn globalFalse() Object.Ref {
    const false_object = global_false.?;
    false_object.ref();
    return false_object;
}

pub fn teardownGlobalObjects(allocator: *Allocator) void {
    if (global_true) |*true_object| {
        true_object.unrefWithAllocator(allocator);
        global_true = null;
    }

    if (global_false) |*false_object| {
        false_object.unrefWithAllocator(allocator);
        global_false = null;
    }

    if (global_nil) |*nil_object| {
        nil_object.unrefWithAllocator(allocator);
        global_nil = null;
    }

    has_been_torn_down = true;
}

pub fn hasBeenTornDown() bool {
    return has_been_torn_down;
}
