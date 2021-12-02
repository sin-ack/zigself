// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig");
const ByteVector = @import("./byte_vector.zig");

var global_nil: ?Value = null;
var global_true: ?Value = null;
var global_false: ?Value = null;
var has_been_torn_down = true;

/// Prepares important objects in the runtime. Note that these are the bare
/// essentials, beyond even those which can be set up by the world script; i.e.
/// required for basic objects and literal evaluation to function.
pub fn prepareRuntimeEnvironment(heap: *Heap) !Value {
    var empty_map = try Object.Map.Slots.create(heap, 0);

    // FIXME: We can't keep track of objects in the heap like this, find another
    //        way.
    global_nil = (try Object.Slots.create(heap, empty_map, &.{})).asValue();
    global_true = (try Object.Slots.create(heap, empty_map, &.{})).asValue();
    global_false = (try Object.Slots.create(heap, empty_map, &.{})).asValue();

    var lobby = try makeLobbyObject(heap);
    has_been_torn_down = false;
    return lobby.asValue();
}

fn makeLobbyObject(heap: *Heap) !*Object.Slots {
    var traits_object = try makeTraitsObject(heap);
    var traits_name = try ByteVector.createFromString(heap, "traits");

    var lobby_map = try Object.Map.Slots.create(heap, 1);
    lobby_map.getSlots()[0].initConstant(traits_name, .NotParent, traits_object.asValue());

    return try Object.Slots.create(heap, lobby_map, &.{});
}

fn makeTraitsObject(heap: *Heap) !*Object.Slots {
    var traits_map = try makeTraitsMap(heap);
    return try Object.Slots.create(heap, traits_map, &.{});
}

fn makeTraitsMap(heap: *Heap) !*Object.Map.Slots {
    var integer_name = try ByteVector.createFromString(heap, "integer");
    var float_name = try ByteVector.createFromString(heap, "float");
    var string_name = try ByteVector.createFromString(heap, "string");
    var block_name = try ByteVector.createFromString(heap, "block");
    var vector_name = try ByteVector.createFromString(heap, "vector");

    var empty_map = try Object.Map.Slots.create(heap, 0);

    var integer_object = try Object.Slots.create(heap, empty_map, &.{});
    var float_object = try Object.Slots.create(heap, empty_map, &.{});
    var string_object = try Object.Slots.create(heap, empty_map, &.{});
    var block_object = try Object.Slots.create(heap, empty_map, &.{});
    var vector_object = try Object.Slots.create(heap, empty_map, &.{});

    var traits_map = try Object.Map.Slots.create(heap, 5);
    var traits_slots = traits_map.getSlots();

    traits_slots[0].initConstant(integer_name, .NotParent, integer_object.asValue());
    traits_slots[1].initConstant(float_name, .NotParent, float_object.asValue());
    traits_slots[2].initConstant(string_name, .NotParent, string_object.asValue());
    traits_slots[3].initConstant(block_name, .NotParent, block_object.asValue());
    traits_slots[4].initConstant(vector_name, .NotParent, vector_object.asValue());

    return traits_map;
}

/// Return the global nil object. The nil object is ref'd before returning.
pub fn globalNil() Value {
    return global_nil.?;
}

/// Return the global true object. The true object is ref'd before returning.
pub fn globalTrue() Value {
    return global_true.?;
}

/// Return the global false object. The false object is ref'd before returning.
pub fn globalFalse() Value {
    return global_false.?;
}

pub fn teardownGlobalObjects() void {
    // FIXME: Stop tracking global objects here
    has_been_torn_down = true;
}

pub fn hasBeenTornDown() bool {
    return has_been_torn_down;
}
