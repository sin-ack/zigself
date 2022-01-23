// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const hash = @import("../../utility/hash.zig");
const Slot = @import("../slot.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const runtime_error = @import("../error.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

pub const LookupIntent = enum { Read, Assign };
pub const LookupError = Allocator.Error || runtime_error.SelfRuntimeError;
const VisitedValueLink = struct { previous: ?*const VisitedValueLink = null, value: Value };

pub fn findTraitsObject(comptime selector: []const u8, allocator: Allocator, context: *InterpreterContext) !Value {
    if (try context.lobby.getValue().lookup(.Read, "traits", allocator, context)) |traits| {
        if (try traits.lookup(.Read, selector, allocator, context)) |traits_object| {
            return traits_object;
        } else {
            return runtime_error.raiseError(allocator, context, "Could not find " ++ selector ++ " in traits", .{});
        }
    } else {
        return runtime_error.raiseError(allocator, context, "Could not find traits in lobby", .{});
    }
}

fn lookupReturnType(comptime intent: LookupIntent) type {
    if (intent == .Assign) {
        return LookupError!?*Value;
    } else {
        return LookupError!?Value;
    }
}

const self_hash = hash.stringHash("self");
const parent_hash = hash.stringHash("parent");

pub fn lookupByHash(
    self: Object,
    comptime intent: LookupIntent,
    selector_hash: u32,
    allocator: ?Allocator,
    context: ?*InterpreterContext,
) lookupReturnType(intent) {
    if (intent == .Read) {
        if (selector_hash == self_hash) {
            return self.asValue();
        }
    }

    return try lookupInternal(self, intent, selector_hash, null, allocator, context);
}

fn lookupInternal(
    self: Object,
    comptime intent: LookupIntent,
    selector_hash: u32,
    previously_visited: ?*const VisitedValueLink,
    allocator: ?Allocator,
    context: ?*InterpreterContext,
) lookupReturnType(intent) {
    switch (self.header.getObjectType()) {
        .ForwardingReference, .Map, .Method => unreachable,
        .Slots => {
            if (try slotsLookup(intent, Object.Slots, self.asSlotsObject(), selector_hash, previously_visited, allocator, context)) |value| {
                return value;
            }

            return null;
        },
        .Activation => {
            if (try slotsLookup(intent, Object.Activation, self.asActivationObject(), selector_hash, previously_visited, allocator, context)) |value| {
                return value;
            }

            // Receiver lookup
            if (try self.asActivationObject().receiver.lookupByHash(intent, selector_hash, allocator, context)) |value| {
                return value;
            }

            return null;
        },
        .Block => {
            // NOTE: executeMessage will handle the execution of the block itself.
            if (context) |ctx| {
                const traits_block = try findTraitsObject("block", allocator.?, ctx);
                if (intent == .Read) {
                    if (selector_hash == parent_hash)
                        return traits_block;
                }

                return try traits_block.lookupByHash(intent, selector_hash, allocator, context);
            } else {
                @panic("Context MUST be passed for Block objects!");
            }
        },
        .ByteVector => {
            if (context) |ctx| {
                const traits_string = try findTraitsObject("string", allocator.?, ctx);
                if (intent == .Read) {
                    if (selector_hash == parent_hash)
                        return traits_string;
                }

                return try traits_string.lookupByHash(intent, selector_hash, allocator, context);
            } else {
                @panic("Context MUST be passed for ByteVector objects!");
            }
        },
    }
}

/// Self Handbook, §3.3.8 The lookup algorithm
fn slotsLookup(
    comptime intent: LookupIntent,
    comptime ObjectType: type,
    object: *ObjectType,
    selector_hash: u32,
    previously_visited: ?*const VisitedValueLink,
    allocator: ?Allocator,
    context: ?*InterpreterContext,
) lookupReturnType(intent) {
    if (previously_visited) |visited| {
        var link: ?*const VisitedValueLink = visited;
        while (link) |l| {
            if (l.value.data == object.asValue().data) {
                // Cyclic reference
                return null;
            }

            link = l.previous;
        }
    }

    const currently_visited = VisitedValueLink{ .previous = previously_visited, .value = object.asValue() };

    // Note that we only return the value for the assign intent if it's assignable;
    // if it's not assignable, then we return null. This is important because
    // it prevents assigning to constant slots. Perhaps a primitive can help you
    // assign to a constant slot? It will cause a map copy though.
    var assignable_slots = object.getAssignableSlots();
    var assignable_slots_cursor: usize = 0;
    // Direct lookup
    for (object.getSlots()) |slot| {
        if (slot.hash == selector_hash) {
            if (intent == .Assign) {
                if (slot.isMutable()) {
                    return &assignable_slots[assignable_slots_cursor];
                } else {
                    // Prevent constant slots from being assigned
                    return null;
                }
            } else {
                if (slot.isMutable()) {
                    return assignable_slots[assignable_slots_cursor];
                } else {
                    return slot.value;
                }
            }
        }

        // FIXME: Don't keep a cursor; use the value field to hold the
        //        assignable slot index instead. It's currently sitting there
        //        doing nothing.
        if (slot.isMutable()) {
            assignable_slots_cursor += 1;
        }
    }

    // Parent lookup
    for (object.getSlots()) |slot| {
        if (slot.isParent()) {
            if (slot.value.isObjectReference()) {
                if (try lookupInternal(slot.value.asObject(), intent, selector_hash, &currently_visited, allocator, context)) |value| {
                    return value;
                }
            } else {
                @panic("FIXME: Allow integers and floating point numbers to be parent slot values (let me know of your usecase!)");
            }
        }
    }

    // Nope, not here
    return null;
}
