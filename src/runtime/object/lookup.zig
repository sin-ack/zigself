// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const hash = @import("../../utility/hash.zig");
const Slot = @import("../slot.zig");
const Object = @import("../object.zig");
const runtime_error = @import("../error.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;

const LookupType = enum { Value, Slot };
const LookupError = Allocator.Error || runtime_error.SelfRuntimeError;
const VisitedObjectLink = struct { previous: ?*const VisitedObjectLink = null, object: *Object };

fn lookupReturnType(comptime lookup_type: LookupType) type {
    if (lookup_type == .Value) {
        return LookupError!?Object.Ref;
    } else {
        return LookupError!?*Slot;
    }
}

pub fn lookup(self: *Object, context: ?*InterpreterContext, selector: []const u8, comptime lookup_type: LookupType) lookupReturnType(lookup_type) {
    const selector_hash = hash.stringHash(selector);
    return lookupHash(self, context, selector_hash, lookup_type);
}

const self_hash = hash.stringHash("self");
const parent_hash = hash.stringHash("parent");

fn lookupHash(self: *Object, context: ?*InterpreterContext, selector_hash: u32, comptime lookup_type: LookupType) lookupReturnType(lookup_type) {
    if (lookup_type == .Value) {
        if (selector_hash == self_hash) {
            return Object.Ref{ .value = self };
        }
    }

    return if (lookup_type == .Value)
        try lookupValue(self, context, selector_hash, null)
    else
        try lookupSlot(self, selector_hash, null);
}

fn lookupValue(self: *Object, context: ?*InterpreterContext, selector_hash: u32, previously_visited: ?*const VisitedObjectLink) LookupError!?Object.Ref {
    if (previously_visited) |visited| {
        var link: ?*const VisitedObjectLink = visited;
        while (link) |l| {
            if (l.object == self) {
                // Cyclic reference
                return null;
            }

            link = l.previous;
        }
    }

    const currently_visited = VisitedObjectLink{ .previous = previously_visited, .object = self };

    switch (self.content) {
        .Empty => return null,

        .Slots => |slots| {
            if (try slotsLookup(slots.slots, selector_hash, &currently_visited)) |slot| {
                return slot.value;
            }

            return null;
        },

        .Activation => |activation| {
            if (try slotsLookup(activation.slots, selector_hash, &currently_visited)) |slot| {
                return slot.value;
            }

            // Receiver lookup
            if (try lookupValue(activation.receiver.value, context, selector_hash, &currently_visited)) |found_object| {
                return found_object;
            }

            return null;
        },

        .Method => unreachable,

        // The 4 types below have an imaginary field called "parent" which
        // refers to their respective traits objects.

        .Block => {
            // NOTE: executeMessage will handle the execution of the block itself.
            if (context) |ctx| {
                const traits_block = try lookupTraitsObject(self.allocator, ctx, "block");
                if (selector_hash == parent_hash)
                    return traits_block;

                return try lookupHash(traits_block.value, ctx, selector_hash, .Value);
            } else {
                @panic("Context MUST be passed for Block objects!");
            }
        },

        .ByteVector => {
            if (context) |ctx| {
                const traits_string = try lookupTraitsObject(self.allocator, ctx, "string");
                if (selector_hash == parent_hash)
                    return traits_string;

                return try lookupHash(traits_string.value, ctx, selector_hash, .Value);
            } else {
                @panic("Context MUST be passed for ByteVector objects!");
            }
        },

        .Vector => {
            if (context) |ctx| {
                const traits_vector = try lookupTraitsObject(self.allocator, ctx, "vector");
                if (selector_hash == parent_hash)
                    return traits_vector;

                return try lookupHash(traits_vector.value, ctx, selector_hash, .Value);
            } else {
                @panic("Context MUST be passed for Vector objects!");
            }
        },

        .Integer => {
            if (context) |ctx| {
                const traits_integer = try lookupTraitsObject(self.allocator, ctx, "integer");
                if (selector_hash == parent_hash)
                    return traits_integer;

                return try lookupHash(traits_integer.value, ctx, selector_hash, .Value);
            } else {
                @panic("Context MUST be passed for Integer objects!");
            }
        },

        .FloatingPoint => {
            if (context) |ctx| {
                const traits_float = try lookupTraitsObject(self.allocator, ctx, "float");
                if (selector_hash == parent_hash)
                    return traits_float;

                return try lookupHash(traits_float.value, ctx, selector_hash, .Value);
            } else {
                @panic("Context MUST be passed for FloatingPoint objects!");
            }
        },
    }
}

/// Like lookupValue but finds slots instead of values.
fn lookupSlot(self: *Object, selector_hash: u32, previously_visited: ?*const VisitedObjectLink) LookupError!?*Slot {
    // I'd like this to not be duplicated but unfortunately I couldn't reconcile
    // them.
    if (previously_visited) |visited| {
        var link: ?*const VisitedObjectLink = visited;
        while (link) |l| {
            if (l.object == self) {
                // Cyclic reference
                return null;
            }

            link = l.previous;
        }
    }

    const currently_visited = VisitedObjectLink{ .previous = previously_visited, .object = self };

    switch (self.content) {
        .Empty, .Block, .ByteVector, .Vector, .Integer, .FloatingPoint => return null,

        .Slots => |slots| {
            return try slotsLookup(slots.slots, selector_hash, &currently_visited);
        },

        // FIXME: Don't repeat this code
        .Activation => |activation| {
            if (try slotsLookup(activation.slots, selector_hash, &currently_visited)) |slot| {
                return slot;
            }

            // Receiver lookup
            if (try lookupSlot(activation.receiver.value, selector_hash, &currently_visited)) |found_slot| {
                return found_slot;
            }

            return null;
        },

        .Method => @panic("Attempting to perform lookup on method?!"),
    }
}

fn lookupTraitsObject(allocator: *Allocator, context: *InterpreterContext, comptime selector: []const u8) !Object.Ref {
    if (try context.lobby.value.lookup(context, "traits", .Value)) |traits| {
        if (try traits.value.lookup(context, selector, .Value)) |traits_object| {
            return traits_object;
        } else {
            return runtime_error.raiseError(allocator, context, "Could not find " ++ selector ++ " in traits", .{});
        }
    } else {
        return runtime_error.raiseError(allocator, context, "Could not find traits in lobby", .{});
    }
}

/// Self Handbook, §3.3.8 The lookup algorithm
fn slotsLookup(slots: []Slot, selector_hash: u32, currently_visited: *const VisitedObjectLink) !?*Slot {
    // Direct lookup
    for (slots) |*slot| {
        if (slot.name_hash == selector_hash) {
            return slot;
        }
    }

    // Parent lookup
    for (slots) |slot| {
        if (slot.is_parent) {
            if (try lookupSlot(slot.value.value, selector_hash, currently_visited)) |found_slot| {
                return found_slot;
            }
        }
    }

    // Nope, not here
    return null;
}
