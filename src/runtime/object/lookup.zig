// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const hash = @import("../../utility/hash.zig");
const Slot = @import("../slot.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Completion = @import("../completion.zig");
const SourceRange = @import("../../language/source_range.zig");
const InterpreterContext = @import("../interpreter.zig").InterpreterContext;
const debug = @import("../../debug.zig");

const SLOTS_LOOKUP_DEBUG = debug.SLOTS_LOOKUP_DEBUG;
const LOOKUP_DEBUG = debug.LOOKUP_DEBUG;

pub const LookupIntent = enum { Read, Assign };
pub const LookupError = Allocator.Error;
const VisitedValueLink = struct { previous: ?*const VisitedValueLink = null, value: Value };

pub const AssignLookupResult = union(enum) {
    Result: struct {
        object: Object,
        value_ptr: *Value,
    },
    Completion: Completion,
};

// NOTE: Only intended to share with value.zig

pub fn lookupReturnType(comptime intent: LookupIntent) type {
    if (intent == .Assign) {
        return LookupError!?AssignLookupResult;
    } else {
        return LookupError!?Completion;
    }
}

const self_hash = hash.stringHash("self");
const parent_hash = hash.stringHash("parent");
const value_hash = hash.stringHash("value");

pub fn lookupByHash(
    self: Object,
    comptime intent: LookupIntent,
    context: *InterpreterContext,
    selector_hash: u32,
    source_range: SourceRange,
) lookupReturnType(intent) {
    if (LOOKUP_DEBUG) std.debug.print("Object.lookupByHash: Looking up hash {x} on a {} object at {*}\n", .{ selector_hash, self.header.getObjectType(), self.header });
    if (intent == .Read) {
        if (selector_hash == self_hash) {
            return Completion.initNormal(self.asValue());
        }
    }

    return try lookupInternal(self, intent, context, selector_hash, null, source_range);
}

fn lookupInternal(
    self: Object,
    comptime intent: LookupIntent,
    context: *InterpreterContext,
    selector_hash: u32,
    previously_visited: ?*const VisitedValueLink,
    source_range: SourceRange,
) lookupReturnType(intent) {
    switch (self.header.getObjectType()) {
        .ForwardingReference, .Map, .Method => unreachable,
        .Slots => {
            if (try slotsLookup(intent, context, Object.Slots, self.asSlotsObject(), selector_hash, previously_visited, source_range)) |result| {
                return result;
            }

            return null;
        },
        .Activation => {
            if (try slotsLookup(intent, context, Object.Activation, self.asActivationObject(), selector_hash, previously_visited, source_range)) |result| {
                return result;
            }

            // Receiver lookup
            if (try self.asActivationObject().receiver.lookupByHash(intent, context, selector_hash, source_range)) |result| {
                return result;
            }

            return null;
        },
        .Block => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at traits block\n", .{});
            // NOTE: executeMessage will handle the execution of the block itself.
            const block_traits = context.vm.block_traits.getValue();
            if (intent == .Read) {
                if (selector_hash == parent_hash)
                    return Completion.initNormal(block_traits);
            }

            return try block_traits.lookupByHash(intent, context, selector_hash, source_range);
        },
        .Array => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at traits array\n", .{});
            const array_traits = context.vm.array_traits.getValue();
            if (intent == .Read) {
                if (selector_hash == parent_hash)
                    return Completion.initNormal(array_traits);
            }

            return try array_traits.lookupByHash(intent, context, selector_hash, source_range);
        },
        .ByteArray => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at traits string\n", .{});
            const string_traits = context.vm.string_traits.getValue();
            if (intent == .Read) {
                if (selector_hash == parent_hash)
                    return Completion.initNormal(string_traits);
            }

            return try string_traits.lookupByHash(intent, context, selector_hash, source_range);
        },
        .Managed => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at a managed object type: {}\n", .{self.asManaged().getManagedType()});
            if (intent == .Read and selector_hash == value_hash) {
                return Completion.initNormal(self.asManaged().value);
            }
            return null;
        },
    }
}

/// Self Handbook, §3.3.8 The lookup algorithm
fn slotsLookup(
    comptime intent: LookupIntent,
    context: *InterpreterContext,
    comptime ObjectType: type,
    object: *ObjectType,
    selector_hash: u32,
    previously_visited: ?*const VisitedValueLink,
    source_range: SourceRange,
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

    // Direct lookup
    for (object.getSlots()) |slot| {
        if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Comparing slot \"{s}\" (hash {x}) vs. our hash {x}\n", .{ slot.name.asByteArray().getValues(), slot.getHash(), selector_hash });
        if (slot.getHash() == selector_hash) {
            if (intent == .Assign) {
                if (slot.isAssignable()) {
                    return AssignLookupResult{
                        .Result = .{
                            .object = Object.fromAddress(object.asObjectAddress()),
                            .value_ptr = object.getAssignableSlotValue(slot),
                        },
                    };
                } else {
                    // Prevent constant slots from being assigned
                    return null;
                }
            } else {
                if (slot.isAssignable()) {
                    return Completion.initNormal(object.getAssignableSlotValue(slot).*);
                } else {
                    return Completion.initNormal(slot.value);
                }
            }
        }
    }

    if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Could not find the slot on this object, looking at parents\n", .{});

    // Parent lookup
    for (object.getSlots()) |slot| {
        if (slot.isParent()) {
            if (slot.value.isObjectReference()) {
                if (try lookupInternal(slot.value.asObject(), intent, context, selector_hash, &currently_visited, source_range)) |result| {
                    return result;
                }
            } else {
                @panic("FIXME: Allow integers and floating point numbers to be parent slot values (let me know of your usecase!)");
            }
        }
    }

    // Nope, not here
    return null;
}
