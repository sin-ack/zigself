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

pub const LookupError = Allocator.Error;
const VisitedValueLink = struct { previous: ?*const VisitedValueLink = null, value: Value };

/// The result of a lookup operation.
pub const LookupResult = union(enum) {
    /// Nothing matching the selector was found.
    Nothing: void,
    /// A regular slot was matched (either a method which should be activated,
    /// or something else which can be returned as-is).
    Regular: Value,
    /// An assignable slot was found and the passed selector is intended to
    /// assign to it. The value pointer can be safely assigned to. Keep in
    /// mind that if you do this, then you will have to also remember the
    /// object reference in the heap.
    Assignment: struct {
        object: Object,
        value_ptr: *Value,
    },
};

/// The hash information of a selector.
pub const SelectorHash = struct {
    /// The hash of the selector itself.
    regular: u32,
    /// If this is not null, then this selector is of the form `foo:` and this
    /// hash is the hash of `foo`. It is intended to be compared with assignable
    /// slots in assignment lookups.
    assignment_target: ?u32,

    pub fn init(selector: []const u8) SelectorHash {
        const regular_hash = hash.stringHash(selector);
        // If the only : in this selector is at the end, then compute an
        // assignment target hash for it.
        const assignment_target_hash = if (std.mem.indexOfScalar(u8, selector, ':') == selector.len - 1)
            hash.stringHash(selector[0 .. selector.len - 1])
        else
            null;

        return .{ .regular = regular_hash, .assignment_target = assignment_target_hash };
    }
};

// Well-known hashes
const self_hash = hash.stringHash("self");
const parent_hash = hash.stringHash("parent");
const value_hash = hash.stringHash("value");

pub fn lookupByHash(
    self: Object,
    context: *InterpreterContext,
    selector_hash: SelectorHash,
    source_range: SourceRange,
) LookupError!LookupResult {
    if (LOOKUP_DEBUG) std.debug.print("Object.lookupByHash: Looking up hash {x} on a {} object at {*}\n", .{ selector_hash, self.header.getObjectType(), self.header });
    if (selector_hash.regular == self_hash) {
        return LookupResult{ .Regular = self.asValue() };
    }

    return try lookupInternal(self, context, selector_hash, null, source_range);
}

const nothing = LookupResult{ .Nothing = .{} };
fn lookupInternal(
    self: Object,
    context: *InterpreterContext,
    selector_hash: SelectorHash,
    previously_visited: ?*const VisitedValueLink,
    source_range: SourceRange,
) LookupError!LookupResult {
    switch (self.header.getObjectType()) {
        .ForwardingReference, .Map, .Method => unreachable,
        .Slots => {
            return try slotsLookup(context, Object.Slots, self.asSlotsObject(), selector_hash, previously_visited, source_range);
        },
        .Activation => {
            const slots_lookup_result = try slotsLookup(context, Object.Activation, self.asActivationObject(), selector_hash, previously_visited, source_range);
            if (slots_lookup_result != .Nothing) return slots_lookup_result;

            // Receiver lookup
            return try self.asActivationObject().receiver.lookupByHash(context, selector_hash, source_range);
        },
        .Block => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at traits block\n", .{});
            // NOTE: executeMessage will handle the execution of the block itself.
            const block_traits = context.vm.block_traits.getValue();
            if (selector_hash.regular == parent_hash)
                return LookupResult{ .Regular = block_traits };

            return try block_traits.lookupByHash(context, selector_hash, source_range);
        },
        .Array => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at traits array\n", .{});
            const array_traits = context.vm.array_traits.getValue();
            if (selector_hash.regular == parent_hash)
                return LookupResult{ .Regular = array_traits };

            return try array_traits.lookupByHash(context, selector_hash, source_range);
        },
        .ByteArray => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at traits string\n", .{});
            const string_traits = context.vm.string_traits.getValue();
            if (selector_hash.regular == parent_hash)
                return LookupResult{ .Regular = string_traits };

            return try string_traits.lookupByHash(context, selector_hash, source_range);
        },
        .Managed => {
            if (LOOKUP_DEBUG) std.debug.print("Object.lookupInternal: Looking at a managed object type: {}\n", .{self.asManaged().getManagedType()});
            if (selector_hash.regular == value_hash) {
                return LookupResult{ .Regular = self.asManaged().value };
            }

            return nothing;
        },
    }
}

/// Self Handbook, §3.3.8 The lookup algorithm
fn slotsLookup(
    context: *InterpreterContext,
    comptime ObjectType: type,
    object: *ObjectType,
    selector_hash: SelectorHash,
    previously_visited: ?*const VisitedValueLink,
    source_range: SourceRange,
) LookupError!LookupResult {
    if (previously_visited) |visited| {
        var link: ?*const VisitedValueLink = visited;
        while (link) |l| {
            if (l.value.data == object.asValue().data) {
                // Cyclic reference
                return nothing;
            }

            link = l.previous;
        }
    }

    const currently_visited = VisitedValueLink{ .previous = previously_visited, .value = object.asValue() };

    // Direct lookup
    for (object.getSlots()) |slot| {
        if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Comparing slot \"{s}\" (hash {x}) vs. our hash {x}\n", .{ slot.name.asByteArray().getValues(), slot.getHash(), selector_hash });
        const slot_hash = slot.getHash();
        if (slot.isAssignable()) {
            if (selector_hash.assignment_target) |assignment_target_hash| {
                if (slot_hash == assignment_target_hash)
                    return LookupResult{
                        .Assignment = .{
                            .object = Object.fromAddress(object.asObjectAddress()),
                            .value_ptr = object.getAssignableSlotValue(slot),
                        },
                    };
            }

            if (slot_hash == selector_hash.regular)
                return LookupResult{ .Regular = object.getAssignableSlotValue(slot).* };
        }

        if (slot_hash == selector_hash.regular)
            return LookupResult{ .Regular = slot.value };
    }

    if (SLOTS_LOOKUP_DEBUG) std.debug.print("Object.slotsLookup: Could not find the slot on this object, looking at parents\n", .{});

    // Parent lookup
    for (object.getSlots()) |slot| {
        if (slot.isParent()) {
            const slot_value = if (slot.isAssignable())
                object.getAssignableSlotValue(slot).*
            else
                slot.value;

            if (slot_value.isObjectReference()) {
                const parent_lookup_result = try lookupInternal(slot_value.asObject(), context, selector_hash, &currently_visited, source_range);
                if (parent_lookup_result != .Nothing) return parent_lookup_result;
            } else {
                @panic("FIXME: Allow integers and floating point numbers to be parent slot values (let me know of your usecase!)");
            }
        }
    }

    // Nope, not here
    return nothing;
}
