// Copyright (c) 2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Slot = @import("slot.zig").Slot;
const Value = @import("value.zig").Value;
const Object = @import("object.zig").Object;
const LookupResult = @import("object_lookup.zig").LookupResult;
const utility_hash = @import("../utility/hash.zig");

name: []const u8,
hash: SelectorHash,
assignment_hash: SelectorHash = SelectorHash.Null,

const Selector = @This();
pub const VisitedValueLink = struct { previous: ?*const VisitedValueLink = null, value: Value };

/// Initialize a new selector from a name.
pub fn fromName(name: []const u8) Selector {
    std.debug.assert(name.len > 0);

    const hash = SelectorHash.fromSelector(name);
    // We compute an additional hash for assignment selectors, so that we can
    // quickly check whether we could assign to an assignable slot without
    // recomputing the hash.
    const assignment_hash = if (looksLikeAssignment(name))
        SelectorHash.fromSelector(name[0 .. name.len - 1])
    else
        .Null;

    return .{
        .name = name,
        .hash = hash,
        .assignment_hash = assignment_hash,
    };
}

/// Initialize a new selector from a slot on an object.
pub fn fromSlot(slot: Slot) Selector {
    return .{
        .name = slot.getName(),
        .hash = @enumFromInt(slot.getHash()),
    };
}

/// Return whether this selector looks like an assignment (foo:).
fn looksLikeAssignment(selector: []const u8) bool {
    return selector.len > 1 and std.mem.indexOfScalar(u8, selector, ':') == selector.len - 1;
}

/// Return whether two selectors are equal.
pub fn equals(self: Selector, other: Selector) bool {
    return self.hash == other.hash;
}

/// Return whether this selector is an appropriate assignment selector for the
/// other selector. This is relevant in cases of of assignable slots which have
/// an implicit assignment selector; so assignable slot "foo" accepts message
/// "foo:" for assignment.
pub fn canAssignTo(self: Selector, other: Selector) bool {
    return self.assignment_hash == other.hash;
}

/// Start a lookup with this selector hash on the given object.
pub fn lookupObject(self: Selector, object: Object.Ptr) LookupResult {
    return self.chainedLookupObject(object, null);
}

/// Perform a lookup with an existing visitor chain. This is used to detect
/// cycles in the lookup chain.
pub fn chainedLookupObject(self: Selector, object: Object.Ptr, previously_visited: ?*const VisitedValueLink) LookupResult {
    return object.lookup(self, previously_visited);
}

pub fn format(
    self: Selector,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try std.fmt.format(writer, "\"{s}\"", .{self.name});
    // TODO: Add a debug flag that displays the hash.
}

/// Well-known selectors.
pub const well_known = struct {
    pub const self = fromName("self");
    pub const parent = fromName("parent");
    pub const value = fromName("value");
    pub const @"value:" = fromName("value:");
};

const SelectorHash = enum(u32) {
    Null = 0,
    _,

    pub fn fromSelector(selector: []const u8) SelectorHash {
        return @enumFromInt(utility_hash.stringHash(selector));
    }

    pub fn getHash(self: SelectorHash) u32 {
        std.debug.assert(self != .Null);
        return @intFromEnum(self);
    }
};
