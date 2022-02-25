// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("./slot.zig").Slot;
const Object = @import("./object.zig");
const Value = @import("./value.zig").Value;
const environment = @import("./environment.zig");

pub const InspectDisplayType = enum { Inline, Multiline };
const VisitedObjectLink = struct {
    object: Object,
    previous: ?*const VisitedObjectLink,
};

pub fn inspectValue(comptime display_type: InspectDisplayType, value: Value) !void {
    return inspectValueInternal(display_type, value, 0, null);
}

fn inspectValueInternal(comptime display_type: InspectDisplayType, value: Value, indent: usize, visited_object_link: ?*const VisitedObjectLink) !void {
    switch (value.getType()) {
        .ObjectMarker => unreachable,
        .Integer => std.debug.print("<integer> {d}", .{value.asInteger()}),
        .FloatingPoint => std.debug.print("<floating point> {d}", .{value.asFloatingPoint()}),
        .ObjectReference => return inspectObject(display_type, value.asObject(), indent, visited_object_link),
    }
}

// FIXME: Move this to object code.
fn inspectObject(
    comptime display_type: InspectDisplayType,
    object: Object,
    indent: usize,
    visited_object_link: ?*const VisitedObjectLink,
) Allocator.Error!void {
    const separator = switch (display_type) {
        .Inline => " ",
        .Multiline => "\n",
    };

    // See if we have a cyclic reference
    var link_it = visited_object_link;
    while (link_it) |link| {
        if (link.object.getAddress() == object.getAddress()) {
            std.debug.print("<cyclic reference>", .{});
            return;
        }

        link_it = link.previous;
    }

    const is_root_object = visited_object_link == null;
    const my_link = VisitedObjectLink{ .object = object, .previous = visited_object_link };

    // If the global objects are printed during inspect and they haven't been
    // printed directly, then print them as a summary.
    if (!is_root_object and !environment.hasBeenTornDown()) {
        const nil_value = environment.globalNil();
        if (object.asValue().data == nil_value.data) {
            std.debug.print("<global nil>", .{});
            return;
        }

        const true_value = environment.globalTrue();
        if (object.asValue().data == true_value.data) {
            std.debug.print("<global true>", .{});
            return;
        }

        const false_value = environment.globalFalse();
        if (object.asValue().data == false_value.data) {
            std.debug.print("<global false>", .{});
            return;
        }
    }

    switch (object.header.getObjectType()) {
        .ForwardingReference, .Map => unreachable,
        .Slots => {
            const slots = object.asSlotsObject();

            std.debug.print("(|{s}", .{separator});
            try inspectSlots(display_type, "getSlots", slots, indent + 2, separator, &my_link);
            printWithIndent(display_type, indent, "|)", .{});
        },
        .Activation => {
            const activation = object.asSlotsObject();

            std.debug.print("<activation object> (|{s}", .{separator});
            try inspectSlots(display_type, "getSlots", activation, indent + 2, separator, &my_link);
            printWithIndent(display_type, indent, "|)", .{});
        },
        .Method => {
            const method = object.asMethodObject();

            std.debug.print("<method object \"{s}\" [", .{method.getMap().method_name.asByteArray().getValues()});
            for (method.getMap().getArgumentSlots()) |slot, i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("\"{s}\"", .{slot.name.asByteArray().getValues()});
            }
            std.debug.print("]> ", .{});

            if (method.getMap().getNonArgumentSlots().len > 0) {
                std.debug.print("(|{s}", .{separator});
                try inspectSlots(display_type, "getNonArgumentSlots", method, indent + 2, separator, &my_link);
                printWithIndent(display_type, indent, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },
        .Block => {
            const block = object.asBlockObject();

            std.debug.print("<block object [", .{});
            for (block.getMap().getArgumentSlots()) |slot, i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("\"{s}\"", .{slot.name.asByteArray().getValues()});
            }
            std.debug.print("]> ", .{});

            if (block.getMap().getNonArgumentSlots().len > 0) {
                std.debug.print("(|{s}", .{separator});
                try inspectSlots(display_type, "getNonArgumentSlots", block, indent + 2, separator, &my_link);
                printWithIndent(display_type, indent, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },
        .ByteArray => {
            const byte_array = object.asByteArrayObject();
            const values = byte_array.getValues();
            std.debug.print("<byte array size: {d}> \"{s}\"", .{ values.len, values });
        },
        .Array => {
            const array = object.asArrayObject();
            const values = array.getValues();

            std.debug.print("<array size: {d}> ", .{values.len});
            if (values.len == 0) {
                std.debug.print("[]", .{});
            } else {
                std.debug.print("[{s}", .{separator});

                for (values) |value, i| {
                    printWithIndent(display_type, indent + 2, "", .{});
                    try inspectValueInternal(display_type, value, indent + 2, &my_link);

                    if (i < values.len - 1) std.debug.print(",", .{});
                    std.debug.print(separator, .{});
                }

                printWithIndent(display_type, indent, "]", .{});
            }
        },
        .Managed => {
            const managed = object.asManaged();
            std.debug.print("<managed object: {}> ", .{managed.getManagedType()});
            try inspectValueInternal(display_type, managed.value, indent, &my_link);
        },
    }
}

fn inspectSlots(
    comptime display_type: InspectDisplayType,
    comptime slot_getter: []const u8,
    object: anytype,
    indent: usize,
    separator: []const u8,
    visited_object_link: *const VisitedObjectLink,
) !void {
    const map = object.getMap();
    const slots: []Slot = @call(.{}, @field(map, slot_getter), .{});

    var assignable_slot_offset: usize = 0;

    for (slots) |slot| {
        const parent_marker: []const u8 = if (slot.isParent()) "*" else "";
        const mutability_marker: []const u8 = if (slot.isMutable()) "<-" else "=";
        printWithIndent(display_type, indent, "{s}{s} {s} ", .{ slot.name.asByteArray().getValues(), parent_marker, mutability_marker });

        if (slot.isParent()) {
            // FIXME: Figure out creator slots, and give the path to this object
            std.debug.print("<parent object>", .{});
        } else {
            if (slot.isMutable()) {
                try inspectValueInternal(display_type, object.getAssignableSlots()[assignable_slot_offset], indent, visited_object_link);
            } else {
                try inspectValueInternal(display_type, slot.value, indent, visited_object_link);
            }
        }

        if (slot.isMutable()) assignable_slot_offset += 1;

        std.debug.print(".{s}", .{separator});
    }
}

fn printWithIndent(comptime display_type: InspectDisplayType, indent: usize, comptime fmt: []const u8, args: anytype) void {
    const writer = std.io.getStdErr().writer();
    switch (display_type) {
        .Multiline => writer.writeByteNTimes(' ', indent) catch return,
        .Inline => {},
    }

    std.debug.print(fmt, args);
}
