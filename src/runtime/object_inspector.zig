// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Slot = @import("./slot.zig");
const Object = @import("./object.zig");
const environment = @import("./environment.zig");

const VisitedObjectSet = std.AutoArrayHashMap(*Object, void);
pub const InspectDisplayType = enum { Inline, Multiline };

fn printWithIndent(indent: usize, comptime display_type: InspectDisplayType, comptime fmt: []const u8, args: anytype) void {
    const writer = std.io.getStdErr().writer();
    switch (display_type) {
        .Multiline => writer.writeByteNTimes(' ', indent) catch return,
        .Inline => {},
    }

    std.debug.print(fmt, args);
}

pub fn inspectObject(allocator: *Allocator, object: Object.Ref, comptime display_type: InspectDisplayType) !void {
    var visited_object_set = VisitedObjectSet.init(allocator);
    defer visited_object_set.deinit();

    try inspectObjectInternal(object, display_type, 0, &visited_object_set);
    std.debug.print("\n", .{});
}

fn inspectObjectInternal(object: Object.Ref, comptime display_type: InspectDisplayType, indent: usize, visited_object_set: *VisitedObjectSet) Allocator.Error!void {
    const separator = switch (display_type) {
        .Inline => " ",
        .Multiline => "\n",
    };

    if (visited_object_set.contains(object.value)) {
        std.debug.print("<cyclic reference>", .{});
        return;
    }

    const is_first_object_to_be_printed = visited_object_set.keys().len == 0;
    try visited_object_set.put(object.value, .{});
    defer _ = visited_object_set.swapRemove(object.value);

    // If the global objects are printed during inspect and they haven't been
    // printed directly, then print them as a summary.
    if (!is_first_object_to_be_printed and !environment.hasBeenTornDown()) {
        const nil_object = environment.globalNil();
        defer nil_object.unref();
        if (object.value == nil_object.value) {
            std.debug.print("<global nil>", .{});
            return;
        }

        const true_object = environment.globalTrue();
        defer true_object.unref();
        if (object.value == true_object.value) {
            std.debug.print("<global true>", .{});
            return;
        }

        const false_object = environment.globalFalse();
        defer false_object.unref();
        if (object.value == false_object.value) {
            std.debug.print("<global false>", .{});
            return;
        }
    }

    switch (object.value.content) {
        .Empty => {
            std.debug.print("()", .{});
        },

        .Slots => |slots| {
            std.debug.print("(|{s}", .{separator});

            try inspectSlots(slots.slots, display_type, indent + 2, separator, visited_object_set);

            printWithIndent(indent, display_type, "|)", .{});
        },

        // FIXME: avoid repeating code
        .Activation => |activation| {
            std.debug.print("<activation object> (|{s}", .{separator});

            try inspectSlots(activation.slots, display_type, indent + 2, separator, visited_object_set);

            printWithIndent(indent, display_type, "|)", .{});
        },

        .Method => |method| {
            std.debug.print("<method object \"{s}\" [", .{method.message_name});
            for (method.arguments) |argument, i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("\"{s}\"", .{argument});
            }
            std.debug.print("]> ", .{});

            if (method.slots.len > 0) {
                std.debug.print("(|{s}", .{separator});
                for (method.slots) |slot| {
                    const parent_marker: []const u8 = if (slot.is_parent) "*" else "";
                    const mutability_marker: []const u8 = if (slot.is_mutable) "<-" else "=";
                    printWithIndent(indent + 2, display_type, "{s}{s} {s} ", .{ slot.name, parent_marker, mutability_marker });

                    try inspectObjectInternal(slot.value, display_type, indent + 2, visited_object_set);

                    std.debug.print(".{s}", .{separator});
                }
                printWithIndent(indent, display_type, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },

        .Block => |block| {
            std.debug.print("<block object for ", .{});
            if (block.nonlocal_return_target_activation.getPointer()) |target_activation| {
                std.debug.print("<*{d}>", .{target_activation.activation_object.value.id});
            } else {
                std.debug.print("<gone>", .{});
            }

            std.debug.print(" [", .{});
            for (block.arguments) |argument, i| {
                if (i > 0) std.debug.print(", ", .{});
                std.debug.print("\"{s}\"", .{argument});
            }
            std.debug.print("]> ", .{});

            if (block.slots.len > 0) {
                std.debug.print("(|{s}", .{separator});
                for (block.slots) |slot| {
                    const parent_marker: []const u8 = if (slot.is_parent) "*" else "";
                    const mutability_marker: []const u8 = if (slot.is_mutable) "<-" else "=";
                    printWithIndent(indent + 2, display_type, "{s}{s} {s} ", .{ slot.name, parent_marker, mutability_marker });

                    try inspectObjectInternal(slot.value, display_type, indent + 2, visited_object_set);

                    std.debug.print(".{s}", .{separator});
                }
                printWithIndent(indent, display_type, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },

        .ByteVector => |vector| {
            std.debug.print("<byte vector size: {d}> \"{s}\"", .{ vector.values.len, vector.values });
        },

        .Integer => |integer| {
            std.debug.print("<integer> {d}", .{integer.value});
        },

        .FloatingPoint => |floating_point| {
            std.debug.print("<floating point> {d}", .{floating_point.value});
        },
    }
}

fn inspectSlots(slots: []Slot, comptime display_type: InspectDisplayType, indent: usize, separator: []const u8, visited_object_set: *VisitedObjectSet) !void {
    for (slots) |slot| {
        const parent_marker: []const u8 = if (slot.is_parent) "*" else "";
        const mutability_marker: []const u8 = if (slot.is_mutable) "<-" else "=";
        printWithIndent(indent, display_type, "{s}{s} {s} ", .{ slot.name, parent_marker, mutability_marker });

        if (slot.is_parent) {
            std.debug.print("<*{d}>", .{slot.value.value.id});
        } else {
            try inspectObjectInternal(slot.value, display_type, indent, visited_object_set);
        }

        std.debug.print(".{s}", .{separator});
    }
}
