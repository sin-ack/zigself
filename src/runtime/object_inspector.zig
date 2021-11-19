// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("./object.zig");
const environment = @import("./environment.zig");

const VisitedObjectSet = std.AutoArrayHashMap(*Object, void);

fn printWithIndent(indent: usize, comptime fmt: []const u8, args: anytype) void {
    const writer = std.io.getStdErr().writer();
    writer.writeByteNTimes(' ', indent) catch return;
    std.debug.print(fmt, args);
}

pub const InspectDisplayType = enum { Inline, Multiline };
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
    if (!is_first_object_to_be_printed) {
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
            for (slots.slots) |slot| {
                if (std.mem.eql(u8, slot.name, "_parent")) {
                    std.debug.print("<activation object> ", .{});
                }
            }

            std.debug.print("(|{s}", .{separator});

            for (slots.slots) |slot| {
                const parent_marker: []const u8 = if (slot.is_parent) "*" else "";
                const mutability_marker: []const u8 = if (slot.is_mutable) "<-" else "=";
                printWithIndent(indent + 2, "{s}{s} {s} ", .{ slot.name, parent_marker, mutability_marker });

                try inspectObjectInternal(slot.value, display_type, indent + 2, visited_object_set);

                std.debug.print(".{s}", .{separator});
            }

            printWithIndent(indent, "|)", .{});
        },

        .Activation => |activation| {
            const activation_type: []const u8 = switch (activation.context) {
                .Method => "method",
                .Block => "block",
            };

            std.debug.print("<{s} activation> ", .{activation_type});
            try inspectObjectInternal(activation.activation_object, display_type, indent, visited_object_set);
        },

        .Method => |method| {
            std.debug.print("<method object [", .{});
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
                    printWithIndent(indent + 2, "{s}{s} {s} ", .{ slot.name, parent_marker, mutability_marker });

                    try inspectObjectInternal(slot.value, display_type, indent + 2, visited_object_set);

                    std.debug.print(".{s}", .{separator});
                }
                printWithIndent(indent, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },

        .Block => |block| {
            std.debug.print("<block object for {*} [", .{block.bound_method.value});
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
                    printWithIndent(indent + 2, "{s}{s} {s} ", .{ slot.name, parent_marker, mutability_marker });

                    try inspectObjectInternal(slot.value, display_type, indent + 2, visited_object_set);

                    std.debug.print(".{s}", .{separator});
                }
                printWithIndent(indent, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },

        .NonlocalReturn => |nonlocal_return| {
            std.debug.print("<non-local return for {*}> ", .{nonlocal_return.target_method.value});
            try inspectObjectInternal(nonlocal_return.value, display_type, indent, visited_object_set);
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
