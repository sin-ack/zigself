// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("./object.zig").Object;
const Value = @import("./value.zig").Value;
const VirtualMachine = @import("./VirtualMachine.zig");

pub const InspectDisplayType = enum { Inline, Multiline };
const VisitedObjectLink = struct {
    object: Object.Ptr,
    previous: ?*const VisitedObjectLink,
};

pub fn inspectValue(comptime display_type: InspectDisplayType, vm: *VirtualMachine, value: Value) !void {
    return inspectValueInternal(display_type, vm, value, 0, null);
}

fn inspectValueInternal(
    comptime display_type: InspectDisplayType,
    vm: *VirtualMachine,
    value: Value,
    indent: usize,
    visited_object_link: ?*const VisitedObjectLink,
) !void {
    switch (value.type) {
        .Integer => std.debug.print("<integer> {d}", .{value.unsafeAsInteger()}),
        .Object => return inspectObject(display_type, vm, value.unsafeAsObject(), indent, visited_object_link),
    }
}

// FIXME: Move this to object code.
fn inspectObject(
    comptime display_type: InspectDisplayType,
    vm: *VirtualMachine,
    object: Object.Ptr,
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
    if (!is_root_object) {
        if (object.asValue().data == vm.global_nil.data) {
            std.debug.print("<global nil>", .{});
            return;
        }

        if (object.asValue().data == vm.global_true.data) {
            std.debug.print("<global true>", .{});
            return;
        }

        if (object.asValue().data == vm.global_false.data) {
            std.debug.print("<global false>", .{});
            return;
        }
    }

    if (object.getMetadata().reachability == .Global) {
        std.debug.print("(G) ", .{});
    }

    std.debug.print("(#{}) ", .{@intFromEnum(object.getMetadata().actor_id)});

    // FIXME: Move this into object delegation.
    switch (object.getMetadata().type) {
        .Slots => {
            const slots = object.unsafeAsType(.Slots);

            std.debug.print("(|{s}", .{separator});
            try inspectSlots(display_type, vm, slots, indent + 2, separator, &my_link);
            printWithIndent(display_type, indent, "|)", .{});
        },
        .Activation => {
            const activation = object.unsafeAsType(.Activation);

            std.debug.print("<activation object> (|{s}", .{separator});
            try inspectSlots(display_type, vm, activation, indent + 2, separator, &my_link);
            printWithIndent(display_type, indent, "|)", .{});
        },
        .Method => {
            const method = object.unsafeAsType(.Method);

            std.debug.print("<method object \"{s}\"> ", .{method.getMap().method_name.get().getValues()});

            if (method.getSlots().len > 0) {
                std.debug.print("(|{s}", .{separator});
                try inspectSlots(display_type, vm, method, indent + 2, separator, &my_link);
                printWithIndent(display_type, indent, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },
        .Block => {
            const block = object.unsafeAsType(.Block);

            std.debug.print("<block object> ", .{});

            if (block.getSlots().len > 0) {
                std.debug.print("(|{s}", .{separator});
                try inspectSlots(display_type, vm, block, indent + 2, separator, &my_link);
                printWithIndent(display_type, indent, "|)", .{});
            } else {
                std.debug.print("()", .{});
            }
        },
        .ByteArray => {
            const byte_array = object.unsafeAsType(.ByteArray);
            const values = byte_array.getValues();
            std.debug.print("<byte array size: {d}> \"{s}\"", .{ values.len, values });
        },
        .Array => {
            const array = object.unsafeAsType(.Array);
            const values = array.getValues();

            std.debug.print("<array size: {d}> ", .{values.len});
            if (values.len == 0) {
                std.debug.print("[]", .{});
            } else {
                std.debug.print("[{s}", .{separator});

                for (values, 0..) |value, i| {
                    printWithIndent(display_type, indent + 2, "", .{});
                    try inspectValueInternal(display_type, vm, value, indent + 2, &my_link);

                    if (i < values.len - 1) std.debug.print(",", .{});
                    std.debug.print(separator, .{});
                }

                printWithIndent(display_type, indent, "]", .{});
            }
        },
        .Managed => {
            const managed = object.unsafeAsType(.Managed);
            std.debug.print("<managed object: {}> ", .{managed.getManagedType()});
            try inspectValueInternal(display_type, vm, managed.value, indent, &my_link);
        },
        .Actor => {
            // TODO: Add more detailed inspection
            std.debug.print("<actor>", .{});
        },
        .ActorProxy => {
            // TODO: Add more detailed inspection
            std.debug.print("<actor proxy>", .{});
        },
        .Float => {
            std.debug.print("<float> {}", .{object.unsafeAsType(.Float).get()});
        },
        .AddrInfo => {
            const addrinfo_map = object.unsafeAsType(.AddrInfo).getMap();

            std.debug.print("<addrinfo> (|{s}", .{separator});

            printWithIndent(display_type, indent + 2, "family = ", .{});
            try inspectValueInternal(display_type, vm, addrinfo_map.family, indent + 2, &my_link);
            std.debug.print(".{s}", .{separator});
            printWithIndent(display_type, indent + 2, "socketType = ", .{});
            try inspectValueInternal(display_type, vm, addrinfo_map.socketType, indent + 2, &my_link);
            std.debug.print(".{s}", .{separator});
            printWithIndent(display_type, indent + 2, "protocol = ", .{});
            try inspectValueInternal(display_type, vm, addrinfo_map.protocol, indent + 2, &my_link);
            std.debug.print(".{s}", .{separator});
            printWithIndent(display_type, indent + 2, "flags = ", .{});
            try inspectValueInternal(display_type, vm, addrinfo_map.flags, indent + 2, &my_link);
            std.debug.print(".{s}", .{separator});
            printWithIndent(display_type, indent + 2, "sockaddrBytes = ", .{});
            try inspectValueInternal(display_type, vm, addrinfo_map.sockaddrBytes, indent + 2, &my_link);
            std.debug.print(".{s}", .{separator});

            printWithIndent(display_type, indent, "|)", .{});
        },
    }
}

fn inspectSlots(
    comptime display_type: InspectDisplayType,
    vm: *VirtualMachine,
    object: anytype,
    indent: usize,
    separator: []const u8,
    visited_object_link: *const VisitedObjectLink,
) !void {
    const slots = object.getSlots();

    for (slots) |slot| {
        const parent_marker: []const u8 = if (slot.isParent()) "*" else "";
        const assignability_marker: []const u8 = if (slot.isAssignable()) "<-" else "=";
        printWithIndent(display_type, indent, "{s}{s} {s} ", .{ slot.name.get().getValues(), parent_marker, assignability_marker });

        if (slot.isParent()) {
            // FIXME: Figure out creator slots, and give the path to this object
            std.debug.print("<parent object>", .{});
        } else {
            if (slot.isArgument()) {
                std.debug.print("<argument>", .{});
            } else if (slot.isAssignable()) {
                try inspectValueInternal(display_type, vm, object.getAssignableSlotValue(slot).*, indent, visited_object_link);
            } else {
                try inspectValueInternal(display_type, vm, slot.value, indent, visited_object_link);
            }
        }

        std.debug.print(".{s}", .{separator});
    }
}

fn printWithIndent(comptime display_type: InspectDisplayType, indent: usize, comptime fmt: []const u8, args: anytype) void {
    // FIXME: Take a writer instead of hardcoding stderr
    // FIXME: Propagate errors
    var writer_buffer: [4096]u8 = undefined;
    var file_writer = std.fs.File.stderr().writer(&writer_buffer);
    const writer = &file_writer.interface;

    switch (display_type) {
        .Multiline => writer.splatByteAll(' ', indent) catch return,
        .Inline => {},
    }

    writer.print(fmt, args) catch return;
}
