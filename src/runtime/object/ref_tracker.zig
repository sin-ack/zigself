// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");

// FIXME: Make this a build config option
const EnableObjectRefTracker = false;

const ObjectMap = std.AutoArrayHashMap(*Object, void);
var object_ref_tracker: ?ObjectMap = null;

pub fn setupObjectRefTracker(allocator: *Allocator) void {
    if (!EnableObjectRefTracker) return;

    object_ref_tracker = ObjectMap.init(allocator);
}

pub fn teardownObjectRefTrackerAndReportAliveRefs() void {
    if (!EnableObjectRefTracker) return;

    if (object_ref_tracker.?.keys().len > 0) {
        std.debug.print("Remaining object refs during teardown:\n", .{});

        var iterator = object_ref_tracker.?.iterator();
        while (iterator.next()) |item| {
            const object = item.key_ptr.*;
            std.debug.print("  <*{d}> (type {s}) - {d} refs\n", .{ object.id, @tagName(object.content), object.ref.ref_count });
        }
    }

    object_ref_tracker.?.deinit();
}

pub fn addObjectToRefTracker(object: *Object) !void {
    if (!EnableObjectRefTracker) return;
    try object_ref_tracker.?.put(object, .{});
}

pub fn removeObjectFromRefTracker(object: *Object) void {
    if (!EnableObjectRefTracker) return;
    object_ref_tracker.?.swapRemove(object);
}
