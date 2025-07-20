// Copyright (c) 2021-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! This struct is an analogue to Range, but contains full Location information.

const std = @import("std");

const Location = @import("./Location.zig");

const LocationRange = @This();

start: Location,
end: Location,

pub fn format(range: LocationRange, writer: *std.io.Writer) !void {
    try range.start.format(writer);
    try writer.writeByte('-');

    if (range.start.line != range.end.line) {
        try range.end.format(writer);
    } else {
        try writer.printInt(range.end.column, 10, .lower, .{});
    }
}
