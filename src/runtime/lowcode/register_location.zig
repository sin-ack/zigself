// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

pub const GeneralPurposeRegisterCount = 8;
pub const RegisterLocation = enum {
    zero,
    ret,
    r0,
    r1,
    r2,
    r3,
    r4,
    r5,
    r6,
    r7,

    /// Return the offset of the register for its register set.
    pub fn registerOffset(self: RegisterLocation) u32 {
        return switch (self) {
            .zero => 0,
            .ret => 0,

            .r0 => 0,
            .r1 => 1,
            .r2 => 2,
            .r3 => 3,
            .r4 => 4,
            .r5 => 5,
            .r6 => 6,
            .r7 => 7,
        };
    }

    /// Return this register as an integer.
    pub fn asInt(self: RegisterLocation) u32 {
        return switch (self) {
            .zero => 0,
            .ret => 1,

            .r0,
            .r1,
            .r2,
            .r3,
            .r4,
            .r5,
            .r6,
            .r7,
            => self.registerOffset() + 2,
        };
    }

    pub fn fromInt(value: u32) RegisterLocation {
        return switch (value) {
            0 => .zero,
            1 => .ret,

            2 => .r0,
            3 => .r1,
            4 => .r2,
            5 => .r3,
            6 => .r4,
            7 => .r5,
            8 => .r6,
            9 => .r7,
            else => @panic("!!! Invalid register location"),
        };
    }

    pub fn format(
        loc: RegisterLocation,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        try writer.writeByte('%');
        try std.fmt.formatText(@tagName(loc), "s", options, writer);
    }
};
