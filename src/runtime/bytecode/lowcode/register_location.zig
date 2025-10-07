// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

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

    pub const GeneralPurposeRegisterCount = 8;
    pub const BitSet = std.StaticBitSet(GeneralPurposeRegisterCount);

    // "%r0"
    pub const ZeroLabel: []const u8 = "   ";

    pub fn isFinite() bool {
        return true;
    }

    pub fn isNothing(self: RegisterLocation) bool {
        return self == .zero;
    }

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

    pub fn format(loc: RegisterLocation, writer: *std.Io.Writer) !void {
        try writer.writeByte('%');
        try writer.writeAll(@tagName(loc));
    }
};
