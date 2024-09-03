// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Value = @import("../../value.zig").Value;
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

/// General purpose registers which can be allocated.
general_purpose: [RegisterLocation.GeneralPurposeRegisterCount]Value = undefined,
/// The return register.
ret: Value = undefined,

const RegisterFile = @This();

pub fn init(self: *RegisterFile) void {
    for (&self.general_purpose) |*register| {
        register.* = Value.fromUnsignedInteger(0);
    }

    self.ret = Value.fromUnsignedInteger(0);
}

pub fn read(self: RegisterFile, location: RegisterLocation) Value {
    return switch (location) {
        // This should be handled in VirtualMachine.readRegister.
        .zero => @panic("!!! Attempting to read zero register in RegisterFile"),
        .ret => self.ret,

        .r0,
        .r1,
        .r2,
        .r3,
        .r4,
        .r5,
        .r6,
        .r7,
        => self.general_purpose[location.registerOffset()],
    };
}

pub fn write(self: *RegisterFile, location: RegisterLocation, value: Value) void {
    switch (location) {
        .zero => {},
        .ret => self.ret = value,

        .r0,
        .r1,
        .r2,
        .r3,
        .r4,
        .r5,
        .r6,
        .r7,
        => self.general_purpose[location.registerOffset()] = value,
    }
}

pub fn visitValues(
    self: *RegisterFile,
    // TODO: Write interfaces proposal for Zig
    visitor: anytype,
) !void {
    for (&self.general_purpose) |*register| {
        try visitor.visit(register);
    }

    try visitor.visit(&self.ret);
}
