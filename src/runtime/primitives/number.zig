// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Object = @import("../object.zig");

fn getNumberTraits(lobby: Object.Ref) !Object.Ref {
    if (try lobby.value.lookup("traits")) |traits| {
        if (try traits.value.lookup("number")) |traits_number| {
            return traits_number;
        } else {
            @panic("Could not find number in traits");
        }
    } else {
        @panic("Could not find traits in lobby");
    }
}

fn makeInteger(allocator: *Allocator, value: u64, lobby: Object.Ref) !Object.Ref {
    var number_traits = try getNumberTraits(lobby);
    number_traits.ref();
    return try Object.createFromIntegerLiteral(allocator, value, number_traits);
}

/// Add two integer numbers. The returned value is an integer.
pub fn IntAdd(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, lobby: Object.Ref) !Object.Ref {
    if (!receiver.value.is(.Integer)) {
        std.debug.panic("Expected Integer as _IntAdd: receiver, got {s}", .{@tagName(receiver.value.content)});
    }

    var addend = arguments[0];
    if (!addend.value.is(.Integer)) {
        std.debug.panic("Expected Integer as _IntAdd: argument, got {s}", .{@tagName(addend.value.content)});
    }

    const result = try makeInteger(
        allocator,
        receiver.value.content.Integer.value + addend.value.content.Integer.value,
        lobby,
    );

    receiver.unref();
    addend.unref();

    return result;
}
