// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

/// Returns whether the given error is contained within the given error set.
pub fn errSetContains(comptime ErrorSet: type, err: anyerror) bool {
    inline for (comptime std.meta.fields(ErrorSet)) |field| {
        if (@field(ErrorSet, field.name) == err) {
            return true;
        }
    }

    return false;
}
