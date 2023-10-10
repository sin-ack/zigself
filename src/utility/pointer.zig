// Copyright (c) 2022-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! Heap pointer utilities.

const builtin = @import("builtin");

const IsConst = enum { Mutable, Const };

/// Return a heap-aligned pointer.
pub fn HeapPtr(comptime T: type, comptime is_const: IsConst) type {
    return switch (is_const) {
        .Const => *align(@alignOf(u64)) const T,
        .Mutable => *align(@alignOf(u64)) T,
    };
}

/// Return a heap-aligned slice.
///
/// Similar situation to HeapPtr above.
pub fn HeapSlice(comptime T: type, comptime is_const: IsConst) type {
    return switch (is_const) {
        .Const => []align(@alignOf(u64)) const T,
        .Mutable => []align(@alignOf(u64)) T,
    };
}
