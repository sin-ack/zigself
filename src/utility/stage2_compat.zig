// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! Compatibility helpers between stage1 and stage2.

const builtin = @import("builtin");

const IsConst = enum { Mutable, Const };
/// Return a heap-aligned pointer.
///
/// In stage1, the default alignment of 0 accepts any alignment (and some
/// operations like indexing actually incorrectly drops the alignment), so we
/// need to return pointers with 0 alignment. stage2 has fixed this behavior and
/// instead ensures that all pointers are correctly aligned on use, so we need
/// to return pointers with our heap's alignment (u64 aligned).
pub fn HeapPtr(comptime T: type, comptime is_const: IsConst) type {
    if (is_const == .Const) {
        return if (builtin.zig_backend == .stage1)
            *const T
        else
            *align(@alignOf(u64)) const T;
    }

    return if (builtin.zig_backend == .stage1)
        *T
    else
        *align(@alignOf(u64)) T;
}

/// Return a heap-aligned slice.
///
/// Similar situation to HeapPtr above.
pub fn HeapSlice(comptime T: type, comptime is_const: IsConst) type {
    if (is_const == .Const) {
        return if (builtin.zig_backend == .stage1)
            []const T
        else
            []align(@alignOf(u64)) const T;
    }

    return if (builtin.zig_backend == .stage1)
        []T
    else
        []align(@alignOf(u64)) T;
}

/// Return a function pointer.
///
/// stage1 uses bare fn types to indicate a function, while stage2 requires a
/// *const fn.
pub fn FnPtr(comptime T: type) type {
    return if (builtin.zig_backend == .stage1)
        T
    else
        *const T;
}

/// Take a pointer to a function in a compatible way.
pub fn fnPtr(function: anytype) FnPtr(@TypeOf(function)) {
    return if (builtin.zig_backend == .stage1)
        function
    else
        &function;
}
