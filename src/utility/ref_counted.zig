// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only
const std = @import("std");

/// The inline struct that should be placed on structs which will be
/// ref-counted. It must be placed as the `ref` member, and must be initialized
/// with:
///
/// ```
/// object.ref = .{};
/// ```
///
/// in order to be used with RefPtr.
pub const RefCount = struct {
    ref_count: usize = 1,
};

/// A reference-counted pointer type. The value within will be accessed via
/// `ptr.value`. When the value is to be used within a function, it should be
/// referenced before use:
///
/// ```
/// ptr.ref();
/// defer ptr.unref();
/// ```
///
/// if the referenced-value is to be stored, it should be referenced beforehand:
///
/// ```
/// ptr.ref();
/// self.value_ptr = ptr;
/// ```
///
/// and should be unreferenced during destruction with:
///
/// ```
/// self.value_ptr.unref();
/// ```
///
/// Construct using `adopt`.
pub fn RefPtr(comptime T: type) type {
    const typeInfo = @typeInfo(T);
    if (typeInfo != .Struct) {
        @compileError("The type passed to RefPtr must be a struct");
    }

    var did_find_ref = false;
    for (typeInfo.Struct.fields) |field| {
        if (std.mem.eql(u8, field.name, "ref") and field.field_type == RefCount) {
            did_find_ref = true;
            break;
        }
    }

    if (!did_find_ref) {
        @compileError("The type passed to RefPtr must contain a field named `ref` which is of type RefCount");
    }

    var did_find_destroy = false;
    for (typeInfo.Struct.decls) |decl| {
        if (std.mem.eql(u8, decl.name, "destroy") and decl.data == .Fn) {
            if (decl.data.Fn.return_type != void) {
                @compileError("The return type on the `destroy` method of the type passed to RefPtr must be void");
            }

            did_find_destroy = true;
            break;
        }
    }

    if (!did_find_destroy) {
        @compileError("The type passed to RefPtr must contain a method named `destroy`");
    }

    return RefPtrWithoutTypeChecks(T);
}

/// Like RefPtr, but skips the struct checks that prevent cyclic structs.
pub fn RefPtrWithoutTypeChecks(comptime T: type) type {
    return struct {
        value: *T,

        pub const Self = @This();

        /// Adopts the given object as a RefPtr. The object must have been
        /// freshly constructed and the `ref` member must be default-initialized.
        ///
        /// Example for an object returning a ref-counted version of itself:
        ///
        /// ```
        /// pub fn create(allocator: *Allocator) RefPtr(Self) {
        ///     var self = try allocator.create(Self);
        ///     defer allocator.destroy(self);
        ///
        ///     // ... initialize other fields ...
        ///     self.ref = .{};
        ///     return RefPtr(Self).adopt(self);
        /// }
        /// ```
        pub fn adopt(value: *T) Self {
            std.debug.assert(value.ref.ref_count == 1);

            return .{ .value = value };
        }

        /// Increment the reference count on this object.
        pub fn ref(self: Self) void {
            if (self.value.ref.ref_count < 1) {
                @panic("!!! Attempting to reference a destroyed RefPtr");
            }

            self.value.ref.ref_count += 1;
        }

        /// Decrement the reference count on this object.
        ///
        /// If the reference count reaches zero, then the `destroy` method of
        /// the object is called with no arguments.
        pub fn unref(self: Self) void {
            if (self.value.ref.ref_count < 1) {
                @panic("!!! Attempting to unreference an already-destroyed RefPtr");
            }

            self.value.ref.ref_count -= 1;
            if (self.value.ref.ref_count == 0) {
                self.value.destroy();
            }
        }
    };
}