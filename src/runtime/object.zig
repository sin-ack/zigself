// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const ref_counted = @import("../utility/ref_counted.zig");
const Slot = @import("./slot.zig");

const Self = @This();
pub const Ref = ref_counted.RefPtrWithoutTypeChecks(Self);

const ObjectContent = union(enum) {
    Empty: void,

    Slots: struct {
        slots: []Slot,
    },

    Method: struct {
        arguments: [][]const u8,
        slots: []Slot,
        statements: []AST.StatementNode,
    },

    ByteVector: struct {
        parent: Ref,
        values: []const u8,
    },

    Integer: struct {
        parent: Ref,
        value: u64,
    },

    FloatingPoint: struct {
        parent: Ref,
        value: f64,
    },
};

ref: ref_counted.RefCount,
allocator: *Allocator,
content: ObjectContent,

pub fn createEmpty(allocator: *Allocator) !Ref {
    return try create(allocator, .{ .Empty = .{} });
}

pub fn createSlots(allocator: *Allocator, slots: []Slot) !Ref {
    return try create(allocator, .{ .Slots = .{ .slots = slots } });
}

/// Takes ownership of `arguments`, `slots` and `statements`.
pub fn createMethod(allocator: *Allocator, arguments: [][]const u8, slots: []Slot, statements: []AST.StatementNode) !Ref {
    return try create(allocator, .{ .Method = .{ .arguments = arguments, .slots = slots, .statements = statements } });
}

pub fn createCopyFromStringLiteral(allocator: *Allocator, contents: []const u8, parent: Ref) !Ref {
    parent.ref();
    errdefer parent.unref();

    const contents_copy = try allocator.dupe(u8, contents);
    errdefer allocator.free(contents_copy);

    return try create(allocator, .{ .ByteVector = .{ .parent = parent, .values = contents_copy } });
}

pub fn createFromIntegerLiteral(allocator: *Allocator, value: u64, parent: Ref) !Ref {
    parent.ref();
    errdefer parent.unref();

    return try create(allocator, .{ .Integer = .{ .parent = parent, .value = value } });
}

pub fn createFromFloatingPointLiteral(allocator: *Allocator, value: f64, parent: Ref) !Ref {
    parent.ref();
    errdefer parent.unref();

    return try create(allocator, .{ .FloatingPoint = .{ .parent = parent, .value = value } });
}

pub fn destroy(self: *Self) void {
    self.deinit();
    self.allocator.destroy(self);
}

fn create(allocator: *Allocator, content: ObjectContent) !Ref {
    const self = try allocator.create(Self);
    self.init(allocator, content);
    return Ref.adopt(self);
}

fn init(self: *Self, allocator: *Allocator, content: ObjectContent) void {
    self.ref = .{};

    self.allocator = allocator;
    self.content = content;
}

fn deinit(self: *Self) void {
    switch (self.content) {
        .Empty => {},
        .Slots => {
            for (self.content.Slots.slots) |*slot| {
                slot.deinit();
            }
            self.allocator.free(self.content.Slots.slots);
        },
        .Method => {
            for (self.content.Method.arguments) |argument| {
                self.allocator.free(argument);
            }
            self.allocator.free(self.content.Method.arguments);

            for (self.content.Method.slots) |*slot| {
                slot.deinit();
            }
            self.allocator.free(self.content.Method.slots);

            for (self.content.Method.statements) |*statement| {
                statement.deinit(self.allocator);
            }
            self.allocator.free(self.content.Method.statements);
        },
        .ByteVector => {
            self.content.ByteVector.parent.unref();
            self.allocator.free(self.content.ByteVector.values);
        },
        .Integer => {
            self.content.Integer.parent.unref();
        },
        .FloatingPoint => {
            self.content.FloatingPoint.parent.unref();
        },
    }
}

const VisitedObjectsSet = std.AutoArrayHashMap(*Self, void);

pub fn lookup(self: *Self, selector: []const u8) !?Ref {
    if (std.mem.eql(u8, selector, "self")) {
        return Ref.adopt(self);
    }

    var visited_objects = VisitedObjectsSet.init(self.allocator);
    defer visited_objects.deinit();

    return try self.lookup_internal(selector, &visited_objects);
}

// Self Handbook, §3.3.8 The lookup algorithm
fn lookup_internal(self: *Self, selector: []const u8, visited_objects: *VisitedObjectsSet) Allocator.Error!?Ref {
    if (visited_objects.contains(self)) {
        return null;
    }

    try visited_objects.put(self, .{});

    switch (self.content) {
        .Empty => return null,

        .Slots => |slots| {
            // Direct lookup
            for (slots.slots) |slot| {
                if (std.mem.eql(u8, selector, slot.name)) {
                    return slot.value;
                }
            }

            // Parent lookup
            for (slots.slots) |slot| {
                if (slot.is_parent) {
                    if (try slot.value.value.lookup_internal(selector, visited_objects)) |found_object| {
                        return found_object;
                    }
                }
            }

            // Nope, not here
            return null;
        },

        .Method => @panic("Attempting to perform lookup on method?!"),

        // The 3 types below have an imaginary field called "parent" which contains
        // the parent object ref.
        .ByteVector => |vector| {
            if (std.mem.eql(u8, selector, "parent")) {
                return vector.parent;
            }

            return try vector.parent.value.lookup_internal(selector, visited_objects);
        },

        .Integer => |integer| {
            if (std.mem.eql(u8, selector, "parent")) {
                return integer.parent;
            }

            return try integer.parent.value.lookup_internal(selector, visited_objects);
        },

        .FloatingPoint => |floating_point| {
            if (std.mem.eql(u8, selector, "parent")) {
                return floating_point.parent;
            }

            return try floating_point.parent.value.lookup_internal(selector, visited_objects);
        },
    }
}
