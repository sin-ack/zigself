// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const Slot = @import("./slot.zig");
const Script = @import("../language/script.zig");
const weak_ref = @import("../utility/weak_ref.zig");
const Activation = @import("./activation.zig");
const ref_counted = @import("../utility/ref_counted.zig");
const runtime_error = @import("./error.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");

pub usingnamespace @import("./object/lookup.zig");
pub usingnamespace @import("./object/activation.zig");
pub usingnamespace @import("./object/ref_tracker.zig");

ref: ref_counted.RefCount,
weak: WeakBlock,
allocator: *Allocator,
content: ObjectContent,
id: usize,

const Self = @This();
const WeakBlock = weak_ref.WeakPtrBlock(Self);
const Weak = weak_ref.WeakPtr(Self);
pub const Ref = ref_counted.RefPtr(Self);

var current_id: usize = 0;

const ObjectContent = union(enum) {
    Empty: void,

    Slots: struct {
        slots: []Slot,
    },

    Activation: struct {
        slots: []Slot,
        receiver: Ref,
    },

    Method: struct {
        // Debug context
        /// The script in which this method was defined.
        script: Script.Ref,
        /// Used for stack traces.
        message_name: []const u8,

        arguments: [][]const u8,
        slots: []Slot,
        statements: []AST.StatementNode,
    },

    Block: struct {
        /// Arguments received for the method activation.
        arguments: [][]const u8,
        /// The slots that are present on the method object.
        slots: []Slot,
        /// Statements to be executed once the block activates.
        statements: []AST.StatementNode,

        /// The activation for the method that this block was created in and
        /// belongs to.
        parent_activation: Activation.Weak,
        /// The activation to which any non-local returns should bubble up to.
        /// In other words, it is the method in which this block was eventually
        /// instantiated.
        nonlocal_return_target_activation: Activation.Weak,

        // Debug context
        /// The script in which this block was defined.
        script: Script.Ref,
    },

    ByteVector: struct {
        values: []u8,
    },

    Vector: struct {
        values: []Ref,
    },

    Integer: struct {
        value: i64,
    },

    FloatingPoint: struct {
        value: f64,
    },
};

pub fn createEmpty(allocator: *Allocator) !Ref {
    return try create(allocator, .{ .Empty = .{} });
}

/// Takes ownership of `slots`.
pub fn createSlots(allocator: *Allocator, slots: []Slot) !Ref {
    return try create(allocator, .{ .Slots = .{ .slots = slots } });
}

/// Takes ownership of `arguments`, `slots` and `statements`.
/// `message_name` is duped.
/// Borrows a ref for `script` from the caller.
pub fn createMethod(allocator: *Allocator, message_name: []const u8, arguments: [][]const u8, slots: []Slot, statements: []AST.StatementNode, script: Script.Ref) !Ref {
    var message_name_copy = try allocator.dupe(u8, message_name);
    errdefer allocator.free(message_name_copy);

    return try create(allocator, .{
        .Method = .{
            .message_name = message_name_copy,
            .arguments = arguments,
            .slots = slots,
            .statements = statements,
            .script = script,
        },
    });
}

/// Dupes `contents`.
pub fn createCopyFromStringLiteral(allocator: *Allocator, contents: []const u8) !Ref {
    const contents_copy = try allocator.dupe(u8, contents);
    errdefer allocator.free(contents_copy);

    return try create(allocator, .{ .ByteVector = .{ .values = contents_copy } });
}

pub fn createFromIntegerLiteral(allocator: *Allocator, value: i64) !Ref {
    return try create(allocator, .{ .Integer = .{ .value = value } });
}

pub fn createFromFloatingPointLiteral(allocator: *Allocator, value: f64) !Ref {
    return try create(allocator, .{ .FloatingPoint = .{ .value = value } });
}

pub fn createEmptyVector(allocator: *Allocator) !Ref {
    var values = try allocator.alloc(Ref, 0);
    errdefer allocator.free(values);

    return try create(allocator, .{ .Vector = .{ .values = values } });
}

/// Takes ownership of `values`. Borrows one ref for each object in `values`.
pub fn createVectorFromValues(allocator: *Allocator, values: []Ref) !Ref {
    return try create(allocator, .{ .Vector = .{ .values = values } });
}

/// Takes ownership of `arguments`, `slots` and `statements`.
/// Borrows a ref for `script` from the caller.
/// `parent_activation` and `nonlocal_return_target_activation` are weakly
/// ref'd.
pub fn createBlock(
    allocator: *Allocator,
    arguments: [][]const u8,
    slots: []Slot,
    statements: []AST.StatementNode,
    parent_activation: *Activation,
    nonlocal_return_target_activation: *Activation,
    script: Script.Ref,
) !Ref {
    var parent_weak = parent_activation.makeWeakRef();
    errdefer parent_weak.deinit();

    var target_weak = nonlocal_return_target_activation.makeWeakRef();
    errdefer target_weak.deinit();

    return try create(allocator, .{
        .Block = .{
            .arguments = arguments,
            .slots = slots,
            .statements = statements,
            .parent_activation = parent_weak,
            .nonlocal_return_target_activation = target_weak,
            .script = script,
        },
    });
}

// NOTE: Made pub for object/activation.zig; do NOT use directly.
pub fn create(allocator: *Allocator, content: ObjectContent) !Ref {
    const self = try allocator.create(Self);
    try self.init(allocator, content);

    try self.addObjectToRefTracker();
    return Ref.adopt(self);
}

pub fn destroy(self: *Self) void {
    self.destroyOptions(false);
}

/// If `avoid_object_unref` is true, avoids calling `unref` on object
/// references. This is used by the object reference tracker to clean up all
/// object references in one go, so that the GeneralPurposeAllocator leak
/// detector shows only actual leaks and not those caused by reference cycles.
pub fn destroyOptions(self: *Self, comptime avoid_object_unref: bool) void {
    self.deinitContent(avoid_object_unref);
    self.weak.deinit();

    self.removeObjectFromRefTracker();
    self.allocator.destroy(self);
}

fn init(self: *Self, allocator: *Allocator, content: ObjectContent) !void {
    self.id = current_id;
    current_id += 1;
    self.ref = .{};
    self.weak = try WeakBlock.init(allocator, self);

    self.allocator = allocator;
    self.content = content;
}

fn deinitContent(self: *Self, comptime avoid_object_unref: bool) void {
    switch (self.content) {
        .Empty => {},
        .Slots => |slots| {
            for (slots.slots) |*slot| {
                slot.deinitOptions(avoid_object_unref);
            }
            self.allocator.free(slots.slots);
        },
        .Activation => |activation| {
            for (activation.slots) |*slot| {
                slot.deinitOptions(avoid_object_unref);
            }
            self.allocator.free(activation.slots);
            activation.receiver.unref();
        },
        .Method => |method| {
            self.allocator.free(method.message_name);
            method.script.unref();

            for (method.arguments) |argument| {
                self.allocator.free(argument);
            }
            self.allocator.free(method.arguments);

            for (method.slots) |*slot| {
                slot.deinitOptions(avoid_object_unref);
            }
            self.allocator.free(method.slots);

            for (method.statements) |*statement| {
                statement.deinit(self.allocator);
            }
            self.allocator.free(method.statements);
        },
        .Block => |block| {
            block.script.unref();

            for (block.arguments) |argument| {
                self.allocator.free(argument);
            }
            self.allocator.free(block.arguments);

            for (block.slots) |*slot| {
                slot.deinitOptions(avoid_object_unref);
            }
            self.allocator.free(block.slots);

            for (block.statements) |*statement| {
                statement.deinit(self.allocator);
            }
            self.allocator.free(block.statements);

            block.parent_activation.deinit();
            block.nonlocal_return_target_activation.deinit();
        },
        .ByteVector => |bytevector| {
            self.allocator.free(bytevector.values);
        },
        .Vector => |vector| {
            if (!avoid_object_unref) for (vector.values) |v| v.unref();
            self.allocator.free(vector.values);
        },
        .Integer, .FloatingPoint => {},
    }
}

pub fn is(self: Self, tag: std.meta.Tag(ObjectContent)) bool {
    return self.content == tag;
}

pub fn copy(self: Self) !Ref {
    switch (self.content) {
        .Empty => return createEmpty(self.allocator),

        .Slots => |slots| {
            var slots_copy = try std.ArrayList(Slot).initCapacity(self.allocator, slots.slots.len);
            errdefer {
                for (slots_copy.items) |*slot| {
                    slot.deinit();
                }
                slots_copy.deinit();
            }

            for (slots.slots) |slot| {
                var slot_copy = try slot.copy();
                errdefer slot_copy.deinit();

                try slots_copy.append(slot_copy);
            }

            return try createSlots(self.allocator, slots_copy.toOwnedSlice());
        },

        // We should _never_ want to actually copy an activation object.
        .Activation => unreachable,

        .Block => |block| {
            var arguments_copy = try std.ArrayList([]const u8).initCapacity(self.allocator, block.arguments.len);
            errdefer {
                for (arguments_copy.items) |argument| {
                    self.allocator.free(argument);
                }
                arguments_copy.deinit();
            }

            var slots_copy = try std.ArrayList(Slot).initCapacity(self.allocator, block.slots.len);
            errdefer {
                for (slots_copy.items) |*slot| {
                    slot.deinit();
                }
                slots_copy.deinit();
            }

            var statements_copy = try std.ArrayList(AST.StatementNode).initCapacity(self.allocator, block.statements.len);
            errdefer {
                for (statements_copy.items) |*statement| {
                    statement.deinit(self.allocator);
                }
                statements_copy.deinit();
            }

            for (block.arguments) |argument| {
                var argument_copy = try self.allocator.dupe(u8, argument);
                errdefer self.allocator.free(argument_copy);

                try arguments_copy.append(argument_copy);
            }

            for (block.slots) |slot| {
                var slot_copy = try slot.copy();
                errdefer slot_copy.deinit();

                try slots_copy.append(slot_copy);
            }

            for (block.statements) |statement| {
                var statement_copy = try ASTCopyVisitor.visitStatement(statement, self.allocator);
                errdefer statement_copy.deinit(self.allocator);

                try statements_copy.append(statement_copy);
            }

            block.script.ref();
            errdefer block.script.unref();

            // FIXME: The activations might be gone at this point, handle that
            //        case.
            return try createBlock(
                self.allocator,
                arguments_copy.toOwnedSlice(),
                slots_copy.toOwnedSlice(),
                statements_copy.toOwnedSlice(),
                block.parent_activation.getPointer().?,
                block.nonlocal_return_target_activation.getPointer().?,
                block.script,
            );
        },

        .ByteVector => |vector| {
            return try createCopyFromStringLiteral(self.allocator, vector.values);
        },

        .Integer => |integer| {
            return try createFromIntegerLiteral(self.allocator, integer.value);
        },

        .FloatingPoint => |floating_point| {
            return try createFromFloatingPointLiteral(self.allocator, floating_point.value);
        },

        .Vector => |vector| {
            var values_copy = try self.allocator.alloc(Ref, vector.values.len);
            errdefer self.allocator.free(values_copy);

            for (values_copy) |*v, i| {
                const value = vector.values[i];
                value.ref();
                v.* = value;
            }
            errdefer for (values_copy) |v| v.unref();

            return try create(self.allocator, .{ .Vector = .{ .values = values_copy } });
        },

        .Method => unreachable,
    }
}

/// Attempt to add the given slot objects to the content. Only Empty and Slots
/// objects allow this; otherwise, error.ObjectDoesNotAcceptSlots is returned.
/// The slots' ownership is passed to the object.
pub fn addSlots(self: *Self, new_slots: []Slot) !void {
    errdefer {
        for (new_slots) |*slot| {
            slot.deinit();
        }
    }

    switch (self.content) {
        .Empty, .Slots => {},
        else => return error.ObjectDoesNotAcceptSlots,
    }

    var slot_list = blk: {
        switch (self.content) {
            .Empty => {
                self.deinitContent(false);

                var new_slot_list = try self.allocator.alloc(Slot, new_slots.len);
                self.content = .{ .Slots = .{ .slots = new_slot_list } };
                break :blk new_slot_list;
            },
            .Slots => |slots| {
                var resized_slots_list = try self.allocator.realloc(slots.slots, slots.slots.len + new_slots.len);
                self.content = .{ .Slots = .{ .slots = resized_slots_list } };
                break :blk resized_slots_list;
            },
            else => unreachable,
        }
    };

    const new_slots_offset = slot_list.len - new_slots.len;
    var i: usize = 0;
    while (i < new_slots.len) : (i += 1) {
        slot_list[new_slots_offset + i] = new_slots[i];
    }
}

/// Removes the given slot. Returns whether a slot was found and removed.
/// Returns error.ObjectDoesNotAcceptSlots if the object isn't an Empty or a
/// Slots.
pub fn removeSlot(self: *Self, name: []const u8) !bool {
    if (self.is(.Empty)) {
        return false;
    } else if (!self.is(.Slots)) {
        return error.ObjectDoesNotAcceptSlots;
    }

    const slots = self.content.Slots.slots;
    var slots_copy = try self.allocator.alloc(Slot, slots.len);
    errdefer self.allocator.free(slots_copy);

    var did_find_and_remove_slot = false;
    var slots_copy_cursor: usize = 0;
    for (slots) |*slot| {
        if (std.mem.eql(u8, name, slot.name)) {
            slot.deinit();
            did_find_and_remove_slot = true;
            continue;
        }

        slots_copy[slots_copy_cursor] = slot.*;
        slots_copy_cursor += 1;
    }

    // NOTE: Can't use deinitContent here since that would also deinit the slots
    self.allocator.free(slots);
    if (slots_copy_cursor == 0) {
        self.allocator.free(slots_copy);
        self.content = .{ .Empty = .{} };
    } else {
        slots_copy = try self.allocator.realloc(slots_copy, slots_copy_cursor);
        self.content = .{ .Slots = .{ .slots = slots_copy } };
    }

    return did_find_and_remove_slot;
}

/// Looks for a message of the form "slotName:", where "slotName" exists as a
/// mutable slot on the object. Returns the slot if it exists, or null when not
/// found.
pub fn getAssignableSlotForMessage(self: *Self, slot_name: []const u8) !?*Slot {
    if (slot_name[slot_name.len - 1] != ':') {
        return null;
    }

    const slot_name_without_colon = slot_name[0 .. slot_name.len - 1];
    if (try self.lookup(null, slot_name_without_colon, .Slot)) |slot| {
        if (slot.is_mutable) return slot;
    }

    return null;
}

const FindActivationReceiverError = Allocator.Error || runtime_error.SelfRuntimeError;
/// This is used for primitives to find the actual value the primitive is
/// supposed to send to. If the `_parent` slot exists, then that is returned as
/// the activation target; otherwise null is returned.
pub fn findActivationReceiver(self: *Self) FindActivationReceiverError!?Ref {
    if (self.is(.Activation)) {
        var receiver = self;
        while (receiver.is(.Activation)) {
            receiver = receiver.content.Activation.receiver.value;
        }

        return Ref{ .value = receiver };
    }

    return null;
}
