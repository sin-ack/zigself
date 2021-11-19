// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");
const ref_counted = @import("../utility/ref_counted.zig");
const Slot = @import("./slot.zig");

const Self = @This();
pub const Ref = ref_counted.RefPtrWithoutTypeChecks(Self);

const ObjectContent = union(enum) {
    Empty: void,

    Slots: struct {
        slots: []Slot,
    },

    Activation: struct {
        activation_object: Ref,
        context: union(enum) {
            Method: void,
            Block: struct {
                /// A reference to the method activation that this block is
                /// bound to. This is used for non-local returns. When a
                /// non-local return happens, the returned expression is raised
                /// through all the blocks it is within until the bound method
                /// is reached.
                bound_method: Ref,
            },
        },
    },

    Method: struct {
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

        /// See `ObjectContent.Activation.context.Block.bound_method`'s
        /// documentation.
        bound_method: Ref,
    },

    NonlocalReturn: struct {
        /// The target method activation object this non-local return targets.
        target_method: Ref,
        /// The value to be returned.
        value: Ref,
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

/// Takes ownership of `slots`.
pub fn createSlots(allocator: *Allocator, slots: []Slot) !Ref {
    return try create(allocator, .{ .Slots = .{ .slots = slots } });
}

/// Takes ownership of `arguments`, `slots` and `statements`.
pub fn createMethod(allocator: *Allocator, arguments: [][]const u8, slots: []Slot, statements: []AST.StatementNode) !Ref {
    return try create(allocator, .{ .Method = .{ .arguments = arguments, .slots = slots, .statements = statements } });
}

/// Takes ownership of `target_method` and `value`.
pub fn createNonlocalReturn(allocator: *Allocator, target_method: Ref, value: Ref) !Ref {
    return try create(allocator, .{ .NonlocalReturn = .{ .target_method = target_method, .value = value } });
}

/// Dupes `contents`. Borrows a ref for `parent` from the caller.
pub fn createCopyFromStringLiteral(allocator: *Allocator, contents: []const u8, parent: Ref) !Ref {
    const contents_copy = try allocator.dupe(u8, contents);
    errdefer allocator.free(contents_copy);

    return try create(allocator, .{ .ByteVector = .{ .parent = parent, .values = contents_copy } });
}

/// Borrows a ref for `parent` from the caller.
pub fn createFromIntegerLiteral(allocator: *Allocator, value: u64, parent: Ref) !Ref {
    return try create(allocator, .{ .Integer = .{ .parent = parent, .value = value } });
}

/// Borrows a ref for `parent` from the caller.
pub fn createFromFloatingPointLiteral(allocator: *Allocator, value: f64, parent: Ref) !Ref {
    return try create(allocator, .{ .FloatingPoint = .{ .parent = parent, .value = value } });
}

/// Takes ownership of `arguments`, `slots` and `statements`. Borrows a ref for
/// `bound_method` from the caller.
pub fn createBlock(allocator: *Allocator, arguments: [][]const u8, slots: []Slot, statements: []AST.StatementNode, bound_method: Ref) !Ref {
    return try create(allocator, .{
        .Block = .{
            .arguments = arguments,
            .slots = slots,
            .statements = statements,
            .bound_method = bound_method,
        },
    });
}

pub fn destroy(self: *Self) void {
    self.deinitContent();
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

fn deinitContent(self: *Self) void {
    switch (self.content) {
        .Empty => {},
        .Slots => |slots| {
            for (slots.slots) |*slot| {
                slot.deinit();
            }
            self.allocator.free(slots.slots);
        },
        .Activation => |activation| {
            activation.activation_object.unref();
            switch (activation.context) {
                .Method => {},
                .Block => |block_context| {
                    block_context.bound_method.unref();
                },
            }
        },
        .Method => |method| {
            for (method.arguments) |argument| {
                self.allocator.free(argument);
            }
            self.allocator.free(method.arguments);

            for (method.slots) |*slot| {
                slot.deinit();
            }
            self.allocator.free(method.slots);

            for (method.statements) |*statement| {
                statement.deinit(self.allocator);
            }
            self.allocator.free(method.statements);
        },
        .Block => |block| {
            for (block.arguments) |argument| {
                self.allocator.free(argument);
            }
            self.allocator.free(block.arguments);

            for (block.slots) |*slot| {
                slot.deinit();
            }
            self.allocator.free(block.slots);

            for (block.statements) |*statement| {
                statement.deinit(self.allocator);
            }
            self.allocator.free(block.statements);

            block.bound_method.unref();
        },
        .NonlocalReturn => |nonlocal_return| {
            nonlocal_return.target_method.unref();
            nonlocal_return.value.unref();
        },
        .ByteVector => |bytevector| {
            bytevector.parent.unref();
            self.allocator.free(bytevector.values);
        },
        .Integer => |integer| {
            integer.parent.unref();
        },
        .FloatingPoint => |floating_point| {
            floating_point.parent.unref();
        },
    }
}

const VisitedObjectsSet = std.AutoArrayHashMap(*Self, void);
const LookupType = enum { Value, Slot };
pub fn lookup(self: *Self, selector: []const u8, comptime lookup_type: LookupType) t: {
    if (lookup_type == .Value) {
        break :t Allocator.Error!?Ref;
    } else {
        break :t Allocator.Error!?*Slot;
    }
} {
    if (lookup_type == .Value) {
        if (std.mem.eql(u8, selector, "self")) {
            return Ref{ .value = self };
        }
    }

    var visited_objects = VisitedObjectsSet.init(self.allocator);
    defer visited_objects.deinit();

    return if (lookup_type == .Value)
        try self.lookupValue(selector, &visited_objects)
    else
        try self.lookupSlot(selector, &visited_objects);
}

/// Self Handbook, §3.3.8 The lookup algorithm
fn lookupValue(self: *Self, selector: []const u8, visited_objects: *VisitedObjectsSet) Allocator.Error!?Ref {
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
                    if (try slot.value.value.lookupValue(selector, visited_objects)) |found_object| {
                        return found_object;
                    }
                }
            }

            // Nope, not here
            return null;
        },

        .Method => @panic("Attempting to perform lookup on method?!"),
        .Activation, .NonlocalReturn => unreachable,

        .Block => |block| {
            // NOTE: executeMessage will handle the execution of the block itself.

            _ = block;
            @panic("FIXME handle parent for block");
        },

        // The 3 types below have an imaginary field called "parent" which contains
        // the parent object ref.
        .ByteVector => |vector| {
            if (std.mem.eql(u8, selector, "parent")) {
                return vector.parent;
            }

            return try vector.parent.value.lookupValue(selector, visited_objects);
        },

        .Integer => |integer| {
            if (std.mem.eql(u8, selector, "parent")) {
                return integer.parent;
            }

            return try integer.parent.value.lookupValue(selector, visited_objects);
        },

        .FloatingPoint => |floating_point| {
            if (std.mem.eql(u8, selector, "parent")) {
                return floating_point.parent;
            }

            return try floating_point.parent.value.lookupValue(selector, visited_objects);
        },
    }
}

/// Self Handbook, §3.3.8 The lookup algorithm
///
/// Like lookupValue but finds slots instead of values.
fn lookupSlot(self: *Self, selector: []const u8, visited_objects: *VisitedObjectsSet) Allocator.Error!?*Slot {
    // I'd like this to not be duplicated but unfortunately I couldn't reconcile
    // them.
    if (visited_objects.contains(self)) {
        return null;
    }

    try visited_objects.put(self, .{});

    switch (self.content) {
        .Empty, .Block, .ByteVector, .Integer, .FloatingPoint => return null,

        .Slots => |slots| {
            // Direct lookup
            for (slots.slots) |*slot| {
                if (std.mem.eql(u8, selector, slot.name)) {
                    return slot;
                }
            }

            // Parent lookup
            for (slots.slots) |slot| {
                if (slot.is_parent) {
                    if (try slot.value.value.lookupSlot(selector, visited_objects)) |found_slot| {
                        return found_slot;
                    }
                }
            }

            // Nope, not here
            return null;
        },

        .Method => @panic("Attempting to perform lookup on method?!"),
        .Activation, .NonlocalReturn => unreachable,
    }
}

// NOTE: This is currently unused but will become used once _Clone is available.
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

        .Method => |method| {
            var arguments_copy = try std.ArrayList([]const u8).initCapacity(self.allocator, method.arguments.len);
            errdefer {
                for (arguments_copy.items) |argument| {
                    self.allocator.free(argument);
                }
                arguments_copy.deinit();
            }

            var slots_copy = try std.ArrayList(Slot).initCapacity(self.allocator, method.slots.len);
            errdefer {
                for (slots_copy.items) |*slot| {
                    slot.deinit();
                }
                slots_copy.deinit();
            }

            var statements_copy = try std.ArrayList(AST.StatementNode).initCapacity(self.allocator, method.statements.len);
            errdefer {
                for (statements_copy.items) |*statement| {
                    statement.deinit(self.allocator);
                }
                statements_copy.deinit();
            }

            for (method.arguments) |argument| {
                var argument_copy = try self.allocator.dupe(u8, argument);
                errdefer self.allocator.free(argument_copy);

                try arguments_copy.append(argument_copy);
            }

            for (method.slots) |slot| {
                var slot_copy = try slot.copy();
                errdefer slot_copy.deinit();

                try slots_copy.append(slot_copy);
            }

            for (method.statements) |statement| {
                var statement_copy = try ASTCopyVisitor.visitStatement(statement, self.allocator);
                errdefer statement_copy.deinit(self.allocator);

                try statements_copy.append(statement_copy);
            }

            return try createMethod(self.allocator, arguments_copy.toOwnedSlice(), slots_copy.toOwnedSlice(), statements_copy.toOwnedSlice());
        },

        .ByteVector => |vector| {
            vector.parent.ref();
            return try createCopyFromStringLiteral(self.allocator, vector.values, vector.parent);
        },

        .Integer => |integer| {
            integer.parent.ref();
            return try createFromIntegerLiteral(self.allocator, integer.value, integer.parent);
        },

        .FloatingPoint => |floating_point| {
            floating_point.parent.ref();
            return try createFromFloatingPointLiteral(self.allocator, floating_point.value, floating_point.parent);
        },
    }
}

pub fn is(self: *Self, tag: std.meta.Tag(ObjectContent)) bool {
    return self.content == tag;
}

/// Returns whether the passed message name is the correct one for this block
/// to be executed. The logic is:
///
/// - For blocks with no arguments, `value`
/// - For blocks with a single argument, `value:`
/// - For blocks with more than one argument, `value:With:`, with as many
///   `With:`s as needed (number of colons should match number of arguments)
pub fn isCorrectMessageForBlockExecution(self: Self, message: []const u8) bool {
    std.debug.assert(self.content == .Block);

    if (self.content.Block.arguments.len == 0 and std.mem.eql(u8, message, "value")) {
        return true;
    }

    if (!std.mem.eql(u8, message[0..6], "value:")) {
        return false;
    }

    var remaining_message = message[6..];
    var remaining_arguments = self.content.Block.arguments.len - 1;
    while (remaining_arguments > 0) : (remaining_arguments -= 1) {
        if (remaining_message.len == 0)
            return false;

        var with_slice = remaining_message[0..5];
        if (!std.mem.eql(u8, with_slice, "With:"))
            return false;

        remaining_message = remaining_message[5..];
    }

    return remaining_message.len == 0;
}

fn activateCommon(allocator: *Allocator, arguments: []Ref, argument_names: [][]const u8, slots: []Slot, parent: Ref) !Ref {
    std.debug.assert(arguments.len == argument_names.len);

    var slots_copy = try std.ArrayList(Slot).initCapacity(allocator, slots.len + arguments.len + 1);
    errdefer {
        for (slots_copy.items) |*slot| {
            slot.deinit();
        }
        slots_copy.deinit();
    }

    // Add argument slots first, as those are more likely to be used
    for (arguments) |argument, i| {
        var new_slot = try Slot.init(allocator, false, false, argument_names[i], argument);
        errdefer new_slot.deinit();

        try slots_copy.append(new_slot);
    }

    // _parent because Self code cannot create or reach slots with _ at start.
    var parent_slot = try Slot.init(allocator, false, true, "_parent", parent);
    errdefer parent_slot.deinit();

    try slots_copy.append(parent_slot);

    for (slots) |slot| {
        var slot_copy = try slot.copy();
        errdefer slot_copy.deinit();

        try slots_copy.append(slot_copy);
    }

    return try createSlots(allocator, slots_copy.toOwnedSlice());
}

/// Activate this block and return a slots object for it.
/// Borrows 1 ref from each object in `arguments` and from `parent` and
/// `bound_method`.
pub fn activateBlock(self: Self, arguments: []Ref, parent: Ref, bound_method: Ref) !Ref {
    std.debug.assert(self.content == .Block);
    const activation_object = try activateCommon(self.allocator, arguments, self.content.Block.arguments, self.content.Block.slots, parent);
    errdefer activation_object.unref();

    return try create(self.allocator, .{ .Activation = .{ .activation_object = activation_object, .context = .{ .Block = .{ .bound_method = bound_method } } } });
}

/// Activate this method and return a slots object for it.
/// Borrows 1 ref from each object in `arguments` and from `parent`.
pub fn activateMethod(self: Self, arguments: []Ref, parent: Ref) !Ref {
    std.debug.assert(self.content == .Method);
    const activation_object = try activateCommon(self.allocator, arguments, self.content.Method.arguments, self.content.Method.slots, parent);
    errdefer activation_object.unref();

    return try create(self.allocator, .{ .Activation = .{ .activation_object = activation_object, .context = .{ .Method = .{} } } });
}

/// Return the method that should be bound for the non-local return.
pub fn getBoundMethodForActivation(self: Self) Ref {
    std.debug.assert(self.content == .Activation);

    return switch (self.content.Activation.context) {
        .Method => self.content.Activation.activation_object,
        .Block => |block_context| block_context.bound_method,
    };
}

/// Attempt to add the given slot objects to the content. Only Empty and Slots
/// objects allow this; otherwise, error.ObjectDoesNotAcceptSlots is returned.
/// The slots' ownership is passed to the object.
pub fn addSlots(self: *Self, new_slots: []Slot) !void {
    switch (self.content) {
        .Empty, .Slots => {},
        else => return error.ObjectDoesNotAcceptSlots,
    }

    var slot_list = blk: {
        switch (self.content) {
            .Empty => {
                self.deinitContent();

                var an_empty_slot_list_to_make_this_streamlined = try self.allocator.alloc(Slot, 0);
                self.content = .{ .Slots = .{ .slots = an_empty_slot_list_to_make_this_streamlined } };
                break :blk an_empty_slot_list_to_make_this_streamlined;
            },
            .Slots => |slots| {
                break :blk slots.slots;
            },
            else => unreachable,
        }
    };

    const slot_offset = slot_list.len;
    // NOTE: No need to assign to self.content.Slots, resize is guaranteed not
    //       to move.
    slot_list = try self.allocator.resize(slot_list, slot_offset + new_slots.len);

    var i: usize = 0;
    while (i < new_slots.len) : (i += 1) {
        slot_list[slot_offset + i] = new_slots[i];
    }
}

/// Looks for a message of the form "slotName:", where "slotName" exists as a
/// mutable slot on the object. Returns the slot if it exists, or null when not
/// found.
pub fn getAssignableSlotForMessage(self: *Self, slot_name: []const u8) !?*Slot {
    if (slot_name[slot_name.len - 1] != ':') {
        return null;
    }

    const slot_name_without_colon = slot_name[0 .. slot_name.len - 1];
    if (try self.lookup(slot_name_without_colon, .Slot)) |slot| {
        if (slot.is_mutable) return slot;
    }

    return null;
}
