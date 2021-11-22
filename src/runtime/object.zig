// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const Slot = @import("./slot.zig");
const weak_ref = @import("../utility/weak_ref.zig");
const ref_counted = @import("../utility/ref_counted.zig");
const runtime_error = @import("./error.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");
const InterpreterContext = @import("./interpreter.zig").InterpreterContext;

const Self = @This();
const WeakBlock = weak_ref.WeakPtrBlock(Self);
const Weak = weak_ref.WeakPtr(Self);
pub const Ref = ref_counted.RefPtrWithoutTypeChecks(Self);

const EnableObjectRefTracker = false;

const ObjectMap = std.AutoArrayHashMap(*Self, void);
var object_ref_tracker: ?ObjectMap = null;

pub fn setupObjectRefTracker(allocator: *Allocator) void {
    if (!EnableObjectRefTracker) return;

    object_ref_tracker = ObjectMap.init(allocator);
}

pub fn teardownObjectRefTrackerAndReportAliveRefs() void {
    if (!EnableObjectRefTracker) return;

    if (object_ref_tracker.?.keys().len > 0) {
        std.debug.print("Remaining object refs during teardown:\n", .{});

        var iterator = object_ref_tracker.?.iterator();
        while (iterator.next()) |item| {
            const object = item.key_ptr.*;
            std.debug.print("  <*{d}> (type {s}) - {d} refs\n", .{ object.id, @tagName(object.content), object.ref.ref_count });
        }
    }

    object_ref_tracker.?.deinit();
}

const ObjectContent = union(enum) {
    Empty: void,

    Slots: struct {
        slots: []Slot,
    },

    Activation: struct {
        activation_object: Ref,
        message_name: []const u8,
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

        /// See `ObjectContent.Activation.context.Block.bound_method`'s
        /// documentation.
        bound_method: Weak,
    },

    NonlocalReturn: struct {
        /// The target method activation object this non-local return targets.
        target_method: Ref,
        /// The value to be returned.
        value: Ref,
    },

    ByteVector: struct {
        values: []const u8,
    },

    Integer: struct {
        value: u64,
    },

    FloatingPoint: struct {
        value: f64,
    },
};

var current_id: usize = 0;

ref: ref_counted.RefCount,
weak: WeakBlock,
allocator: *Allocator,
content: ObjectContent,
id: usize,

pub fn createEmpty(allocator: *Allocator) !Ref {
    return try create(allocator, .{ .Empty = .{} });
}

/// Takes ownership of `slots`.
pub fn createSlots(allocator: *Allocator, slots: []Slot) !Ref {
    return try create(allocator, .{ .Slots = .{ .slots = slots } });
}

/// Takes ownership of `arguments`, `slots` and `statements`.
/// `message_name` is duped.
pub fn createMethod(allocator: *Allocator, message_name: []const u8, arguments: [][]const u8, slots: []Slot, statements: []AST.StatementNode) !Ref {
    var message_name_copy = try allocator.dupe(u8, message_name);
    errdefer allocator.free(message_name_copy);

    return try create(allocator, .{ .Method = .{ .message_name = message_name_copy, .arguments = arguments, .slots = slots, .statements = statements } });
}

/// Takes ownership of `target_method` and `value`.
pub fn createNonlocalReturn(allocator: *Allocator, target_method: Ref, value: Ref) !Ref {
    return try create(allocator, .{ .NonlocalReturn = .{ .target_method = target_method, .value = value } });
}

/// Dupes `contents`.
pub fn createCopyFromStringLiteral(allocator: *Allocator, contents: []const u8) !Ref {
    const contents_copy = try allocator.dupe(u8, contents);
    errdefer allocator.free(contents_copy);

    return try create(allocator, .{ .ByteVector = .{ .values = contents_copy } });
}

pub fn createFromIntegerLiteral(allocator: *Allocator, value: u64) !Ref {
    return try create(allocator, .{ .Integer = .{ .value = value } });
}

pub fn createFromFloatingPointLiteral(allocator: *Allocator, value: f64) !Ref {
    return try create(allocator, .{ .FloatingPoint = .{ .value = value } });
}

/// Takes ownership of `arguments`, `slots` and `statements`. Borrows a ref for
/// `bound_method` from the caller.
pub fn createBlock(allocator: *Allocator, arguments: [][]const u8, slots: []Slot, statements: []AST.StatementNode, bound_method: Ref) !Ref {
    var bound_method_weak = Weak.init(bound_method.value);
    errdefer bound_method_weak.deinit();

    return try create(allocator, .{
        .Block = .{
            .arguments = arguments,
            .slots = slots,
            .statements = statements,
            .bound_method = bound_method_weak,
        },
    });
}

pub fn destroy(self: *Self) void {
    self.deinitContent();
    self.weak.deinit();

    if (EnableObjectRefTracker) _ = object_ref_tracker.?.swapRemove(self);
    self.allocator.destroy(self);
}

fn create(allocator: *Allocator, content: ObjectContent) !Ref {
    const self = try allocator.create(Self);
    try self.init(allocator, content);

    if (EnableObjectRefTracker) try object_ref_tracker.?.put(self, .{});
    return Ref.adopt(self);
}

fn init(self: *Self, allocator: *Allocator, content: ObjectContent) !void {
    self.id = current_id;
    current_id += 1;
    self.ref = .{};
    self.weak = try WeakBlock.init(allocator, self);

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
            self.allocator.free(activation.message_name);
            switch (activation.context) {
                .Method => {},
                .Block => |block_context| {
                    block_context.bound_method.unref();
                },
            }
        },
        .Method => |method| {
            self.allocator.free(method.message_name);

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

            block.bound_method.deinit();
        },
        .NonlocalReturn => |nonlocal_return| {
            nonlocal_return.target_method.unref();
            nonlocal_return.value.unref();
        },
        .ByteVector => |bytevector| {
            self.allocator.free(bytevector.values);
        },
        .Integer, .FloatingPoint => {},
    }
}

const VisitedObjectsSet = std.AutoArrayHashMap(*Self, void);
const LookupType = enum { Value, Slot };
const LookupError = Allocator.Error || runtime_error.SelfRuntimeError;
pub fn lookup(self: *Self, context: ?*InterpreterContext, selector: []const u8, comptime lookup_type: LookupType) t: {
    if (lookup_type == .Value) {
        break :t LookupError!?Ref;
    } else {
        break :t LookupError!?*Slot;
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
        try self.lookupValue(context, selector, &visited_objects)
    else
        try self.lookupSlot(selector, &visited_objects);
}

/// Self Handbook, §3.3.8 The lookup algorithm
fn lookupValue(self: *Self, context: ?*InterpreterContext, selector: []const u8, visited_objects: *VisitedObjectsSet) LookupError!?Ref {
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
                    if (try slot.value.value.lookupValue(context, selector, visited_objects)) |found_object| {
                        return found_object;
                    }
                }
            }

            // Nope, not here
            return null;
        },

        .Method, .Activation, .NonlocalReturn => unreachable,

        // The 4 types below have an imaginary field called "parent" which
        // refers to their respective traits objects.

        .Block => {
            // NOTE: executeMessage will handle the execution of the block itself.

            if (context) |ctx| {
                if (try ctx.lobby.value.lookup(context, "traits", .Value)) |traits| {
                    if (try traits.value.lookup(context, "block", .Value)) |traits_block| {
                        if (std.mem.eql(u8, selector, "parent"))
                            return traits_block;

                        return try traits_block.value.lookup(ctx, selector, .Value);
                    } else {
                        return runtime_error.raiseError(self.allocator, ctx, "Could not find block in traits", .{});
                    }
                } else {
                    return runtime_error.raiseError(self.allocator, ctx, "Could not find traits in lobby", .{});
                }
            } else {
                @panic("Context MUST be passed for Block objects!");
            }
        },
        .ByteVector => {
            if (context) |ctx| {
                if (try ctx.lobby.value.lookup(context, "traits", .Value)) |traits| {
                    if (try traits.value.lookup(context, "string", .Value)) |traits_string| {
                        if (std.mem.eql(u8, selector, "parent"))
                            return traits_string;

                        return try traits_string.value.lookup(ctx, selector, .Value);
                    } else {
                        return runtime_error.raiseError(self.allocator, ctx, "Could not find string in traits", .{});
                    }
                } else {
                    return runtime_error.raiseError(self.allocator, ctx, "Could not find traits in lobby", .{});
                }
            } else {
                @panic("Context MUST be passed for ByteVector objects!");
            }
        },
        .Integer => {
            if (context) |ctx| {
                if (try ctx.lobby.value.lookup(context, "traits", .Value)) |traits| {
                    if (try traits.value.lookup(context, "integer", .Value)) |traits_integer| {
                        if (std.mem.eql(u8, selector, "parent"))
                            return traits_integer;

                        return try traits_integer.value.lookup(ctx, selector, .Value);
                    } else {
                        return runtime_error.raiseError(self.allocator, ctx, "Could not find integer in traits", .{});
                    }
                } else {
                    return runtime_error.raiseError(self.allocator, ctx, "Could not find traits in lobby", .{});
                }
            } else {
                @panic("Context MUST be passed for Integer objects!");
            }
        },

        .FloatingPoint => {
            if (context) |ctx| {
                if (try ctx.lobby.value.lookup(context, "traits", .Value)) |traits| {
                    if (try traits.value.lookup(context, "float", .Value)) |traits_float| {
                        if (std.mem.eql(u8, selector, "parent"))
                            return traits_float;

                        return try traits_float.value.lookup(ctx, selector, .Value);
                    } else {
                        return runtime_error.raiseError(self.allocator, ctx, "Could not find float in traits", .{});
                    }
                } else {
                    return runtime_error.raiseError(self.allocator, ctx, "Could not find traits in lobby", .{});
                }
            } else {
                @panic("Context MUST be passed for FloatingPoint objects!");
            }
        },
    }
}

/// Self Handbook, §3.3.8 The lookup algorithm
///
/// Like lookupValue but finds slots instead of values.
fn lookupSlot(self: *Self, selector: []const u8, visited_objects: *VisitedObjectsSet) LookupError!?*Slot {
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

            // FIXME: The bound method might be gone at this point, just copy the
            //        weak ref here instead.
            var bound_method_ref = Ref{ .value = block.bound_method.getPointer().? };
            return try createBlock(self.allocator, arguments_copy.toOwnedSlice(), slots_copy.toOwnedSlice(), statements_copy.toOwnedSlice(), bound_method_ref);
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

        .NonlocalReturn, .Activation, .Method => unreachable,
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

    if (message.len < 6 or !std.mem.eql(u8, message[0..6], "value:")) {
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

fn createMessageNameForBlock(self: Self) ![]const u8 {
    std.debug.assert(self.content == .Block);

    var needed_space: usize = 5; // value
    if (self.content.Block.arguments.len > 0) {
        needed_space += 1; // :
        needed_space += 5 * (self.content.Block.arguments.len - 1); // Any other With:s needed
    }

    var message_name = try self.allocator.alloc(u8, needed_space);
    std.mem.copy(u8, message_name, "value");

    if (self.content.Block.arguments.len > 0) {
        message_name[5] = ':';

        var remaining_buffer = message_name[6..];
        while (remaining_buffer.len > 0) {
            std.mem.copy(u8, remaining_buffer, "With:");
            remaining_buffer = remaining_buffer[5..];
        }
    }

    return message_name;
}

fn activateCommon(
    allocator: *Allocator,
    context: *InterpreterContext,
    arguments: []Ref,
    argument_names: [][]const u8,
    slots: []Slot,
    parent: Ref,
    check_activation_receiver: bool,
) !Ref {
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

    // NOTE: This is very important! When we're performing a method activation,
    //       we must NOT parent previous method or block activation objects,
    //       that would make their slots visible to us which we absolutely do
    //       not want.
    //
    //       Not only that, but that would also cause the actual object to be
    //       wrapped in multiple layers of activation objects like ogres.
    var the_parent = parent;
    if (check_activation_receiver) {
        if (try parent.value.findActivationReceiver(context)) |actual_parent| {
            the_parent = actual_parent;
        }
    }
    the_parent.ref();

    // _parent because Self code cannot create or reach slots with _ at start.
    var parent_slot = try Slot.init(allocator, false, true, "_parent", the_parent);
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
/// Borrows 1 ref from each object in `arguments`.
/// `bound_method` is ref'd internally.
pub fn activateBlock(self: Self, context: *InterpreterContext, arguments: []Ref, bound_method: Ref) !Ref {
    std.debug.assert(self.content == .Block);
    const activation_object = try activateCommon(self.allocator, context, arguments, self.content.Block.arguments, self.content.Block.slots, bound_method, false);
    errdefer activation_object.unref();

    var message_name = try self.createMessageNameForBlock();
    errdefer self.allocator.free(message_name);

    bound_method.ref();
    return try create(self.allocator, .{
        .Activation = .{
            .activation_object = activation_object,
            .message_name = message_name,
            .context = .{ .Block = .{ .bound_method = bound_method } },
        },
    });
}

/// Activate this method and return a slots object for it.
/// Borrows 1 ref from each object in `arguments`.
/// `parent` is ref'd internally.
pub fn activateMethod(self: Self, context: *InterpreterContext, arguments: []Ref, parent: Ref) !Ref {
    std.debug.assert(self.content == .Method);
    const activation_object = try activateCommon(self.allocator, context, arguments, self.content.Method.arguments, self.content.Method.slots, parent, true);
    errdefer activation_object.unref();

    var message_name_copy = try self.allocator.dupe(u8, self.content.Method.message_name);
    errdefer self.allocator.free(message_name_copy);

    return try create(self.allocator, .{
        .Activation = .{
            .activation_object = activation_object,
            .message_name = message_name_copy,
            .context = .{ .Method = .{} },
        },
    });
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
                self.deinitContent();

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

const FindActivationReceiverError = LookupError;
/// This is used for primitives to find the actual value the primitive is
/// supposed to send to. If the `_parent` slot exists, then that is returned as
/// the activation target; otherwise null is returned.
pub fn findActivationReceiver(self: *Self, context: *InterpreterContext) FindActivationReceiverError!?Ref {
    if (try self.lookup(context, "_parent", .Value)) |parent| {
        if (try parent.value.findActivationReceiver(context)) |even_more_parent| {
            return even_more_parent;
        } else {
            return parent;
        }
    } else {
        return null;
    }
}
