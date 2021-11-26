// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const Slot = @import("./slot.zig");
const hash = @import("../utility/hash.zig");
const Range = @import("../language/location_range.zig");
const Script = @import("../language/script.zig");
const weak_ref = @import("../utility/weak_ref.zig");
const Activation = @import("./activation.zig");
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

    Integer: struct {
        value: i64,
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
            for (activation.slots) |*slot| {
                slot.deinit();
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
                slot.deinit();
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
                slot.deinit();
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
        .Integer, .FloatingPoint => {},
    }
}

const LookupType = enum { Value, Slot };
const LookupError = Allocator.Error || runtime_error.SelfRuntimeError;
const VisitedObjectLink = struct { previous: ?*const VisitedObjectLink = null, object: *Self };
fn lookupReturnType(comptime lookup_type: LookupType) type {
    if (lookup_type == .Value) {
        return LookupError!?Ref;
    } else {
        return LookupError!?*Slot;
    }
}

pub fn lookup(self: *Self, context: ?*InterpreterContext, selector: []const u8, comptime lookup_type: LookupType) lookupReturnType(lookup_type) {
    const selector_hash = hash.stringHash(selector);
    return self.lookupHash(context, selector_hash, lookup_type);
}

const self_hash = hash.stringHash("self");
const parent_hash = hash.stringHash("parent");

fn lookupHash(self: *Self, context: ?*InterpreterContext, selector_hash: u32, comptime lookup_type: LookupType) lookupReturnType(lookup_type) {
    if (lookup_type == .Value) {
        if (selector_hash == self_hash) {
            return Ref{ .value = self };
        }
    }

    return if (lookup_type == .Value)
        try self.lookupValue(context, selector_hash, null)
    else
        try self.lookupSlot(selector_hash, null);
}

/// Self Handbook, §3.3.8 The lookup algorithm
fn lookupValue(self: *Self, context: ?*InterpreterContext, selector_hash: u32, previously_visited: ?*const VisitedObjectLink) LookupError!?Ref {
    if (previously_visited) |visited| {
        var link: ?*const VisitedObjectLink = visited;
        while (link) |l| {
            if (l.object == self) {
                // Cyclic reference
                return null;
            }

            link = l.previous;
        }
    }

    const currently_visited = VisitedObjectLink{ .previous = previously_visited, .object = self };

    switch (self.content) {
        .Empty => return null,

        .Slots => |slots| {
            // Direct lookup
            for (slots.slots) |slot| {
                if (slot.name_hash == selector_hash) {
                    return slot.value;
                }
            }

            // Parent lookup
            for (slots.slots) |slot| {
                if (slot.is_parent) {
                    if (try slot.value.value.lookupValue(context, selector_hash, &currently_visited)) |found_object| {
                        return found_object;
                    }
                }
            }

            // Nope, not here
            return null;
        },

        // FIXME: Don't repeat this code with .Slots
        .Activation => |activation| {
            // Direct lookup
            for (activation.slots) |slot| {
                if (slot.name_hash == selector_hash) {
                    return slot.value;
                }
            }

            // Parent lookup
            for (activation.slots) |slot| {
                if (slot.is_parent) {
                    if (try slot.value.value.lookupValue(context, selector_hash, &currently_visited)) |found_object| {
                        return found_object;
                    }
                }
            }

            // Receiver lookup
            if (try activation.receiver.value.lookupValue(context, selector_hash, &currently_visited)) |found_object| {
                return found_object;
            }

            // Nope, not here
            return null;
        },

        .Method => unreachable,

        // The 4 types below have an imaginary field called "parent" which
        // refers to their respective traits objects.

        .Block => {
            // NOTE: executeMessage will handle the execution of the block itself.

            if (context) |ctx| {
                if (try ctx.lobby.value.lookup(context, "traits", .Value)) |traits| {
                    if (try traits.value.lookup(context, "block", .Value)) |traits_block| {
                        if (selector_hash == parent_hash)
                            return traits_block;

                        return try traits_block.value.lookupHash(ctx, selector_hash, .Value);
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
                        if (selector_hash == parent_hash)
                            return traits_string;

                        return try traits_string.value.lookupHash(ctx, selector_hash, .Value);
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
                        if (selector_hash == parent_hash)
                            return traits_integer;

                        return try traits_integer.value.lookupHash(ctx, selector_hash, .Value);
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
                        if (selector_hash == parent_hash)
                            return traits_float;

                        return try traits_float.value.lookupHash(ctx, selector_hash, .Value);
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
fn lookupSlot(self: *Self, selector_hash: u32, previously_visited: ?*const VisitedObjectLink) LookupError!?*Slot {
    // I'd like this to not be duplicated but unfortunately I couldn't reconcile
    // them.
    if (previously_visited) |visited| {
        var link: ?*const VisitedObjectLink = visited;
        while (link) |l| {
            if (l.object == self) {
                // Cyclic reference
                return null;
            }

            link = l.previous;
        }
    }

    const currently_visited = VisitedObjectLink{ .previous = previously_visited, .object = self };

    switch (self.content) {
        .Empty, .Block, .ByteVector, .Integer, .FloatingPoint => return null,

        .Slots => |slots| {
            // Direct lookup
            for (slots.slots) |*slot| {
                if (slot.name_hash == selector_hash) {
                    return slot;
                }
            }

            // Parent lookup
            for (slots.slots) |slot| {
                if (slot.is_parent) {
                    if (try slot.value.value.lookupSlot(selector_hash, &currently_visited)) |found_slot| {
                        return found_slot;
                    }
                }
            }

            // Nope, not here
            return null;
        },

        // FIXME: Don't repeat this code
        .Activation => |activation| {
            // Direct lookup
            for (activation.slots) |*slot| {
                if (slot.name_hash == selector_hash) {
                    return slot;
                }
            }

            // Parent lookup
            for (activation.slots) |slot| {
                if (slot.is_parent) {
                    if (try slot.value.value.lookupSlot(selector_hash, &currently_visited)) |found_slot| {
                        return found_slot;
                    }
                }
            }

            // Receiver lookup
            if (try activation.receiver.value.lookupSlot(selector_hash, &currently_visited)) |found_slot| {
                return found_slot;
            }

            // Nope, not here
            return null;
        },

        .Method => @panic("Attempting to perform lookup on method?!"),
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

        .Method => unreachable,
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

fn createActivationObject(
    allocator: *Allocator,
    arguments: []Ref,
    argument_names: [][]const u8,
    slots: []Slot,
    receiver: Ref,
    check_activation_receiver: bool,
) !Ref {
    std.debug.assert(arguments.len == argument_names.len);

    var slots_copy = try std.ArrayList(Slot).initCapacity(allocator, slots.len + arguments.len);
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

    for (slots) |slot| {
        var slot_copy = try slot.copy();
        errdefer slot_copy.deinit();

        try slots_copy.append(slot_copy);
    }

    // NOTE: This is very important! When we're performing a method activation,
    //       we must NOT select previous method or block activation objects as
    //       receiver, that would make their slots visible to us which we
    //       absolutely do not want.
    //
    //       Not only that, but that would also cause the actual object to be
    //       wrapped in multiple layers of activation objects like ogres.
    var the_receiver = receiver;
    if (check_activation_receiver) {
        if (try receiver.value.findActivationReceiver()) |actual_receiver| {
            the_receiver = actual_receiver;
        }
    }
    the_receiver.ref();

    return try create(allocator, .{ .Activation = .{ .slots = slots_copy.toOwnedSlice(), .receiver = the_receiver } });
}

/// Activate this block and return a slots object for it.
/// Borrows 1 ref from each object in `arguments`.
/// `parent_activation_object` is ref'd internally.
/// `context.script` is ref'd once.
pub fn activateBlock(self: Self, context: *InterpreterContext, message_range: Range, arguments: []Ref, parent_activation_object: Ref) !*Activation {
    std.debug.assert(self.content == .Block);

    const activation_object = try createActivationObject(
        self.allocator,
        arguments,
        self.content.Block.arguments,
        self.content.Block.slots,
        parent_activation_object,
        false,
    );
    errdefer activation_object.unref();

    var message_name = try self.createMessageNameForBlock();
    errdefer self.allocator.free(message_name);

    context.script.ref();
    errdefer context.script.unref();

    var activation = try Activation.create(self.allocator, activation_object, message_name, context.script, message_range);
    // If we got here then the parent and non-local return target activations
    // must exist.
    activation.parent_activation = self.content.Block.parent_activation.getPointer().?;
    activation.nonlocal_return_target_activation = self.content.Block.nonlocal_return_target_activation.getPointer().?;
    return activation;
}

/// Activate this method and return a slots object for it.
/// Borrows 1 ref from each object in `arguments`.
/// `parent` is ref'd internally.
/// `context.script` is ref'd once.
pub fn activateMethod(self: Self, context: *InterpreterContext, message_range: Range, arguments: []Ref, parent: Ref) !*Activation {
    std.debug.assert(self.content == .Method);

    const activation_object = try createActivationObject(
        self.allocator,
        arguments,
        self.content.Method.arguments,
        self.content.Method.slots,
        parent,
        true,
    );
    errdefer activation_object.unref();

    var message_name_copy = try self.allocator.dupe(u8, self.content.Method.message_name);
    errdefer self.allocator.free(message_name_copy);

    context.script.ref();
    errdefer context.script.unref();

    return try Activation.create(self.allocator, activation_object, message_name_copy, context.script, message_range);
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
