// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");
const Object = @import("./object.zig");
const Slot = @import("./slot.zig");
const primitives = @import("./primitives.zig");
const basic_primitives = @import("./primitives/basic.zig");

/// Executes a script node. `self_object` and `lobby` are ref'd for the function
/// lifetime. The last expression result is returned, or if no statements were
/// available, null is returned.
pub fn executeScript(allocator: *Allocator, script: AST.ScriptNode, self_object: Object.Ref, lobby: Object.Ref) !?Object.Ref {
    self_object.ref();
    defer self_object.unref();

    lobby.ref();
    defer lobby.unref();

    var last_expression_result: ?Object.Ref = null;

    for (script.statements) |statement| {
        if (last_expression_result) |*result| {
            result.unref();
        }

        last_expression_result = try executeStatement(allocator, statement, self_object, lobby);
    }

    return last_expression_result;
}

/// Executes a statement. All refs are forwardded.
pub fn executeStatement(allocator: *Allocator, statement: AST.StatementNode, self_object: Object.Ref, lobby: Object.Ref) !Object.Ref {
    return try executeExpression(allocator, statement.expression, self_object, lobby);
}

/// Executes an expression. All refs are forwarded.
pub fn executeExpression(allocator: *Allocator, expression: AST.ExpressionNode, self_object: Object.Ref, lobby: Object.Ref) Allocator.Error!Object.Ref {
    return switch (expression) {
        .Object => |object| try executeObject(allocator, object.*, self_object, lobby),
        .Block => |block| try executeBlock(allocator, block.*, self_object, lobby),
        .Message => |message| try executeMessage(allocator, message.*, self_object, lobby),
        .Return => |return_node| try executeReturn(allocator, return_node.*, self_object),

        .Identifier => |identifier| try executeIdentifier(identifier, self_object),
        .String => |string| try executeString(allocator, string, lobby),
        .Number => |number| try executeNumber(allocator, number, lobby),
    };
}

/// Creates a new method object. All refs are forwarded. `arguments` and
/// `object_node`'s statements are copied.
fn executeMethod(allocator: *Allocator, object_node: AST.ObjectNode, arguments: [][]const u8, self_object: Object.Ref, lobby: Object.Ref) !Object.Ref {
    var arguments_copy = try std.ArrayList([]const u8).initCapacity(allocator, arguments.len);
    errdefer {
        for (arguments_copy.items) |argument| {
            allocator.free(argument);
        }
        arguments_copy.deinit();
    }

    var slots = try std.ArrayList(Slot).initCapacity(allocator, object_node.slots.len);
    errdefer {
        for (slots.items) |*slot| {
            slot.deinit();
        }
        slots.deinit();
    }

    var statements = try std.ArrayList(AST.StatementNode).initCapacity(allocator, object_node.slots.len);
    errdefer {
        for (statements.items) |*statement| {
            statement.deinit(allocator);
        }
        statements.deinit();
    }

    for (arguments) |argument| {
        var argument_copy = try allocator.dupe(u8, argument);
        errdefer allocator.free(argument_copy);

        try arguments_copy.append(argument_copy);
    }

    for (object_node.slots) |slot| {
        var slot_copy = try executeSlot(allocator, slot, self_object, lobby);
        errdefer slot_copy.deinit();

        try slots.append(slot_copy);
    }

    for (object_node.statements) |statement| {
        var statement_copy = try ASTCopyVisitor.visitStatement(statement, allocator);
        errdefer statement_copy.deinit(allocator);

        try statements.append(statement_copy);
    }

    return try Object.createMethod(allocator, arguments_copy.toOwnedSlice(), slots.toOwnedSlice(), statements.toOwnedSlice());
}

/// Creates a new slot. All refs are forwarded.
pub fn executeSlot(allocator: *Allocator, slot_node: AST.SlotNode, self_object: Object.Ref, lobby: Object.Ref) Allocator.Error!Slot {
    var value = blk: {
        if (slot_node.arguments.len > 0) {
            std.debug.assert(slot_node.value == .Object);
            break :blk try executeMethod(allocator, slot_node.value.Object.*, slot_node.arguments, self_object, lobby);
        } else {
            break :blk try executeExpression(allocator, slot_node.value, self_object, lobby);
        }
    };
    errdefer value.unref();

    return try Slot.init(allocator, slot_node.is_mutable, slot_node.is_parent, slot_node.name, value);
}

/// Creates a new slots object. All refs are forwarded.
pub fn executeObject(allocator: *Allocator, object_node: AST.ObjectNode, self_object: Object.Ref, lobby: Object.Ref) !Object.Ref {
    // Verify that we are executing a slots object and not a method; methods
    // are created through executeSlot.
    if (object_node.statements.len > 0) {
        @panic("!!! Attempted to execute a non-slots object! Methods must be created via executeSlot.");
    }

    if (object_node.slots.len == 0) {
        return try Object.createEmpty(allocator);
    }

    var slots = try std.ArrayList(Slot).initCapacity(allocator, object_node.slots.len);
    errdefer {
        for (slots.items) |*slot| {
            slot.deinit();
        }
        slots.deinit();
    }

    for (object_node.slots) |*slot_node| {
        var slot = try executeSlot(allocator, slot_node.*, self_object, lobby);
        errdefer slot.deinit();

        try slots.append(slot);
    }

    return try Object.createSlots(allocator, slots.toOwnedSlice());
}

pub fn executeBlock(allocator: *Allocator, block: AST.BlockNode, self_object: Object.Ref, lobby: Object.Ref) !Object.Ref {
    _ = allocator;
    _ = block;
    _ = self_object;
    _ = lobby;

    @panic("executeBlock has not been implemented yet!");
}

/// Executes a message. `self_object` and `lobby` are forwarded.
pub fn executeMessage(allocator: *Allocator, message: AST.MessageNode, self_object: Object.Ref, lobby: Object.Ref) !Object.Ref {
    // Primitive check
    if (message.message_name[0] == '_') {
        // All primitives borrow a ref from the caller for the receiver and
        // each argument. It is the primitive's job to unref any argument after
        // its work is done.
        if (primitives.hasPrimitive(message.message_name)) {
            var receiver = try executeExpression(allocator, message.receiver, self_object, lobby);
            errdefer receiver.unref();

            var arguments = try std.ArrayList(Object.Ref).initCapacity(allocator, message.arguments.len);
            defer arguments.deinit();
            errdefer {
                for (arguments.items) |*argument| {
                    argument.unref();
                }
            }

            for (message.arguments) |argument| {
                var expression_result = try executeExpression(allocator, argument, self_object, lobby);
                errdefer expression_result.unref();

                try arguments.append(expression_result);
            }

            return try primitives.callPrimitive(allocator, message.message_name, receiver, arguments.items, lobby);
        } else {
            std.debug.panic("Unknown primitive selector \"{s}\"\n", .{message.message_name});
        }
    }

    // Non-primitive messages
    var receiver = try executeExpression(allocator, message.receiver, self_object, lobby);
    defer receiver.unref();

    if (try receiver.value.lookup(message.message_name)) |lookup_result| {
        switch (lookup_result.value.content) {
            .Integer, .FloatingPoint, .ByteVector, .Slots, .Empty => {
                lookup_result.ref();
                return lookup_result;
            },

            .Method => |method| {
                var arguments = try std.ArrayList(Object.Ref).initCapacity(allocator, message.arguments.len);
                defer arguments.deinit();
                errdefer {
                    for (arguments.items) |*argument| {
                        argument.unref();
                    }
                }

                for (message.arguments) |argument| {
                    var expression_result = try executeExpression(allocator, argument, self_object, lobby);
                    errdefer expression_result.unref();

                    try arguments.append(expression_result);
                }

                var method_activation = try lookup_result.value.activateMethod(arguments.items);
                defer method_activation.unref();

                var last_expression_result: ?Object.Ref = null;

                for (method.statements) |statement| {
                    if (last_expression_result) |last_result| {
                        last_result.unref();
                    }

                    last_expression_result = try executeStatement(allocator, statement, method_activation, lobby);
                }

                if (last_expression_result) |last_result| {
                    return last_result;
                } else {
                    // If there were no statements, return nil.
                    lobby.ref();
                    return basic_primitives.Nil(allocator, lobby, &[_]Object.Ref{}, lobby);
                }
            },
        }
    } else {
        std.debug.panic("Unknown selector \"{s}\"", .{message.message_name});
    }
}

pub fn executeReturn(allocator: *Allocator, return_node: AST.ReturnNode, self_object: Object.Ref) !Object.Ref {
    _ = allocator;
    _ = return_node;
    _ = self_object;

    @panic("executeReturn has not been implemented yet!");
}

/// Executes an identifier expression. If the looked up value exists, the value
/// gains a ref. `self_object` gains a ref during the lifetime of the function.
pub fn executeIdentifier(identifier: AST.IdentifierNode, self_object: Object.Ref) !Object.Ref {
    self_object.ref();
    defer self_object.unref();

    if (try self_object.value.lookup(identifier.value)) |value| {
        value.ref();
        return value;
    } else {
        std.debug.panic("Failed looking up \"{s}\" on {any}", .{ identifier.value, self_object });
    }
}

/// Executes a string literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeString(allocator: *Allocator, string: AST.StringNode, lobby: Object.Ref) !Object.Ref {
    lobby.ref();
    defer lobby.unref();

    if (try lobby.value.lookup("traits")) |traits| {
        if (try traits.value.lookup("string")) |traits_string| {
            return Object.createCopyFromStringLiteral(allocator, string.value, traits_string);
        } else {
            @panic("Could not find string in traits");
        }
    } else {
        @panic("Could not find traits in lobby");
    }
}

/// Executes a number literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeNumber(allocator: *Allocator, number: AST.NumberNode, lobby: Object.Ref) !Object.Ref {
    lobby.ref();
    defer lobby.unref();

    if (try lobby.value.lookup("traits")) |traits| {
        if (try traits.value.lookup("number")) |traits_number| {
            return switch (number) {
                .Integer => Object.createFromIntegerLiteral(allocator, number.Integer, traits_number),
                .FloatingPoint => Object.createFromFloatingPointLiteral(allocator, number.FloatingPoint, traits_number),
            };
        } else {
            @panic("Could not find number in traits");
        }
    } else {
        @panic("Could not find traits in lobby");
    }
}
