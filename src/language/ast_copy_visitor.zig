// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("./ast.zig");

pub fn visitScript(script: AST.ScriptNode, allocator: *Allocator) !AST.ScriptNode {
    const statements = try std.ArrayList(AST.StatementNode).initCapacity(allocator, script.statements.len);
    errdefer {
        for (statements.items) |*statement| {
            statement.deinit(allocator);
        }
        statements.deinit();
    }

    for (script.statements.items) |statement| {
        try statements.append(try visitStatement(statement, allocator));
    }

    return AST.ScriptNode{ .statements = statements.toOwnedSlice() };
}

pub fn visitStatement(statement: AST.StatementNode, allocator: *Allocator) !AST.StatementNode {
    return AST.StatementNode{ .expression = try visitExpression(statement.expression, allocator) };
}

pub fn visitExpression(expression: AST.ExpressionNode, allocator: *Allocator) Allocator.Error!AST.ExpressionNode {
    return switch (expression) {
        .Object => AST.ExpressionNode{ .Object = try visitObject(expression.Object.*, allocator) },
        .Block => AST.ExpressionNode{ .Block = try visitBlock(expression.Block.*, allocator) },
        .Message => AST.ExpressionNode{ .Message = try visitMessage(expression.Message.*, allocator) },
        .Return => AST.ExpressionNode{ .Return = try visitReturn(expression.Return.*, allocator) },

        .Identifier => AST.ExpressionNode{ .Identifier = try visitIdentifier(expression.Identifier, allocator) },
        .String => AST.ExpressionNode{ .String = try visitString(expression.String, allocator) },
        .Number => AST.ExpressionNode{ .Number = visitNumber(expression.Number, allocator) },
    };
}

pub fn visitSlot(slot: AST.SlotNode, allocator: *Allocator) !AST.SlotNode {
    var arguments = try std.ArrayList([]const u8).initCapacity(allocator, slot.arguments.len);
    errdefer {
        for (arguments.items) |argument| {
            allocator.free(argument);
        }
        arguments.deinit();
    }

    for (slot.arguments) |argument| {
        try arguments.append(try allocator.dupe(u8, argument));
    }

    return AST.SlotNode{
        .is_mutable = slot.is_mutable,
        .is_parent = slot.is_parent,
        .is_argument = slot.is_argument,

        .name = try allocator.dupe(u8, slot.name),
        .arguments = arguments.toOwnedSlice(),
        .value = try visitExpression(slot.value, allocator),
    };
}

fn visitSlotsStatementsCommon(
    slots: []AST.SlotNode,
    statements: []AST.StatementNode,
    out_slots: *[]AST.SlotNode,
    out_statements: *[]AST.StatementNode,
    allocator: *Allocator,
) !void {
    var slots_copy = try std.ArrayList(AST.SlotNode).initCapacity(allocator, slots.len);
    errdefer {
        for (slots_copy.items) |*slot| {
            slot.deinit(allocator);
        }
        slots_copy.deinit();
    }

    var statements_copy = try std.ArrayList(AST.StatementNode).initCapacity(allocator, statements.len);
    errdefer {
        for (statements_copy.items) |*statement| {
            statement.deinit(allocator);
        }
        statements_copy.deinit();
    }

    for (slots) |slot| {
        try slots_copy.append(try visitSlot(slot, allocator));
    }

    for (statements) |statement| {
        try statements_copy.append(try visitStatement(statement, allocator));
    }

    out_slots.* = slots_copy.toOwnedSlice();
    out_statements.* = statements_copy.toOwnedSlice();
}

pub fn visitObject(object: AST.ObjectNode, allocator: *Allocator) !*AST.ObjectNode {
    var slots: []AST.SlotNode = undefined;
    var statements: []AST.StatementNode = undefined;

    var object_copy = try allocator.create(AST.ObjectNode);
    errdefer allocator.destroy(object_copy);

    try visitSlotsStatementsCommon(object.slots, object.statements, &slots, &statements, allocator);
    object_copy.slots = slots;
    object_copy.statements = statements;

    return object_copy;
}

pub fn visitBlock(block: AST.BlockNode, allocator: *Allocator) !*AST.BlockNode {
    var slots: []AST.SlotNode = undefined;
    var statements: []AST.StatementNode = undefined;

    var block_copy = try allocator.create(AST.BlockNode);
    errdefer allocator.destroy(block_copy);

    try visitSlotsStatementsCommon(block.slots, block.statements, &slots, &statements, allocator);
    block_copy.slots = slots;
    block_copy.statements = statements;

    return block_copy;
}

pub fn visitMessage(message: AST.MessageNode, allocator: *Allocator) !*AST.MessageNode {
    var arguments = try std.ArrayList(AST.ExpressionNode).initCapacity(allocator, message.arguments.len);
    errdefer {
        for (arguments.items) |*argument| {
            argument.deinit(allocator);
        }
        arguments.deinit();
    }

    for (message.arguments) |argument| {
        try arguments.append(try visitExpression(argument, allocator));
    }

    var receiver_copy = try visitExpression(message.receiver, allocator);
    errdefer receiver_copy.deinit(allocator);

    var message_name_copy = try allocator.dupe(u8, message.message_name);
    errdefer allocator.free(message_name_copy);

    var message_copy = try allocator.create(AST.MessageNode);
    message_copy.receiver = receiver_copy;
    message_copy.message_name = message_name_copy;
    message_copy.arguments = arguments.toOwnedSlice();

    return message_copy;
}

pub fn visitReturn(return_node: AST.ReturnNode, allocator: *Allocator) !*AST.ReturnNode {
    var return_copy = try allocator.create(AST.ReturnNode);
    errdefer allocator.destroy(return_copy);
    return_copy.expression = try visitExpression(return_node.expression, allocator);

    return return_copy;
}

pub fn visitIdentifier(identifier: AST.IdentifierNode, allocator: *Allocator) !AST.IdentifierNode {
    return AST.IdentifierNode{ .value = try allocator.dupe(u8, identifier.value) };
}

pub fn visitString(string: AST.StringNode, allocator: *Allocator) !AST.StringNode {
    return AST.StringNode{ .value = try allocator.dupe(u8, string.value) };
}

pub fn visitNumber(number: AST.NumberNode, allocator: *Allocator) AST.NumberNode {
    _ = allocator;
    return number;
}
