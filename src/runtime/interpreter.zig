// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");
const Object = @import("./object.zig");
const Slot = @import("./slot.zig");

const message_interpreter = @import("./interpreter/message.zig");

pub const InterpreterContext = struct {
    /// The object that is the current context. The identifier "self" will
    /// resolve to this object.
    self_object: Object.Ref,
    /// The root of the current Self world.
    lobby: Object.Ref,
    /// The method/block activation stack. This is used with blocks in order to
    /// verify that the block is executed within its enclosing method, and in
    /// order to select the correct non-local return target; the block's "bound
    /// method" (the method activation in which the block object was
    /// instantiated) becomes the target of the non-local return. (The parser
    /// ensures that a non-local return is always the last statement directly
    /// under a block.) When the activation completes, the activation object is
    /// popped; when a new activation occurs, it is pushed. Pushed objects must
    /// be pushed with the assumption that 1 ref is borrowed by this stack.
    activation_stack: *std.ArrayList(Object.Ref),
};

/// Executes a script node. `lobby` is ref'd for the function lifetime. The last
/// expression result is returned, or if no statements were available, null is
/// returned.
pub fn executeScript(allocator: *Allocator, script: AST.ScriptNode, lobby: Object.Ref) !?Object.Ref {
    lobby.ref();
    defer lobby.unref();

    var last_expression_result: ?Object.Ref = null;
    var activation_stack = std.ArrayList(Object.Ref).init(allocator);
    defer activation_stack.deinit();

    const context = InterpreterContext{
        .self_object = lobby,
        .lobby = lobby,
        .activation_stack = &activation_stack,
    };
    for (script.statements) |statement| {
        std.debug.assert(activation_stack.items.len == 0);

        if (last_expression_result) |*result| {
            result.unref();
        }

        const expression_result = try executeStatement(allocator, statement, context);
        if (expression_result.value.is(.NonlocalReturn)) {
            @panic("FIXME handle situation where a non-local return reaches top level");
        }
        last_expression_result = expression_result;
    }

    return last_expression_result;
}

/// Executes a statement. All refs are forwardded.
pub fn executeStatement(allocator: *Allocator, statement: AST.StatementNode, context: InterpreterContext) !Object.Ref {
    return try executeExpression(allocator, statement.expression, context);
}

/// Executes an expression. All refs are forwarded.
pub fn executeExpression(allocator: *Allocator, expression: AST.ExpressionNode, context: InterpreterContext) Allocator.Error!Object.Ref {
    return switch (expression) {
        .Object => |object| try executeObject(allocator, object.*, context),
        .Block => |block| try executeBlock(allocator, block.*, context),
        .Message => |message| try message_interpreter.executeMessage(allocator, message.*, context),
        .Return => |return_node| try executeReturn(allocator, return_node.*, context),

        .Identifier => |identifier| try executeIdentifier(allocator, identifier, context),
        .String => |string| try executeString(allocator, string, context),
        .Number => |number| try executeNumber(allocator, number, context),
    };
}

/// Creates a new method object. All refs are forwarded. `arguments` and
/// `object_node`'s statements are copied.
fn executeMethod(allocator: *Allocator, object_node: AST.ObjectNode, arguments: [][]const u8, context: InterpreterContext) !Object.Ref {
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
        var slot_copy = try executeSlot(allocator, slot, context);
        errdefer slot_copy.deinit();

        // TODO nonlocals

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
pub fn executeSlot(allocator: *Allocator, slot_node: AST.SlotNode, context: InterpreterContext) Allocator.Error!Slot {
    var value = blk: {
        if (slot_node.value == .Object and slot_node.value.Object.statements.len > 0) {
            break :blk try executeMethod(allocator, slot_node.value.Object.*, slot_node.arguments, context);
        } else {
            break :blk try executeExpression(allocator, slot_node.value, context);
        }
    };
    errdefer value.unref();

    return try Slot.init(allocator, slot_node.is_mutable, slot_node.is_parent, slot_node.name, value);
}

/// Creates a new slots object. All refs are forwarded.
pub fn executeObject(allocator: *Allocator, object_node: AST.ObjectNode, context: InterpreterContext) !Object.Ref {
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
        var slot = try executeSlot(allocator, slot_node.*, context);
        errdefer slot.deinit();

        // TODO nonlocals

        try slots.append(slot);
    }

    return try Object.createSlots(allocator, slots.toOwnedSlice());
}

pub fn executeBlock(allocator: *Allocator, block: AST.BlockNode, context: InterpreterContext) !Object.Ref {
    var arguments = try std.ArrayList([]const u8).initCapacity(allocator, block.slots.len);
    errdefer {
        for (arguments.items) |argument| {
            allocator.free(argument);
        }
        arguments.deinit();
    }

    var slots = try std.ArrayList(Slot).initCapacity(allocator, block.slots.len);
    errdefer {
        for (slots.items) |*slot| {
            slot.deinit();
        }
        slots.deinit();
    }

    var statements = try std.ArrayList(AST.StatementNode).initCapacity(allocator, block.statements.len);
    errdefer {
        for (statements.items) |*statement| {
            statement.deinit(allocator);
        }
        statements.deinit();
    }

    for (block.slots) |slot_node| {
        if (slot_node.is_argument) {
            var argument_copy = try allocator.dupe(u8, slot_node.name);
            errdefer allocator.free(argument_copy);

            try arguments.append(argument_copy);
        } else {
            var slot_copy = try executeSlot(allocator, slot_node, context);
            errdefer slot_copy.deinit();

            // TODO nonlocals

            try slots.append(slot_copy);
        }
    }

    for (block.statements) |statement| {
        var statement_copy = try ASTCopyVisitor.visitStatement(statement, allocator);
        errdefer statement_copy.deinit(allocator);

        try statements.append(statement_copy);
    }

    var latest_activation: Object.Ref = context.activation_stack.items[context.activation_stack.items.len - 1];
    const bound_method = latest_activation.value.getBoundMethodForActivation();
    bound_method.ref();
    errdefer bound_method.unref();

    return try Object.createBlock(allocator, arguments.toOwnedSlice(), slots.toOwnedSlice(), statements.toOwnedSlice(), bound_method);
}

pub fn executeReturn(allocator: *Allocator, return_node: AST.ReturnNode, context: InterpreterContext) !Object.Ref {
    const value = try executeExpression(allocator, return_node.expression, context);
    errdefer value.unref();

    const latest_activation: Object.Ref = context.activation_stack.items[context.activation_stack.items.len - 1];
    const activation_target_method = latest_activation.value.getBoundMethodForActivation();
    activation_target_method.ref();
    errdefer activation_target_method.unref();

    return try Object.createNonlocalReturn(allocator, activation_target_method, value);
}

/// Executes an identifier expression. If the looked up value exists, the value
/// gains a ref. `self_object` gains a ref during a method execution.
pub fn executeIdentifier(allocator: *Allocator, identifier: AST.IdentifierNode, context: InterpreterContext) !Object.Ref {
    if (identifier.value[0] == '_') {
        return try message_interpreter.executePrimitiveMessage(allocator, context.self_object, identifier.value, &[_]AST.ExpressionNode{}, context);
    }

    if (try context.self_object.value.lookup(identifier.value)) |value| {
        switch (value.value.content) {
            .Integer, .FloatingPoint, .ByteVector, .Slots, .Empty, .Block => {
                value.ref();
                return value;
            },

            .Method => {
                return try message_interpreter.executeMethodMessage(allocator, context.self_object, value, &[_]AST.ExpressionNode{}, context);
            },

            else => unreachable,
        }
    } else {
        std.debug.panic("Failed looking up \"{s}\" on {any}", .{ identifier.value, context.self_object });
    }
}

/// Executes a string literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeString(allocator: *Allocator, string: AST.StringNode, context: InterpreterContext) !Object.Ref {
    context.lobby.ref();
    defer context.lobby.unref();

    if (try context.lobby.value.lookup("traits")) |traits| {
        if (try traits.value.lookup("string")) |traits_string| {
            traits_string.ref();

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
pub fn executeNumber(allocator: *Allocator, number: AST.NumberNode, context: InterpreterContext) !Object.Ref {
    context.lobby.ref();
    defer context.lobby.unref();

    if (try context.lobby.value.lookup("traits")) |traits| {
        if (try traits.value.lookup("number")) |traits_number| {
            traits_number.ref();

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
