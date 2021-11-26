// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const Slot = @import("./slot.zig");
const Object = @import("./object.zig");
const Script = @import("../language/script.zig");
const Activation = @import("./activation.zig");
const runtime_error = @import("./error.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");

const message_interpreter = @import("./interpreter/message.zig");

pub const InterpreterContext = struct {
    /// The object that is the current context. The identifier "self" will
    /// resolve to this object.
    self_object: Object.Ref,
    /// The root of the current Self world.
    lobby: Object.Ref,
    /// The method/block activation stack. This is used with blocks in order to
    /// verify that the block is executed within its enclosing method and for
    /// stack traces. When the activation completes, the activation object is
    /// popped; when a new activation occurs, it is pushed. Pushed objects must
    /// be pushed with the assumption that 1 ref is borrowed by this stack.
    activation_stack: *std.ArrayList(*Activation),
    /// The script file that is currently executing, used to resolve the
    /// relative paths of other script files.
    script: Script.Ref,
    /// The current error message value. executeScript catches this and displays
    /// the error with a stack trace. The user must free it.
    current_error: ?[]const u8,
    /// The current non-local return value. Should *NOT* rise to executeScript.
    current_nonlocal_return: ?struct {
        /// The activation at which this non-local return should become the
        /// regular return value.
        target_activation: Activation.Weak,
        /// The value that should be returned when the non-local return reaches
        /// its destination.
        value: Object.Ref,
    },
};

// FIXME: These aren't very nice. Collect them into a single place.
pub const NonlocalReturnError = error{NonlocalReturn};
pub const InterpreterError = Allocator.Error || runtime_error.SelfRuntimeError || NonlocalReturnError;

/// Executes a script node. `lobby` is ref'd for the function lifetime. The last
/// expression result is returned, or if no statements were available, null is
/// returned.
///
/// Borrows a ref for `script` from the caller.
pub fn executeScript(allocator: *Allocator, script: Script.Ref, lobby: Object.Ref) InterpreterError!?Object.Ref {
    defer script.unref();

    lobby.ref();
    defer lobby.unref();

    var last_expression_result: ?Object.Ref = null;
    var activation_stack = std.ArrayList(*Activation).init(allocator);
    defer activation_stack.deinit();
    errdefer {
        for (activation_stack.items) |activation| {
            activation.destroy();
        }
    }

    var context = InterpreterContext{
        .self_object = lobby,
        .lobby = lobby,
        .activation_stack = &activation_stack,
        .script = script,
        .current_error = null,
        .current_nonlocal_return = null,
    };
    for (script.value.ast_root.?.statements) |statement| {
        std.debug.assert(activation_stack.items.len == 0);

        if (last_expression_result) |*result| {
            result.unref();
        }

        const expression_result = executeStatement(allocator, statement, &context) catch |err| {
            switch (err) {
                runtime_error.SelfRuntimeError.RuntimeError => {
                    var error_message = context.current_error.?;
                    defer allocator.free(error_message);

                    std.debug.print("Received error at top level: {s}\n", .{error_message});
                    runtime_error.printTraceFromActivationStack(activation_stack.items);

                    // Since the execution was abruptly stopped the activation
                    // stack wasn't properly unwound, so let's do that now.
                    for (activation_stack.items) |activation| {
                        activation.destroy();
                    }

                    return null;
                },
                NonlocalReturnError.NonlocalReturn => {
                    std.debug.print("A non-local return has bubbled up to the top! This is likely a bug!", .{});
                    runtime_error.printTraceFromActivationStack(activation_stack.items);
                    context.current_nonlocal_return.?.value.unref();

                    // Since the execution was abruptly stopped the activation
                    // stack wasn't properly unwound, so let's do that now.
                    for (activation_stack.items) |activation| {
                        activation.destroy();
                    }

                    return null;
                },
                else => return err,
            }
        };

        last_expression_result = expression_result;
    }

    return last_expression_result;
}

/// Execute a script object as a child script of the root script. The root
/// interpreter context is passed in order to preserve the activation stack and
/// various other context objects.
///
/// Borrows a ref for `script` from the caller.
pub fn executeSubScript(allocator: *Allocator, script: Script.Ref, parent_context: *InterpreterContext) InterpreterError!?Object.Ref {
    defer script.unref();

    parent_context.lobby.ref();
    defer parent_context.lobby.unref();

    var last_expression_result: ?Object.Ref = null;

    var child_context = InterpreterContext{
        .self_object = parent_context.lobby,
        .lobby = parent_context.lobby,
        .activation_stack = parent_context.activation_stack,
        .script = script,
        .current_error = null,
        .current_nonlocal_return = null,
    };
    for (script.value.ast_root.?.statements) |statement| {
        if (last_expression_result) |*result| {
            result.unref();
        }

        const expression_result = executeStatement(allocator, statement, &child_context) catch |err| {
            switch (err) {
                runtime_error.SelfRuntimeError.RuntimeError => {
                    // Pass the error message up the script chain.
                    parent_context.current_error = child_context.current_error;
                    // Allow the error to keep bubbling up.
                    return err;
                },
                NonlocalReturnError.NonlocalReturn => {
                    return runtime_error.raiseError(allocator, parent_context, "A non-local return has bubbled up to the top of a sub-script! This is likely a bug!", .{});
                },
                else => return err,
            }
        };
        last_expression_result = expression_result;
    }

    return last_expression_result;
}

/// Executes a statement. All refs are forwardded.
pub fn executeStatement(allocator: *Allocator, statement: AST.StatementNode, context: *InterpreterContext) InterpreterError!Object.Ref {
    return try executeExpression(allocator, statement.expression, context);
}

/// Executes an expression. All refs are forwarded.
pub fn executeExpression(allocator: *Allocator, expression: AST.ExpressionNode, context: *InterpreterContext) InterpreterError!Object.Ref {
    return switch (expression) {
        .Object => |object| try executeObject(allocator, object.*, context),
        .Block => |block| try executeBlock(allocator, block.*, context),
        .Message => |message| try message_interpreter.executeMessage(allocator, message.*, context),
        .Return => |return_node| return executeReturn(allocator, return_node.*, context),

        .Identifier => |identifier| try executeIdentifier(allocator, identifier, context),
        .String => |string| try executeString(allocator, string, context),
        .Number => |number| try executeNumber(allocator, number, context),
    };
}

/// Creates a new method object. All refs are forwarded. `arguments` and
/// `object_node`'s statements are copied.
fn executeMethod(allocator: *Allocator, name: []const u8, object_node: AST.ObjectNode, arguments: [][]const u8, context: *InterpreterContext) InterpreterError!Object.Ref {
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

    context.script.ref();
    errdefer context.script.unref();
    return try Object.createMethod(allocator, name, arguments_copy.toOwnedSlice(), slots.toOwnedSlice(), statements.toOwnedSlice(), context.script);
}

/// Creates a new slot. All refs are forwarded.
pub fn executeSlot(allocator: *Allocator, slot_node: AST.SlotNode, context: *InterpreterContext) InterpreterError!Slot {
    var value = blk: {
        if (slot_node.value == .Object and slot_node.value.Object.statements.len > 0) {
            break :blk try executeMethod(allocator, slot_node.name, slot_node.value.Object.*, slot_node.arguments, context);
        } else {
            break :blk try executeExpression(allocator, slot_node.value, context);
        }
    };
    errdefer value.unref();

    return try Slot.init(allocator, slot_node.is_mutable, slot_node.is_parent, slot_node.name, value);
}

/// Creates a new slots object. All refs are forwarded.
pub fn executeObject(allocator: *Allocator, object_node: AST.ObjectNode, context: *InterpreterContext) InterpreterError!Object.Ref {
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

pub fn executeBlock(allocator: *Allocator, block: AST.BlockNode, context: *InterpreterContext) InterpreterError!Object.Ref {
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

    // The latest activation is where the block was created, so it will always
    // be the parent activation (i.e., where we look for parent blocks' and the
    // method's slots).
    const parent_activation = context.activation_stack.items[context.activation_stack.items.len - 1];
    // However, we want the _method_ as the non-local return target; because the
    // non-local return can only be returned by the method in which the block
    // making the non-local return was defined, this needs to be separate from
    // parent_activation. If the parent activation is a block, it will also
    // contain a target activation; if it's a method the target activation _is_
    // the parent.
    const nonlocal_return_target_activation = if (parent_activation.nonlocal_return_target_activation) |target| target else parent_activation;
    std.debug.assert(nonlocal_return_target_activation.nonlocal_return_target_activation == null);

    context.script.ref();
    errdefer context.script.unref();
    return try Object.createBlock(
        allocator,
        arguments.toOwnedSlice(),
        slots.toOwnedSlice(),
        statements.toOwnedSlice(),
        parent_activation,
        nonlocal_return_target_activation,
        context.script,
    );
}

pub fn executeReturn(allocator: *Allocator, return_node: AST.ReturnNode, context: *InterpreterContext) InterpreterError {
    const latest_activation = context.activation_stack.items[context.activation_stack.items.len - 1];
    const target_activation = latest_activation.nonlocal_return_target_activation.?;
    std.debug.assert(target_activation.nonlocal_return_target_activation == null);

    {
        const value = try executeExpression(allocator, return_node.expression, context);
        errdefer value.unref();

        const target_activation_weak = target_activation.makeWeakRef();
        context.current_nonlocal_return = .{ .target_activation = target_activation_weak, .value = value };
    }

    return NonlocalReturnError.NonlocalReturn;
}

/// Executes an identifier expression. If the looked up value exists, the value
/// gains a ref. `self_object` gains a ref during a method execution.
pub fn executeIdentifier(allocator: *Allocator, identifier: AST.IdentifierNode, context: *InterpreterContext) InterpreterError!Object.Ref {
    if (identifier.value[0] == '_') {
        var receiver = context.self_object;

        if (try receiver.value.findActivationReceiver()) |actual_receiver| {
            receiver = actual_receiver;
        }

        return try message_interpreter.executePrimitiveMessage(allocator, identifier.range, receiver, identifier.value, &[_]Object.Ref{}, context);
    }

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    {
        var receiver = context.self_object;
        if (try receiver.value.findActivationReceiver()) |actual_receiver| {
            receiver = actual_receiver;
        }

        if (receiver.value.is(.Block) and receiver.value.isCorrectMessageForBlockExecution(identifier.value)) {
            return try message_interpreter.executeBlockMessage(allocator, identifier.range, receiver, &[_]Object.Ref{}, context);
        }
    }

    if (try context.self_object.value.lookup(context, identifier.value, .Value)) |value| {
        switch (value.value.content) {
            .Integer, .FloatingPoint, .ByteVector, .Vector, .Slots, .Empty, .Block, .Activation => {
                value.ref();
                return value;
            },

            .Method => {
                return try message_interpreter.executeMethodMessage(allocator, identifier.range, context.self_object, value, &[_]Object.Ref{}, context);
            },
        }
    } else {
        return runtime_error.raiseError(allocator, context, "Failed looking up \"{s}\"", .{identifier.value});
    }
}

/// Executes a string literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeString(allocator: *Allocator, string: AST.StringNode, context: *InterpreterContext) InterpreterError!Object.Ref {
    _ = context;

    return Object.createCopyFromStringLiteral(allocator, string.value);
}

/// Executes a number literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeNumber(allocator: *Allocator, number: AST.NumberNode, context: *InterpreterContext) InterpreterError!Object.Ref {
    _ = context;

    return switch (number.value) {
        .Integer => Object.createFromIntegerLiteral(allocator, number.value.Integer),
        .FloatingPoint => Object.createFromFloatingPointLiteral(allocator, number.value.FloatingPoint),
    };
}
