// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../../language/ast.zig");
const Object = @import("../object.zig");
const Slot = @import("../slot.zig");
const primitives = @import("../primitives.zig");
const environment = @import("../environment.zig");

const root_interpreter = @import("../interpreter.zig");
const InterpreterContext = root_interpreter.InterpreterContext;

fn getMessageArguments(allocator: *Allocator, ast_arguments: []AST.ExpressionNode, context: *InterpreterContext) ![]Object.Ref {
    var arguments = try std.ArrayList(Object.Ref).initCapacity(allocator, ast_arguments.len);
    errdefer {
        for (arguments.items) |*argument| {
            argument.unref();
        }
        arguments.deinit();
    }

    for (ast_arguments) |argument| {
        var expression_result = try root_interpreter.executeExpression(allocator, argument, context);
        errdefer expression_result.unref();

        try arguments.append(expression_result);
    }

    return arguments.toOwnedSlice();
}

pub fn executeBlockMessage(allocator: *Allocator, receiver: Object.Ref, ast_arguments: []AST.ExpressionNode, context: *InterpreterContext) !Object.Ref {
    // Check if this method can be executed, i.e. whether its enclosing
    // activation is currently on the stack.
    var i = @intCast(isize, context.activation_stack.items.len - 1);
    var did_find_activation_in_stack = false;

    while (i >= 0) : (i -= 1) {
        var activation_object: Object.Ref = context.activation_stack.items[@intCast(usize, i)];
        std.debug.assert(activation_object.value.content == .Activation);

        if (activation_object.value.content.Activation.context == .Method and
            activation_object.value.content.Activation.activation_object.value == receiver.value.content.Block.bound_method.value)
        {
            did_find_activation_in_stack = true;
            break;
        }
    }

    if (!did_find_activation_in_stack) {
        @panic("Attempted to execute a block after its enclosing method has returned. Use objects for closures.");
    }

    const arguments = try getMessageArguments(allocator, ast_arguments, context);
    // NOTE: The block activation borrows refs from the arguments, so no need
    //       to unref them.
    defer allocator.free(arguments);

    const bound_method = receiver.value.content.Block.bound_method;
    bound_method.ref();
    context.self_object.ref();
    var block_activation = try receiver.value.activateBlock(arguments, context.self_object, bound_method);

    // Push it onto the activation stack. Borrows the ref from block_activation.
    try context.activation_stack.append(block_activation);

    const block_activation_self_object = block_activation.value.content.Activation.activation_object;
    var block_context = InterpreterContext{
        .lobby = context.lobby,
        .self_object = block_activation_self_object,
        .activation_stack = context.activation_stack,
    };

    var last_expression_result: ?Object.Ref = null;
    for (receiver.value.content.Block.statements) |statement| {
        if (last_expression_result) |last_result| {
            last_result.unref();
        }

        const expression_result = try root_interpreter.executeStatement(allocator, statement, &block_context);
        if (expression_result.value.is(.NonlocalReturn)) {
            // Looks like a non-local return is bubbling up. This cannot
            // target us, as we're a block, so let it bubble.
            last_expression_result = expression_result;
            break;
        }

        last_expression_result = expression_result;
    }

    const popped_activation = context.activation_stack.pop();
    std.debug.assert(popped_activation.value == block_activation.value);
    popped_activation.unref();

    if (last_expression_result) |last_result| {
        return last_result;
    } else {
        return environment.globalNil();
    }
}

/// `receiver` handles its own refs. `method_object` is forwarded.
pub fn executeMethodMessage(
    allocator: *Allocator,
    receiver: Object.Ref,
    method_object: Object.Ref,
    ast_arguments: []AST.ExpressionNode,
    context: *InterpreterContext,
) !Object.Ref {
    var arguments = try getMessageArguments(allocator, ast_arguments, context);
    // NOTE: The method activation borrows refs from the arguments, so no need
    //       to unref them.
    defer allocator.free(arguments);

    receiver.ref();
    const method_activation = try method_object.value.activateMethod(arguments, receiver);
    try context.activation_stack.append(method_activation);

    var last_expression_result: ?Object.Ref = null;

    const method_activation_object = method_activation.value.content.Activation.activation_object;
    var method_context = InterpreterContext{
        .lobby = context.lobby,
        .self_object = method_activation_object,
        .activation_stack = context.activation_stack,
    };
    for (method_object.value.content.Method.statements) |statement| {
        if (last_expression_result) |last_result| {
            last_result.unref();
        }

        const expression_result = try root_interpreter.executeStatement(allocator, statement, &method_context);
        if (expression_result.value.is(.NonlocalReturn)) {
            // A non-local return has bubbled up to us. If it belongs to us, we
            // can unwrap it to reach the expression inside and use it as our
            // return value; if it belongs to someone else, we just let it
            // bubble up further.
            if (expression_result.value.content.NonlocalReturn.target_method.value == method_activation_object.value) {
                const returned_value = expression_result.value.content.NonlocalReturn.value;

                returned_value.ref();
                expression_result.unref();
                last_expression_result = returned_value;
                break;
            } else {
                last_expression_result = expression_result;
                break;
            }
        }

        last_expression_result = expression_result;
    }

    var popped_activation = context.activation_stack.pop();
    std.debug.assert(popped_activation.value == method_activation.value);
    popped_activation.unref();

    if (last_expression_result) |last_result| {
        return last_result;
    } else {
        return environment.globalNil();
    }
}

/// Refs `receiver`.
pub fn executePrimitiveMessage(
    allocator: *Allocator,
    receiver: Object.Ref,
    name: []const u8,
    ast_arguments: []AST.ExpressionNode,
    context: *InterpreterContext,
) !Object.Ref {
    // All primitives borrow a ref from the caller for the receiver and
    // each argument. It is the primitive's job to unref any argument after
    // its work is done.
    if (primitives.hasPrimitive(name)) {
        var arguments = try getMessageArguments(allocator, ast_arguments, context);
        defer allocator.free(arguments);

        receiver.ref();
        return try primitives.callPrimitive(allocator, name, receiver, arguments, context);
    } else {
        std.debug.panic("Unknown primitive selector \"{s}\"\n", .{name});
    }
}

/// The original value in `slot` is unref'd.
pub fn executeAssignmentMessage(
    allocator: *Allocator,
    slot: *Slot,
    ast_argument: AST.ExpressionNode,
    context: *InterpreterContext,
) !Object.Ref {
    var argument = try root_interpreter.executeExpression(allocator, ast_argument, context);
    errdefer argument.unref();

    slot.assignNewValue(argument);

    return environment.globalNil();
}

/// Executes a message. All refs are forwarded.
pub fn executeMessage(allocator: *Allocator, message: AST.MessageNode, context: *InterpreterContext) !Object.Ref {
    var receiver = try root_interpreter.executeExpression(allocator, message.receiver, context);
    defer receiver.unref();

    // Check for assignable slots
    if (try receiver.value.getAssignableSlotForMessage(message.message_name)) |slot| {
        return try executeAssignmentMessage(allocator, slot, message.arguments[0], context);
    }

    // Primitive check
    if (message.message_name[0] == '_') {
        if (try receiver.value.findActivationReceiver()) |actual_receiver| {
            actual_receiver.ref();
            receiver.unref();
            receiver = actual_receiver;
        }

        return try executePrimitiveMessage(allocator, receiver, message.message_name, message.arguments, context);
    }

    // Check for block activation
    if (receiver.value.is(.Block) and receiver.value.isCorrectMessageForBlockExecution(message.message_name)) {
        return try executeBlockMessage(allocator, receiver, message.arguments, context);
    }

    if (try receiver.value.lookup(message.message_name, .Value)) |lookup_result| {
        switch (lookup_result.value.content) {
            .Integer, .FloatingPoint, .ByteVector, .Slots, .Empty, .Block => {
                lookup_result.ref();
                return lookup_result;
            },

            .Method => {
                return try executeMethodMessage(allocator, receiver, lookup_result, message.arguments, context);
            },

            else => unreachable,
        }
    } else {
        std.debug.panic("Unknown selector \"{s}\"", .{message.message_name});
    }
}
