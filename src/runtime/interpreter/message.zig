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
const runtime_error = @import("../error.zig");

const root_interpreter = @import("../interpreter.zig");
const InterpreterContext = root_interpreter.InterpreterContext;

fn getMessageArguments(allocator: *Allocator, ast_arguments: []AST.ExpressionNode, context: *InterpreterContext) root_interpreter.InterpreterError![]Object.Ref {
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

pub fn executeBlockMessage(allocator: *Allocator, receiver: Object.Ref, arguments: []Object.Ref, context: *InterpreterContext) root_interpreter.InterpreterError!Object.Ref {
    // Check if this method can be executed, i.e. whether its enclosing
    // activation is currently on the stack.
    var i = @intCast(isize, context.activation_stack.items.len - 1);
    var did_find_activation_in_stack = false;
    var bound_method: Object.Ref = undefined;

    if (receiver.value.content.Block.bound_method.getPointer()) |bound_method_ptr| {
        bound_method = .{ .value = bound_method_ptr };

        while (i >= 0) : (i -= 1) {
            var activation_object: Object.Ref = context.activation_stack.items[@intCast(usize, i)];
            std.debug.assert(activation_object.value.content == .Activation);

            if (activation_object.value.content.Activation.context == .Method and
                activation_object.value.content.Activation.activation_object.value == bound_method_ptr)
            {
                did_find_activation_in_stack = true;
                break;
            }
        }
    }

    if (!did_find_activation_in_stack) {
        return runtime_error.raiseError(allocator, context, "Attempted to execute a block after its enclosing method has returned. Use objects for closures.", .{});
    }

    var block_activation = try receiver.value.activateBlock(context, arguments, bound_method);

    // Push it onto the activation stack. Borrows the ref from block_activation.
    try context.activation_stack.append(block_activation);

    const block_activation_self_object = block_activation.value.content.Activation.activation_object;
    var block_context = InterpreterContext{
        .lobby = context.lobby,
        .self_object = block_activation_self_object,
        .activation_stack = context.activation_stack,
        .script = context.script,
        .current_error = null,
    };

    var last_expression_result: ?Object.Ref = null;
    for (receiver.value.content.Block.statements) |statement| {
        if (last_expression_result) |last_result| {
            last_result.unref();
        }

        const expression_result = root_interpreter.executeStatement(allocator, statement, &block_context) catch |err| {
            switch (err) {
                runtime_error.SelfRuntimeError.RuntimeError => {
                    // Pass the error message up the script chain.
                    context.current_error = block_context.current_error;
                    // Allow the error to keep bubbling up.
                    return err;
                },
                else => return err,
            }
        };
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
    arguments: []Object.Ref,
    context: *InterpreterContext,
) !Object.Ref {
    const method_activation = try method_object.value.activateMethod(context, arguments, receiver);
    try context.activation_stack.append(method_activation);

    var last_expression_result: ?Object.Ref = null;

    const method_activation_object = method_activation.value.content.Activation.activation_object;
    var method_context = InterpreterContext{
        .lobby = context.lobby,
        .self_object = method_activation_object,
        .activation_stack = context.activation_stack,
        .script = context.script,
        .current_error = null,
    };
    for (method_object.value.content.Method.statements) |statement| {
        if (last_expression_result) |last_result| {
            last_result.unref();
        }

        const expression_result = root_interpreter.executeStatement(allocator, statement, &method_context) catch |err| {
            switch (err) {
                runtime_error.SelfRuntimeError.RuntimeError => {
                    // Pass the error message up the script chain.
                    context.current_error = method_context.current_error;
                    // Allow the error to keep bubbling up.
                    return err;
                },
                else => return err,
            }
        };
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
    arguments: []Object.Ref,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Object.Ref {
    // All primitives borrow a ref from the caller for the receiver and
    // each argument. It is the primitive's job to unref any argument after
    // its work is done.
    if (primitives.hasPrimitive(name)) {
        receiver.ref();
        return try primitives.callPrimitive(allocator, name, receiver, arguments, context);
    } else {
        return runtime_error.raiseError(allocator, context, "Unknown primitive selector \"{s}\"", .{name});
    }
}

/// The original value in `slot` is unref'd.
pub fn executeAssignmentMessage(
    allocator: *Allocator,
    slot: *Slot,
    ast_argument: AST.ExpressionNode,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Object.Ref {
    var argument = try root_interpreter.executeExpression(allocator, ast_argument, context);
    errdefer argument.unref();

    slot.assignNewValue(argument);

    return environment.globalNil();
}

/// Executes a message. All refs are forwarded.
pub fn executeMessage(allocator: *Allocator, message: AST.MessageNode, context: *InterpreterContext) root_interpreter.InterpreterError!Object.Ref {
    var receiver = try root_interpreter.executeExpression(allocator, message.receiver, context);
    defer receiver.unref();

    // Check for assignable slots
    if (try receiver.value.getAssignableSlotForMessage(message.message_name)) |slot| {
        return try executeAssignmentMessage(allocator, slot, message.arguments[0], context);
    }

    // Primitive check
    if (message.message_name[0] == '_') {
        const arguments = try getMessageArguments(allocator, message.arguments, context);
        // NOTE: The activations borrow refs from the arguments, so no need to unref
        //       them.
        defer allocator.free(arguments);

        if (try receiver.value.findActivationReceiver(context)) |actual_receiver| {
            actual_receiver.ref();
            receiver.unref();
            receiver = actual_receiver;
        }

        return try executePrimitiveMessage(allocator, receiver, message.message_name, arguments, context);
    }

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    {
        var block_receiver = receiver;
        if (try block_receiver.value.findActivationReceiver(context)) |actual_receiver| {
            actual_receiver.ref();
            block_receiver.unref();
            block_receiver = actual_receiver;
        }

        if (block_receiver.value.is(.Block) and block_receiver.value.isCorrectMessageForBlockExecution(message.message_name)) {
            const arguments = try getMessageArguments(allocator, message.arguments, context);
            defer allocator.free(arguments);

            return try executeBlockMessage(allocator, block_receiver, arguments, context);
        }
    }

    if (try receiver.value.lookup(context, message.message_name, .Value)) |lookup_result| {
        switch (lookup_result.value.content) {
            .Integer, .FloatingPoint, .ByteVector, .Slots, .Empty, .Block => {
                lookup_result.ref();
                return lookup_result;
            },

            .Method => {
                const arguments = try getMessageArguments(allocator, message.arguments, context);
                defer allocator.free(arguments);

                return try executeMethodMessage(allocator, receiver, lookup_result, arguments, context);
            },

            else => unreachable,
        }
    } else {
        return runtime_error.raiseError(allocator, context, "Unknown selector \"{s}\"", .{message.message_name});
    }
}
