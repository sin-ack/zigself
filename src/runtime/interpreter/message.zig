// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../../language/ast.zig");
const Slot = @import("../slot.zig").Slot;
const Heap = @import("../heap.zig");
const Range = @import("../../language/location_range.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Activation = @import("../activation.zig");
const primitives = @import("../primitives.zig");
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");

const root_interpreter = @import("../interpreter.zig");
const InterpreterContext = root_interpreter.InterpreterContext;

const MaximumStackDepth = 2048;

fn getMessageArguments(allocator: *Allocator, ast_arguments: []AST.ExpressionNode, context: *InterpreterContext) root_interpreter.InterpreterError![]Value {
    var arguments = try std.ArrayList(Value).initCapacity(allocator, ast_arguments.len);
    errdefer {
        for (arguments.items) |*argument| {
            argument.unrefWithAllocator(allocator);
        }
        arguments.deinit();
    }

    for (ast_arguments) |argument| {
        var expression_result = try root_interpreter.executeExpression(allocator, argument, context);
        errdefer expression_result.unrefWithAllocator(allocator);

        try arguments.append(expression_result);
    }

    return arguments.toOwnedSlice();
}

pub fn executeBlockMessage(
    allocator: *Allocator,
    message_range: Range,
    block_object: Value,
    arguments: []Value,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Value {
    var did_find_activation_in_stack = false;
    var parent_activation: *Activation = undefined;

    // Check if this method can be executed, i.e. whether its enclosing
    // activation is currently on the stack.
    if (block_object.value.content.Block.parent_activation.getPointer()) |parent_activation_ptr| {
        parent_activation = parent_activation_ptr;

        var i = @intCast(isize, context.activation_stack.items.len - 1);
        while (i >= 0) : (i -= 1) {
            const activation = context.activation_stack.items[@intCast(usize, i)];

            if (activation == parent_activation) {
                did_find_activation_in_stack = true;
                break;
            }
        }
    }

    if (!did_find_activation_in_stack) {
        return runtime_error.raiseError(allocator, context, "Attempted to execute a block after its enclosing method has returned. Use objects for closures.", .{});
    }

    if (context.activation_stack.items.len >= MaximumStackDepth) {
        return runtime_error.raiseError(allocator, context, "Maximum stack size reached", .{});
    }

    // This is done so that we never hold an Activation in value form, and
    // always refer to the in-place version in the activation stack.
    const block_activation = try block_object.value.activateBlock(allocator, context, message_range, arguments, parent_activation.activation_object);
    var did_execute_normally = false;

    {
        errdefer block_activation.destroy();
        try context.activation_stack.append(block_activation);
    }

    // NOTE: We shouldn't pop from the activation stack if we didn't execute
    //       normally and didn't non-local return.
    var did_nonlocal_return = false;
    defer {
        if (did_execute_normally or did_nonlocal_return) {
            const popped_activation = context.activation_stack.pop();
            std.debug.assert(popped_activation == block_activation);
            popped_activation.destroy();
        }
    }

    const previous_script = context.script;
    const previous_self_object = context.self_object;

    const block_script = block_object.value.content.Block.script;
    const block_activation_object = block_activation.activation_object;
    context.script = block_script;
    context.self_object = block_activation_object;

    // NOTE: We don't care about this if an error is bubbling up.
    defer {
        if (did_execute_normally) {
            context.script = previous_script;
            context.self_object = previous_self_object;
        }
    }

    var last_expression_result: ?Value = null;
    for (block_object.value.content.Block.statements) |statement| {
        if (last_expression_result) |last_result| {
            last_result.unrefWithAllocator(allocator);
        }

        last_expression_result = root_interpreter.executeStatement(allocator, statement, context) catch |err|
            switch (err) {
            root_interpreter.NonlocalReturnError.NonlocalReturn => |e| {
                did_nonlocal_return = true;
                return e;
            },
            else => |e| return e,
        };
    }

    did_execute_normally = true;

    if (last_expression_result) |last_result| {
        return last_result;
    } else {
        return environment.globalNil();
    }
}

/// `receiver` handles its own refs. `method_object` is forwarded.
pub fn executeMethodMessage(
    allocator: *Allocator,
    message_range: Range,
    receiver: Value,
    method_object: Value,
    arguments: []Value,
    context: *InterpreterContext,
) !Value {
    if (context.activation_stack.items.len >= MaximumStackDepth) {
        return runtime_error.raiseError(allocator, context, "Maximum stack size reached", .{});
    }

    // NOTE: This is done so that we never hold an Activation in value form, and
    //       always refer to the in-place version in the activation stack.
    const method_activation = try method_object.value.activateMethod(allocator, context, message_range, arguments, receiver);
    var did_execute_normally = false;

    {
        errdefer method_activation.destroy();
        try context.activation_stack.append(method_activation);
    }

    // NOTE: We shouldn't pop from the activation stack if we didn't execute
    //       normally and didn't non-local return.
    var did_nonlocal_return = false;
    defer {
        if (did_execute_normally or did_nonlocal_return) {
            const popped_activation = context.activation_stack.pop();
            std.debug.assert(popped_activation == method_activation);
            popped_activation.destroy();
        }
    }

    const previous_script = context.script;
    const previous_self_object = context.self_object;

    const method_script = method_object.value.content.Method.script;
    const method_activation_object = method_activation.activation_object;
    context.script = method_script;
    context.self_object = method_activation_object;

    // NOTE: We don't care about this if an error is bubbling up.
    defer {
        if (did_execute_normally) {
            context.script = previous_script;
            context.self_object = previous_self_object;
        }
    }

    var last_expression_result: ?Value = null;
    for (method_object.value.content.Method.statements) |statement| {
        if (last_expression_result) |last_result| {
            last_result.unrefWithAllocator(allocator);
        }

        const expression_result = root_interpreter.executeStatement(allocator, statement, context) catch |err| {
            switch (err) {
                root_interpreter.NonlocalReturnError.NonlocalReturn => {
                    if (context.current_nonlocal_return.?.target_activation.getPointer()) |target_activation| {
                        if (target_activation == method_activation) {
                            context.current_nonlocal_return.?.target_activation.deinit();
                            last_expression_result = context.current_nonlocal_return.?.value;
                            context.current_nonlocal_return = null;
                            break;
                        }
                    }

                    // The target of the non-local return wasn't us. Allow the
                    // error to keep bubbling up.
                    did_nonlocal_return = true;
                    return err;
                },
                else => return err,
            }
        };

        last_expression_result = expression_result;
    }

    did_execute_normally = true;

    if (last_expression_result) |last_result| {
        return last_result;
    } else {
        return environment.globalNil();
    }
}

/// Refs `receiver`.
pub fn executePrimitiveMessage(
    allocator: *Allocator,
    message_range: Range,
    receiver: Value,
    name: []const u8,
    arguments: []Value,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Value {
    // All primitives borrow a ref from the caller for the receiver and
    // each argument. It is the primitive's job to unref any argument after
    // its work is done.
    if (primitives.hasPrimitive(name)) {
        receiver.ref();
        return try primitives.callPrimitive(allocator, message_range, name, receiver, arguments, context);
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
) root_interpreter.InterpreterError!Value {
    var argument = try root_interpreter.executeExpression(allocator, ast_argument, context);
    errdefer argument.unrefWithAllocator(allocator);

    // NOTE: This is required, for instance, when we are assigning `self` to
    //       a slot (happens more often than you might think!). We need to strip
    //       the activation object to get to the actual value inside.
    if (try argument.value.findActivationReceiver()) |actual_argument| {
        actual_argument.ref();
        argument.unrefWithAllocator(allocator);
        argument = actual_argument;
    }

    slot.assignNewValue(allocator, argument);

    return environment.globalNil();
}

/// Executes a message. All refs are forwarded.
pub fn executeMessage(allocator: *Allocator, heap: *Heap, message: AST.MessageNode, context: *InterpreterContext) root_interpreter.InterpreterError!Value {
    // FIXME: Track the receiver, and untrack on return
    var receiver = try root_interpreter.executeExpression(allocator, heap, message.receiver, context);

    // Check for assignable slots
    if (receiver.isObjectReference()) {
        if (receiver.asObject().getAssignableSlotForMessage(message.message_name)) |slot| {
            return try executeAssignmentMessage(allocator, slot, message.arguments[0], context);
        }
    }

    // Primitive check
    if (message.message_name[0] == '_') {
        const arguments = try getMessageArguments(allocator, message.arguments, context);
        // NOTE: The activations borrow refs from the arguments, so no need to unref
        //       them.
        defer allocator.free(arguments);

        if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
            if (try receiver.asObject().asActivationObject().findActivationReceiver()) |actual_receiver| {
                // FIXME: Stop tracking receiver, and track actual receiver
                receiver = actual_receiver;
            }
        }

        return try executePrimitiveMessage(allocator, message.range, receiver, message.message_name, arguments, context);
    }

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    {
        // FIXME: Track block_receiver here
        var block_receiver = receiver;
        if (block_receiver.isObjectReference() and block_receiver.asObject().isActivationObject()) {
            if (try block_receiver.value.findActivationReceiver()) |actual_receiver| {
                // FIXME: Stop tracking block_receiver, track actual_receiver
                block_receiver = actual_receiver;
            }
        }

        if (block_receiver.value.is(.Block) and block_receiver.value.isCorrectMessageForBlockExecution(message.message_name)) {
            const arguments = try getMessageArguments(allocator, message.arguments, context);
            defer allocator.free(arguments);

            return try executeBlockMessage(allocator, message.range, block_receiver, arguments, context);
        }
    }

    if (try receiver.lookup(allocator, context, message.message_name, .Value)) |lookup_result| {
        // FIXME: Track lookup_result before getting message arguments
        const arguments = try getMessageArguments(allocator, message.arguments, context);
        defer allocator.free(arguments);

        if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
            return try executeMethodMessage(allocator, heap, message.range, receiver, lookup_result, arguments, context);
        } else {
            return lookup_result;
        }
    } else {
        return runtime_error.raiseError(allocator, context, "Unknown selector \"{s}\"", .{message.message_name});
    }
}
