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
const ByteVector = @import("../byte_vector.zig");
const primitives = @import("../primitives.zig");
const environment = @import("../environment.zig");
const runtime_error = @import("../error.zig");

const root_interpreter = @import("../interpreter.zig");
const InterpreterContext = root_interpreter.InterpreterContext;

const MaximumStackDepth = 2048;

fn getMessageArguments(
    allocator: Allocator,
    heap: *Heap,
    ast_arguments: []AST.ExpressionNode,
    context: *InterpreterContext,
) root_interpreter.InterpreterError![]Heap.Tracked {
    var arguments = try std.ArrayList(Heap.Tracked).initCapacity(allocator, ast_arguments.len);
    errdefer {
        for (arguments.items) |argument| {
            argument.untrackAndDestroy(heap);
        }
        arguments.deinit();
    }

    for (ast_arguments) |argument| {
        var expression_result = try root_interpreter.executeExpression(allocator, heap, argument, context);
        var tracked_result = try heap.track(expression_result);
        errdefer tracked_result.untrackAndDestroy(heap);

        try arguments.append(tracked_result);
    }

    return arguments.toOwnedSlice();
}

pub fn executeBlockMessage(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    block_value: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Value {
    var did_find_activation_in_stack = false;
    var parent_activation: *Activation = undefined;

    var block_object = block_value.getValue().asObject().asBlockObject();
    // Check if this method can be executed, i.e. whether its enclosing
    // activation is currently on the stack.
    if (block_object.getMap().getParentActivation()) |parent_activation_ptr| {
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

    // Ensure that we won't cause a GC by activating the block.
    try heap.ensureSpaceInEden(Object.Activation.requiredSizeForAllocation(block_object.getAssignableSlotCount()));

    // Refresh the pointer in case a GC happened
    block_object = block_value.getValue().asObject().asBlockObject();

    var argument_values = try allocator.alloc(Value, arguments.len);
    defer allocator.free(argument_values);
    for (arguments) |argument, i| {
        argument_values[i] = argument.getValue();
    }

    context.script.ref();
    const block_activation = blk: {
        errdefer context.script.unref();

        const activation = try block_object.activateBlock(
            allocator,
            heap,
            parent_activation.activation_object.getValue(),
            argument_values,
            message_range,
            context.script,
        );
        try context.activation_stack.append(activation);
        break :blk activation;
    };

    // NOTE: We shouldn't pop from the activation stack if we didn't execute
    //       normally and didn't non-local return.
    var did_execute_normally = false;
    var did_nonlocal_return = false;
    defer {
        if (did_execute_normally or did_nonlocal_return) {
            const popped_activation = context.activation_stack.pop();
            std.debug.assert(popped_activation == block_activation);
            popped_activation.destroy(heap);
        }
    }

    const previous_script = context.script;
    const previous_self_object = context.self_object;

    const block_script = block_object.getDefinitionScript();
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

    var last_expression_result: ?Heap.Tracked = null;
    for (block_object.getStatementsSlice()) |statement| {
        if (last_expression_result) |last_result| {
            last_result.untrackAndDestroy(heap);
        }

        const expression_result = root_interpreter.executeStatement(allocator, heap, statement, context) catch |err|
            switch (err) {
            root_interpreter.NonlocalReturnError.NonlocalReturn => |e| {
                did_nonlocal_return = true;
                return e;
            },
            else => |e| return e,
        };

        last_expression_result = try heap.track(expression_result);
    }

    did_execute_normally = true;

    if (last_expression_result) |last_result| {
        defer last_result.untrackAndDestroy(heap);
        return last_result.getValue();
    } else {
        return environment.globalNil();
    }
}

/// `receiver` handles its own refs. `method_object` is forwarded.
pub fn executeMethodMessage(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    receiver: Heap.Tracked,
    tracked_method_object: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) !Value {
    if (context.activation_stack.items.len >= MaximumStackDepth) {
        return runtime_error.raiseError(allocator, context, "Maximum stack size reached", .{});
    }

    var method_object = tracked_method_object.getValue().asObject().asMethodObject();
    // Ensure that we won't cause a GC by activating the method.
    try heap.ensureSpaceInEden(Object.Activation.requiredSizeForAllocation(method_object.getAssignableSlotCount()));

    // Refresh the pointer in case a GC happened
    method_object = tracked_method_object.getValue().asObject().asMethodObject();

    var argument_values = try allocator.alloc(Value, arguments.len);
    defer allocator.free(argument_values);
    for (arguments) |argument, i| {
        argument_values[i] = argument.getValue();
    }

    context.script.ref();

    const method_activation = blk: {
        errdefer context.script.unref();

        const activation = try method_object.activateMethod(
            allocator,
            heap,
            receiver.getValue(),
            argument_values,
            ByteVector.fromAddress(method_object.getMap().method_name.asObjectAddress()).getValues(),
            message_range,
            context.script,
        );
        try context.activation_stack.append(activation);
        break :blk activation;
    };

    // NOTE: We shouldn't pop from the activation stack if we didn't execute
    //       normally and didn't non-local return.
    var did_execute_normally = false;
    var did_nonlocal_return = false;
    defer {
        if (did_execute_normally or did_nonlocal_return) {
            const popped_activation = context.activation_stack.pop();
            std.debug.assert(popped_activation == method_activation);
            popped_activation.destroy(heap);
        }
    }

    const previous_script = context.script;
    const previous_self_object = context.self_object;

    const method_script = method_object.getDefinitionScript();
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

    var last_expression_result: ?Heap.Tracked = null;
    for (method_object.getStatementsSlice()) |statement| {
        if (last_expression_result) |last_result| {
            last_result.untrackAndDestroy(heap);
        }

        const expression_result = root_interpreter.executeStatement(allocator, heap, statement, context) catch |err| {
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

        last_expression_result = try heap.track(expression_result);
    }

    did_execute_normally = true;

    if (last_expression_result) |last_result| {
        defer last_result.untrackAndDestroy(heap);
        return last_result.getValue();
    } else {
        return environment.globalNil();
    }
}

/// Refs `receiver`.
pub fn executePrimitiveMessage(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    receiver: Heap.Tracked,
    name: []const u8,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Value {
    // All primitives borrow a ref from the caller for the receiver and
    // each argument. It is the primitive's job to unref any argument after
    // its work is done.
    if (primitives.hasPrimitive(name)) {
        return try primitives.callPrimitive(allocator, heap, message_range, name, receiver, arguments, context);
    } else {
        return runtime_error.raiseError(allocator, context, "Unknown primitive selector \"{s}\"", .{name});
    }
}

/// The original value in `slot` is unref'd.
pub fn executeAssignmentMessage(
    allocator: Allocator,
    heap: *Heap,
    receiver: Heap.Tracked,
    message_name: []const u8,
    ast_argument: AST.ExpressionNode,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!?Value {
    var argument = try root_interpreter.executeExpression(allocator, heap, ast_argument, context);

    // NOTE: This is required, for instance, when we are assigning `self` to
    //       a slot (happens more often than you might think!). We need to strip
    //       the activation object to get to the actual value inside.
    if (argument.isObjectReference() and argument.asObject().isActivationObject()) {
        argument = argument.asObject().asActivationObject().findActivationReceiver();
    }

    const message_name_without_colon = message_name[0 .. message_name.len - 1];
    if (try receiver.getValue().lookup(.Assign, message_name_without_colon, allocator, context)) |value_ptr| {
        value_ptr.* = argument;
        return environment.globalNil();
    } else {
        return null;
    }
}

/// Executes a message. All refs are forwarded.
pub fn executeMessage(allocator: Allocator, heap: *Heap, message: AST.MessageNode, context: *InterpreterContext) root_interpreter.InterpreterError!Value {
    var receiver = try root_interpreter.executeExpression(allocator, heap, message.receiver, context);

    // Check for assignable slots
    if (message.message_name[message.message_name.len - 1] == ':') {
        var tracked_receiver = try heap.track(receiver);
        defer tracked_receiver.untrackAndDestroy(heap);

        if (try executeAssignmentMessage(allocator, heap, tracked_receiver, message.message_name, message.arguments[0], context)) |value| {
            return value;
        }

        // Make sure to refresh the pointer as executeAssignmentMessage could
        // have caused a GC
        receiver = tracked_receiver.getValue();
    }

    // Primitive check
    if (message.message_name[0] == '_') {
        if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
            receiver = receiver.asObject().asActivationObject().findActivationReceiver();
        }

        var tracked_receiver = try heap.track(receiver);
        defer tracked_receiver.untrackAndDestroy(heap);

        const arguments = try getMessageArguments(allocator, heap, message.arguments, context);
        defer {
            for (arguments) |argument| {
                argument.untrackAndDestroy(heap);
            }
            allocator.free(arguments);
        }

        return try executePrimitiveMessage(allocator, heap, message.range, tracked_receiver, message.message_name, arguments, context);
    }

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    {
        var block_receiver = receiver;
        if (block_receiver.isObjectReference() and block_receiver.asObject().isActivationObject()) {
            block_receiver = receiver.asObject().asActivationObject().findActivationReceiver();
        }

        if (block_receiver.isObjectReference() and
            block_receiver.asObject().isBlockObject() and
            block_receiver.asObject().asBlockObject().isCorrectMessageForBlockExecution(message.message_name))
        {
            var tracked_receiver = try heap.track(block_receiver);
            defer tracked_receiver.untrackAndDestroy(heap);

            const arguments = try getMessageArguments(allocator, heap, message.arguments, context);
            defer {
                for (arguments) |argument| {
                    argument.untrackAndDestroy(heap);
                }
                allocator.free(arguments);
            }

            return try executeBlockMessage(allocator, heap, message.range, tracked_receiver, arguments, context);
        }
    }

    if (try receiver.lookup(.Read, message.message_name, allocator, context)) |lookup_result| {
        if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
            var tracked_receiver = try heap.track(receiver);
            defer tracked_receiver.untrackAndDestroy(heap);

            var tracked_lookup_result = try heap.track(lookup_result);
            defer tracked_lookup_result.untrackAndDestroy(heap);

            const arguments = try getMessageArguments(allocator, heap, message.arguments, context);
            defer allocator.free(arguments);

            return try executeMethodMessage(allocator, heap, message.range, tracked_receiver, tracked_lookup_result, arguments, context);
        } else {
            return lookup_result;
        }
    } else {
        return runtime_error.raiseError(allocator, context, "Unknown selector \"{s}\"", .{message.message_name});
    }
}
