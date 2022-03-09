// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../../language/ast.zig");
const Slot = @import("../slot.zig").Slot;
const Heap = @import("../heap.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../object.zig");
const Activation = @import("../activation.zig");
const ByteVector = @import("../byte_array.zig");
const Completion = @import("../completion.zig");
const primitives = @import("../primitives.zig");
const SourceRange = @import("../../language/source_range.zig");

const root_interpreter = @import("../interpreter.zig");
const InterpreterContext = root_interpreter.InterpreterContext;
const MaximumStackDepth = root_interpreter.MaximumStackDepth;
const MaximumArguments = root_interpreter.MaximumArguments;

fn getMessageArguments(
    context: *InterpreterContext,
    ast_arguments: []AST.ExpressionNode,
    arguments: *std.BoundedArray(Heap.Tracked, MaximumArguments),
) root_interpreter.InterpreterError!?Completion {
    var did_complete_normally = false;
    defer {
        if (!did_complete_normally) {
            for (arguments.slice()) |argument| {
                argument.untrack(context.vm.heap);
            }
        }
    }

    for (ast_arguments) |argument| {
        const completion = try root_interpreter.executeExpression(context, argument);
        if (completion.isNormal()) {
            // Make sure that we are not adding the activation object to
            // arguments
            var result = completion.data.Normal;
            if (result.isObjectReference() and result.asObject().isActivationObject()) {
                result = result.asObject().asActivationObject().findActivationReceiver();
            }

            const tracked_result = try context.vm.heap.track(result);
            arguments.appendAssumeCapacity(tracked_result);
        } else {
            return completion;
        }
    }

    did_complete_normally = true;
    return null;
}

pub fn executeBlockMessage(
    context: *InterpreterContext,
    block_value: Heap.Tracked,
    arguments: []const Heap.Tracked,
    source_range: SourceRange,
) root_interpreter.InterpreterError!Completion {
    if (context.activation_stack.depth >= MaximumStackDepth) {
        return Completion.initRuntimeError(context.vm, source_range, "Maximum stack size reached", .{});
    }

    var block_object = block_value.getValue().asObject().asBlockObject();
    // Check if this method can be executed. A block can only be executed if its
    // parent completion is on the activation stack (meaning its weak ptr has not
    // been deactivated).
    if (!block_object.getMap().parent_activation.isAlive(context)) {
        return Completion.initRuntimeError(context.vm, source_range, "Attempted to execute a block after its enclosing method has returned. Use objects for closures.", .{});
    }

    const tracked_message_name = try context.vm.getOrCreateBlockMessageName(@intCast(u8, arguments.len));
    const block_activation = blk: {
        // Ensure that we won't cause a GC by activating the block.
        try context.vm.heap.ensureSpaceInEden(Object.Activation.requiredSizeForAllocation(block_object.getArgumentSlotCount(), block_object.getAssignableSlotCount()));

        // Refresh the pointer in case a GC happened
        block_object = block_value.getValue().asObject().asBlockObject();

        var argument_values = std.BoundedArray(Value, MaximumArguments).init(0) catch unreachable;
        for (arguments) |argument| argument_values.appendAssumeCapacity(argument.getValue());

        const new_activation = context.activation_stack.getNewActivationSlot();
        try block_object.activateBlock(
            context,
            block_object.getMap().parent_activation.get(context).?.activation_object,
            argument_values.constSlice(),
            tracked_message_name,
            source_range,
            new_activation,
        );
        break :blk new_activation;
    };

    // NOTE: We shouldn't pop from the activation stack if we didn't execute
    //       normally and didn't non-local return.
    var did_execute_normally = false;
    var did_nonlocal_return = false;
    defer {
        if (did_execute_normally or did_nonlocal_return) {
            const popped_activation = context.activation_stack.popActivation();
            // NOTE: This isn't perfect but should be a good enough heuristic that something is
            //       seriously wrong if it fails.
            std.debug.assert(popped_activation == block_activation);
            popped_activation.deinit();
        }
    }

    const previous_script = context.script;
    const previous_self_object = context.self_object;

    const block_script = block_object.getDefinitionScript();
    const block_activation_object = block_activation.activation_object;
    const tracked_block_activation_object = try context.vm.heap.track(block_activation_object);

    context.script = block_script;
    context.self_object = tracked_block_activation_object;

    defer {
        tracked_block_activation_object.untrack(context.vm.heap);

        // NOTE: We don't care about this if an error is bubbling up.
        if (did_execute_normally) {
            context.script = previous_script;
            context.self_object = previous_self_object;
        }
    }

    var last_expression_result: ?Heap.Tracked = null;
    var statement_index: usize = 0;
    const statements_slice = block_object.getStatementsSlice();
    while (statement_index < statements_slice.len) {
        const expression = statements_slice[statement_index];

        if (last_expression_result) |last_result| {
            last_result.untrack(context.vm.heap);
        }

        const completion = try root_interpreter.executeExpression(context, expression);
        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try context.vm.heap.track(value);
                statement_index += 1;
            },
            .NonlocalReturn => {
                did_nonlocal_return = true;
                return completion;
            },
            .RuntimeError => return completion,
            .Restart => {
                statement_index = 0;
                last_expression_result = null;
                continue;
            },
        }
    }

    did_execute_normally = true;

    if (last_expression_result) |last_result| {
        defer last_result.untrack(context.vm.heap);

        // Do not return the activation object from the block when returning "self".
        var result = last_result.getValue();
        if (result.isObjectReference() and result.asObject().isActivationObject()) {
            result = result.asObject().asActivationObject().findActivationReceiver();
        }

        return Completion.initNormal(result);
    } else {
        return Completion.initNormal(context.vm.nil());
    }
}

pub fn executeMethodMessage(
    context: *InterpreterContext,
    receiver: Heap.Tracked,
    tracked_method_object: Heap.Tracked,
    arguments: []const Heap.Tracked,
    source_range: SourceRange,
) root_interpreter.InterpreterError!Completion {
    if (context.activation_stack.depth >= MaximumStackDepth) {
        return Completion.initRuntimeError(context.vm, source_range, "Maximum stack size reached", .{});
    }

    var method_object = tracked_method_object.getValue().asObject().asMethodObject();
    // Ensure that we won't cause a GC by activating the method.
    try context.vm.heap.ensureSpaceInEden(Object.Activation.requiredSizeForAllocation(method_object.getArgumentSlotCount(), method_object.getAssignableSlotCount()));

    // Refresh the pointer in case a GC happened
    method_object = tracked_method_object.getValue().asObject().asMethodObject();

    var argument_values = std.BoundedArray(Value, MaximumArguments).init(0) catch unreachable;
    for (arguments) |argument| argument_values.appendAssumeCapacity(argument.getValue());

    const method_activation = blk: {
        const new_activation = context.activation_stack.getNewActivationSlot();
        try method_object.activateMethod(
            context.vm.heap,
            receiver.getValue(),
            argument_values.constSlice(),
            source_range,
            new_activation,
        );
        break :blk new_activation;
    };

    // NOTE: We shouldn't pop from the activation stack if we didn't execute
    //       normally and didn't non-local return.
    var did_execute_normally = false;
    var did_nonlocal_return = false;
    defer {
        if (did_execute_normally or did_nonlocal_return) {
            const popped_activation = context.activation_stack.popActivation();
            // NOTE: Same deal as in executeBlockMessage.
            std.debug.assert(popped_activation == method_activation);
            popped_activation.deinit();
        }
    }

    const previous_script = context.script;
    const previous_self_object = context.self_object;

    const method_script = method_object.getDefinitionScript();
    const method_activation_object = method_activation.activation_object;
    const tracked_method_activation_object = try context.vm.heap.track(method_activation_object);
    context.script = method_script;
    context.self_object = tracked_method_activation_object;

    // NOTE: We don't care about this if an error is bubbling up.
    defer {
        tracked_method_activation_object.untrack(context.vm.heap);

        if (did_execute_normally) {
            context.script = previous_script;
            context.self_object = previous_self_object;
        }
    }

    var last_expression_result: ?Heap.Tracked = null;
    var statement_index: usize = 0;
    const statements_slice = method_object.getStatementsSlice();
    while (statement_index < statements_slice.len) {
        const expression = statements_slice[statement_index];

        if (last_expression_result) |last_result| {
            last_result.untrack(context.vm.heap);
        }

        var completion = try root_interpreter.executeExpression(context, expression);
        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try context.vm.heap.track(value);
                statement_index += 1;
            },
            .NonlocalReturn => |nonlocal_return| {
                if (nonlocal_return.target_activation.get(context)) |target_activation| {
                    if (target_activation == method_activation) {
                        // The target was us! Turn this into a regular completion
                        // and return with it.
                        last_expression_result = nonlocal_return.value;
                        completion.deinit(context.vm);
                        break;
                    }
                }

                // The target of the non-local return wasn't us. Allow the error
                // to keep bubbling up.
                did_nonlocal_return = true;
                return completion;
            },
            .RuntimeError => return completion,
            .Restart => {
                statement_index = 0;
                last_expression_result = null;
                continue;
            },
        }
    }

    did_execute_normally = true;

    if (last_expression_result) |last_result| {
        defer last_result.untrack(context.vm.heap);

        // Do not return the activation object from the method when returning "self".
        var result = last_result.getValue();
        if (result.isObjectReference() and result.asObject().isActivationObject()) {
            result = result.asObject().asActivationObject().findActivationReceiver();
        }

        return Completion.initNormal(result);
    } else {
        return Completion.initNormal(context.vm.nil());
    }
}

/// Refs `receiver`.
pub fn executePrimitiveMessage(
    context: *InterpreterContext,
    tracked_receiver: Heap.Tracked,
    name: []const u8,
    arguments: []const Heap.Tracked,
    source_range: SourceRange,
) root_interpreter.InterpreterError!Completion {
    var receiver = tracked_receiver.getValue();
    if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
        receiver = receiver.asObject().asActivationObject().findActivationReceiver();
    }

    var tracked_bare_receiver = try context.vm.heap.track(receiver);
    defer tracked_bare_receiver.untrack(context.vm.heap);

    // All primitives borrow a ref from the caller for the receiver and
    // each argument. It is the primitive's job to unref any argument after
    // its work is done.
    if (primitives.getPrimitive(name)) |primitive| {
        return primitive.call(context, tracked_bare_receiver, arguments, source_range);
    } else {
        return Completion.initRuntimeError(context.vm, source_range, "Unknown primitive selector \"{s}\"", .{name});
    }
}

/// The original value in `slot` is unref'd.
pub fn executeAssignmentMessage(
    context: *InterpreterContext,
    tracked_receiver: Heap.Tracked,
    message_name: []const u8,
    tracked_argument: Heap.Tracked,
    source_range: SourceRange,
) root_interpreter.InterpreterError!?Completion {
    const receiver = tracked_receiver.getValue();
    var argument = tracked_argument.getValue();
    // NOTE: This is required, for instance, when we are assigning `self` to
    //       a slot (happens more often than you might think!). We need to strip
    //       the activation object to get to the actual value inside.
    if (argument.isObjectReference() and argument.asObject().isActivationObject()) {
        argument = argument.asObject().asActivationObject().findActivationReceiver();
    }

    const message_name_without_colon = message_name[0 .. message_name.len - 1];
    if (try receiver.lookup(.Assign, context, message_name_without_colon, source_range)) |assign_lookup_result| {
        if (assign_lookup_result == .Completion) {
            return assign_lookup_result.Completion;
        }

        const result = assign_lookup_result.Result;
        const object_that_has_the_assignable_slot = result.object;
        const value_ptr = result.value_ptr;
        value_ptr.* = argument;

        // David will remember that.
        try context.vm.heap.rememberObjectReference(object_that_has_the_assignable_slot.asValue(), argument);

        return Completion.initNormal(context.vm.nil());
    } else {
        return null;
    }
}

/// Executes a message. All refs are forwarded.
pub fn executeMessage(context: *InterpreterContext, message: AST.MessageNode) root_interpreter.InterpreterError!Completion {
    var source_range = SourceRange.init(context.script, message.range);
    defer source_range.deinit();

    const receiver = if (message.receiver) |expr| blk: {
        const receiver_completion = try root_interpreter.executeExpression(context, expr);
        if (!receiver_completion.isNormal())
            return receiver_completion;
        break :blk receiver_completion.data.Normal;
    } else context.self_object.getValue();

    var tracked_receiver = try context.vm.heap.track(receiver);
    defer tracked_receiver.untrack(context.vm.heap);

    var arguments = std.BoundedArray(Heap.Tracked, MaximumArguments).init(0) catch unreachable;
    if (try getMessageArguments(context, message.arguments, &arguments)) |completion| {
        return completion;
    }

    defer {
        for (arguments.slice()) |argument| {
            argument.untrack(context.vm.heap);
        }
    }
    const arguments_slice = arguments.constSlice();

    return sendMessage(context, tracked_receiver, message.message_name, arguments_slice, source_range);
}

/// Send a message to a receiver with the given arguments. Trusts the caller
/// to supply the correct amount of arguments.
pub fn sendMessage(
    context: *InterpreterContext,
    tracked_receiver: Heap.Tracked,
    name: []const u8,
    arguments: []const Heap.Tracked,
    source_range: SourceRange,
) root_interpreter.InterpreterError!Completion {
    // Check for assignable slots
    if (name[name.len - 1] == ':' and arguments.len == 1) {
        if (try executeAssignmentMessage(context, tracked_receiver, name, arguments[0], source_range)) |completion| {
            return completion;
        }
    }

    // Primitive check
    if (name[0] == '_') {
        return executePrimitiveMessage(context, tracked_receiver, name, arguments, source_range);
    }

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    // FIXME: Only activate this when the message looks like a block execution.
    {
        var block_receiver = tracked_receiver.getValue();
        if (block_receiver.isObjectReference() and block_receiver.asObject().isActivationObject()) {
            block_receiver = block_receiver.asObject().asActivationObject().findActivationReceiver();
        }

        if (block_receiver.isObjectReference() and
            block_receiver.asObject().isBlockObject() and
            block_receiver.asObject().asBlockObject().isCorrectMessageForBlockExecution(name))
        {
            var tracked_block_receiver = try context.vm.heap.track(block_receiver);
            defer tracked_block_receiver.untrack(context.vm.heap);

            return executeBlockMessage(context, tracked_block_receiver, arguments, source_range);
        }
    }

    if (try tracked_receiver.getValue().lookup(.Read, context, name, source_range)) |lookup_completion| {
        if (!lookup_completion.isNormal()) {
            return lookup_completion;
        }

        const lookup_result = lookup_completion.data.Normal;
        if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
            var tracked_lookup_result = try context.vm.heap.track(lookup_result);
            defer tracked_lookup_result.untrack(context.vm.heap);

            return executeMethodMessage(context, tracked_receiver, tracked_lookup_result, arguments, source_range);
        } else {
            return Completion.initNormal(lookup_result);
        }
    } else {
        return Completion.initRuntimeError(context.vm, source_range, "Unknown selector \"{s}\"", .{name});
    }
}
