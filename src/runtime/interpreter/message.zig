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

const ActivationExecutionResult = enum { Success, NonlocalReturn, Error };
/// Execute the activation at the top of the activation stack from its statement
/// offset. If the activation has completed normally or the execution resulted
/// in a non-local return, pops the activation off the activation stack and
/// returns the result of the last expression.
pub fn executeCurrentActivation(context: *InterpreterContext) root_interpreter.InterpreterError!Completion {
    var activation = context.activation_stack.getCurrent().?;

    var execution_result = ActivationExecutionResult.Success;
    defer {
        switch (execution_result) {
            .Success, .NonlocalReturn => {
                const popped_activation = context.activation_stack.popActivation();
                // NOTE: This isn't perfect but should be a good enough heuristic that something is
                //       seriously wrong if it fails.
                std.debug.assert(popped_activation == activation);
                popped_activation.deinit();
            },
            .Error => {},
        }
    }

    // FIXME: Move these pointers to each individual activation, and use getters
    //        on the InterpreterContext.
    const previous_script = context.script;
    const previous_self_object = context.self_object;

    const activation_object = activation.activation_object.asObject().asActivationObject();
    const tracked_activation_object = try context.vm.heap.track(activation.activation_object);
    const script = activation_object.getDefinitionScript();

    context.script = script;
    context.self_object = tracked_activation_object;

    defer {
        tracked_activation_object.untrack(context.vm.heap);

        // Restore the context on successful execution.
        switch (execution_result) {
            .Success => {
                context.script = previous_script;
                context.self_object = previous_self_object;
            },
            else => {},
        }
    }

    var last_expression_result: ?Value = null;
    const statements_slice = activation_object.getStatementsSlice();
    while (activation.statement_index < statements_slice.len) {
        const expression = statements_slice[activation.statement_index];
        var completion = try root_interpreter.executeExpression(context, expression);
        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = value;
                activation.statement_index += 1;
            },
            .NonlocalReturn => |nonlocal_return| {
                if (nonlocal_return.target_activation.get(context)) |target_activation| {
                    if (target_activation == activation) {
                        // The target was us! Turn this into a regular completion
                        // and return with it.
                        last_expression_result = nonlocal_return.value.getValue();
                        completion.deinit(context.vm);
                        break;
                    }
                }

                // The target of the non-local return wasn't us. Allow the error
                // to keep bubbling up.
                execution_result = .NonlocalReturn;
                return completion;
            },
            .RuntimeError => {
                execution_result = .Error;
                return completion;
            },
            .Restart => {
                activation.statement_index = 0;
                last_expression_result = null;
                continue;
            },
        }
    }

    if (last_expression_result) |last_result| {
        // Do not return the activation object from the block when returning "self".
        var result = last_result;
        if (result.isObjectReference() and result.asObject().isActivationObject()) {
            result = result.asObject().asActivationObject().findActivationReceiver();
        }

        return Completion.initNormal(result);
    } else {
        return Completion.initNormal(context.vm.nil());
    }
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

    return try executeCurrentActivation(context);
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

    // NOTE: The receiver of a method activation must never be an activation
    //       object (unless it explicitly wants that), as that would allow
    //       us to access the slots of upper scopes.
    var receiver_of_method = receiver.getValue();
    if (!method_object.expectsActivationObjectAsReceiver() and
        receiver_of_method.isObjectReference() and
        receiver_of_method.asObject().isActivationObject())
    {
        receiver_of_method = receiver_of_method.asObject().asActivationObject().findActivationReceiver();
    }

    const new_activation = context.activation_stack.getNewActivationSlot();
    try method_object.activateMethod(
        context.vm.heap,
        receiver_of_method,
        argument_values.constSlice(),
        source_range,
        new_activation,
    );

    return try executeCurrentActivation(context);
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

    return switch (try tracked_receiver.getValue().lookup(context, name, source_range)) {
        .Regular => |lookup_result| {
            if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
                var tracked_lookup_result = try context.vm.heap.track(lookup_result);
                defer tracked_lookup_result.untrack(context.vm.heap);

                return executeMethodMessage(context, tracked_receiver, tracked_lookup_result, arguments, source_range);
            } else {
                return Completion.initNormal(lookup_result);
            }
        },
        .Assignment => |assignment_context| {
            var argument = arguments[0].getValue();
            // NOTE: This is required, for instance, when we are assigning `self` to
            //       a slot (happens more often than you might think!). We need to strip
            //       the activation object to get to the actual value inside.
            if (argument.isObjectReference() and argument.asObject().isActivationObject()) {
                argument = argument.asObject().asActivationObject().findActivationReceiver();
            }

            const object_that_has_the_assignable_slot = assignment_context.object;
            const value_ptr = assignment_context.value_ptr;
            value_ptr.* = argument;

            // David will remember that.
            try context.vm.heap.rememberObjectReference(object_that_has_the_assignable_slot.asValue(), argument);

            return Completion.initNormal(tracked_receiver.getValue());
        },
        .Nothing => Completion.initRuntimeError(context.vm, source_range, "Unknown selector \"{s}\"", .{name}),
    };
}
