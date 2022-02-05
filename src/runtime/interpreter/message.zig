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
const Completion = @import("../completion.zig");
const primitives = @import("../primitives.zig");
const environment = @import("../environment.zig");

const root_interpreter = @import("../interpreter.zig");
const InterpreterContext = root_interpreter.InterpreterContext;
const MaximumStackDepth = root_interpreter.MaximumStackDepth;

const MessageArgumentsOrCompletion = union(enum) {
    Arguments: []Heap.Tracked,
    Completion: Completion,
};

fn getMessageArguments(
    allocator: Allocator,
    heap: *Heap,
    ast_arguments: []AST.ExpressionNode,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!MessageArgumentsOrCompletion {
    var arguments = try std.ArrayList(Heap.Tracked).initCapacity(allocator, ast_arguments.len);
    var did_complete_normally = false;
    defer {
        if (!did_complete_normally) {
            for (arguments.items) |argument| {
                argument.untrack(heap);
            }
            arguments.deinit();
        }
    }

    for (ast_arguments) |argument| {
        const completion = try root_interpreter.executeExpression(allocator, heap, argument, context);
        if (completion.isNormal()) {
            const tracked_result = try heap.track(completion.data.Normal);
            errdefer tracked_result.untrack(heap);

            try arguments.append(tracked_result);
        } else {
            return MessageArgumentsOrCompletion{ .Completion = completion };
        }
    }

    did_complete_normally = true;
    return MessageArgumentsOrCompletion{ .Arguments = arguments.toOwnedSlice() };
}

fn requiredSizeForBlockMessageName(argument_count: u8) usize {
    var needed_space: usize = 5; // value
    if (argument_count > 0) {
        needed_space += 1; // :
        needed_space += 5 * (argument_count - 1); // Any other With:s needed
    }

    return needed_space;
}

fn writeBlockMessageName(name: []u8, argument_count: u8) void {
    std.debug.assert(name.len == requiredSizeForBlockMessageName(argument_count));
    std.mem.copy(u8, name, "value");

    if (argument_count > 0) {
        name[5] = ':';

        var remaining_buffer = name[6..];
        while (remaining_buffer.len > 0) {
            std.mem.copy(u8, remaining_buffer, "With:");
            remaining_buffer = remaining_buffer[5..];
        }
    }
}

fn getOrCreateBlockMessageName(allocator: Allocator, heap: *Heap, context: *InterpreterContext, argument_count: u8) !Heap.Tracked {
    const result = try context.block_message_names.getOrPut(allocator, argument_count);
    if (result.found_existing) {
        return result.value_ptr.*;
    } else {
        const byte_vector = try ByteVector.createUninitialized(heap, requiredSizeForBlockMessageName(argument_count));
        writeBlockMessageName(byte_vector.getValues(), argument_count);

        const tracked_value = try heap.track(byte_vector.asValue());
        result.value_ptr.* = tracked_value;
        return tracked_value;
    }
}

pub fn executeBlockMessage(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    block_value: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Completion {
    if (context.activation_stack.depth >= MaximumStackDepth) {
        return Completion.initRuntimeError(allocator, "Maximum stack size reached", .{});
    }

    var block_object = block_value.getValue().asObject().asBlockObject();
    // Check if this method can be executed. A block can only be executed if its
    // parent completion is on the activation stack (meaning its weak ptr has not
    // been deactivated).
    if (block_object.getMap().getParentActivation() == null) {
        return Completion.initRuntimeError(allocator, "Attempted to execute a block after its enclosing method has returned. Use objects for closures.", .{});
    }

    const tracked_message_name = try getOrCreateBlockMessageName(allocator, heap, context, @intCast(u8, arguments.len));
    const block_activation = blk: {
        errdefer tracked_message_name.untrack(heap);

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
        errdefer context.script.unref();

        const new_activation = context.activation_stack.getNewActivationSlot();
        try block_object.activateBlock(
            allocator,
            heap,
            block_object.getMap().getParentActivation().?.activation_object,
            argument_values,
            tracked_message_name,
            message_range,
            context.script,
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
    const tracked_block_activation_object = try heap.track(block_activation_object);

    context.script = block_script;
    context.self_object = tracked_block_activation_object;

    defer {
        tracked_block_activation_object.untrack(heap);

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
        const statement = statements_slice[statement_index];

        if (last_expression_result) |last_result| {
            last_result.untrack(heap);
        }

        const completion = try root_interpreter.executeStatement(allocator, heap, statement, context);
        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try heap.track(value);
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
        defer last_result.untrack(heap);
        return Completion.initNormal(last_result.getValue());
    } else {
        return Completion.initNormal(environment.globalNil());
    }
}

pub fn executeMethodMessage(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    receiver: Heap.Tracked,
    tracked_method_object: Heap.Tracked,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Completion {
    if (context.activation_stack.depth >= MaximumStackDepth) {
        return Completion.initRuntimeError(allocator, "Maximum stack size reached", .{});
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

        const new_activation = context.activation_stack.getNewActivationSlot();
        try method_object.activateMethod(
            allocator,
            heap,
            receiver.getValue(),
            argument_values,
            message_range,
            context.script,
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
    const tracked_method_activation_object = try heap.track(method_activation_object);
    context.script = method_script;
    context.self_object = tracked_method_activation_object;

    // NOTE: We don't care about this if an error is bubbling up.
    defer {
        tracked_method_activation_object.untrack(heap);

        if (did_execute_normally) {
            context.script = previous_script;
            context.self_object = previous_self_object;
        }
    }

    var last_expression_result: ?Heap.Tracked = null;
    var statement_index: usize = 0;
    const statements_slice = method_object.getStatementsSlice();
    while (statement_index < statements_slice.len) {
        const statement = statements_slice[statement_index];

        if (last_expression_result) |last_result| {
            last_result.untrack(heap);
        }

        var completion = try root_interpreter.executeStatement(allocator, heap, statement, context);
        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try heap.track(value);
                statement_index += 1;
            },
            .NonlocalReturn => |nonlocal_return| {
                if (nonlocal_return.target_activation.getPointer()) |target_activation| {
                    if (target_activation == method_activation) {
                        // The target was us! Turn this into a regular completion
                        // and return with it.
                        last_expression_result = nonlocal_return.value;
                        completion.deinit(allocator);
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
        defer last_result.untrack(heap);
        return Completion.initNormal(last_result.getValue());
    } else {
        return Completion.initNormal(environment.globalNil());
    }
}

/// Refs `receiver`.
pub fn executePrimitiveMessage(
    allocator: Allocator,
    heap: *Heap,
    message_range: Range,
    tracked_receiver: Heap.Tracked,
    name: []const u8,
    arguments: []Heap.Tracked,
    context: *InterpreterContext,
) root_interpreter.InterpreterError!Completion {
    var receiver = tracked_receiver.getValue();
    if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
        receiver = receiver.asObject().asActivationObject().findActivationReceiver();
    }

    var tracked_bare_receiver = try heap.track(receiver);
    defer tracked_bare_receiver.untrack(heap);

    // All primitives borrow a ref from the caller for the receiver and
    // each argument. It is the primitive's job to unref any argument after
    // its work is done.
    if (primitives.hasPrimitive(name)) {
        return primitives.callPrimitive(allocator, heap, message_range, name, tracked_bare_receiver, arguments, context);
    } else {
        return Completion.initRuntimeError(allocator, "Unknown primitive selector \"{s}\"", .{name});
    }
}

/// The original value in `slot` is unref'd.
pub fn executeAssignmentMessage(
    allocator: Allocator,
    heap: *Heap,
    tracked_receiver: Heap.Tracked,
    message_name: []const u8,
    tracked_argument: Heap.Tracked,
    context: *InterpreterContext,
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
    if (try receiver.lookup(.Assign, message_name_without_colon, allocator, context)) |assign_lookup_result| {
        if (assign_lookup_result == .Completion) {
            return assign_lookup_result.Completion;
        }

        const result = assign_lookup_result.Result;
        const object_that_has_the_assignable_slot = result.object;
        const value_ptr = result.value_ptr;
        value_ptr.* = argument;

        // David will remember that.
        try heap.rememberObjectReference(object_that_has_the_assignable_slot.asValue(), argument);

        return Completion.initNormal(environment.globalNil());
    } else {
        return null;
    }
}

/// Executes a message. All refs are forwarded.
pub fn executeMessage(allocator: Allocator, heap: *Heap, message: AST.MessageNode, context: *InterpreterContext) root_interpreter.InterpreterError!Completion {
    const receiver_completion = try root_interpreter.executeExpression(allocator, heap, message.receiver, context);
    if (!receiver_completion.isNormal()) {
        return receiver_completion;
    }
    var tracked_receiver = try heap.track(receiver_completion.data.Normal);
    defer tracked_receiver.untrack(heap);

    const arguments_or_completion = try getMessageArguments(allocator, heap, message.arguments, context);
    if (arguments_or_completion == .Completion) {
        return arguments_or_completion.Completion;
    }

    var arguments = arguments_or_completion.Arguments;
    defer {
        for (arguments) |argument| {
            argument.untrack(heap);
        }
        allocator.free(arguments);
    }

    // Check for assignable slots
    if (message.message_name[message.message_name.len - 1] == ':' and arguments.len == 1) {
        if (try executeAssignmentMessage(allocator, heap, tracked_receiver, message.message_name, arguments[0], context)) |completion| {
            return completion;
        }
    }

    // Primitive check
    if (message.message_name[0] == '_') {
        return executePrimitiveMessage(allocator, heap, message.range, tracked_receiver, message.message_name, arguments, context);
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
            block_receiver.asObject().asBlockObject().isCorrectMessageForBlockExecution(message.message_name))
        {
            var tracked_block_receiver = try heap.track(block_receiver);
            defer tracked_block_receiver.untrack(heap);

            return executeBlockMessage(allocator, heap, message.range, tracked_block_receiver, arguments, context);
        }
    }

    if (try tracked_receiver.getValue().lookup(.Read, message.message_name, allocator, context)) |lookup_completion| {
        if (!lookup_completion.isNormal()) {
            return lookup_completion;
        }

        const lookup_result = lookup_completion.data.Normal;
        if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
            var tracked_lookup_result = try heap.track(lookup_result);
            defer tracked_lookup_result.untrack(heap);

            return executeMethodMessage(allocator, heap, message.range, tracked_receiver, tracked_lookup_result, arguments, context);
        } else {
            return Completion.initNormal(lookup_result);
        }
    } else {
        return Completion.initRuntimeError(allocator, "Unknown selector \"{s}\"", .{message.message_name});
    }
}
