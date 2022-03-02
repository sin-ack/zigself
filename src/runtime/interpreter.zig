// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("../language/ast.zig");
const Slot = @import("./slot.zig").Slot;
const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig");
const Script = @import("../language/script.zig");
const ByteArray = @import("./byte_array.zig");
const Activation = @import("./activation.zig");
const Completion = @import("./completion.zig");
const SourceRange = @import("../language/source_range.zig");
const runtime_error = @import("./error.zig");
const VirtualMachine = @import("./virtual_machine.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");
const ActivationStack = Activation.ActivationStack;

const message_interpreter = @import("./interpreter/message.zig");

pub const MaximumStackDepth = 2048;
pub const MaximumArguments = 128; // Reasonable limit
pub const MaximumAssignableSlots = 255;

pub const InterpreterContext = struct {
    /// The virtual machine that the interpreter is currently executing on.
    vm: *VirtualMachine,
    /// The object that is the current context. The identifier "self" will
    /// resolve to this object.
    self_object: Heap.Tracked,
    /// The method/block activation stack. This is used with blocks in order to
    /// verify that the block is executed within its enclosing method and for
    /// stack traces. When the activation completes, the activation object is
    /// popped; when a new activation occurs, it is pushed.
    activation_stack: *ActivationStack,
    /// The script file that is currently executing, used to resolve the
    /// relative paths of other script files.
    script: Script.Ref,
};

pub const InterpreterError = Allocator.Error;

/// Executes a script node. `lobby` is ref'd for the function lifetime. The last
/// expression result is returned, or if no statements were available, null is
/// returned.
///
/// Refs `script`.
pub fn executeScript(vm: *VirtualMachine, script: Script.Ref) InterpreterError!?Value {
    script.ref();
    defer script.unref();

    // Did this script execute normally?
    var did_execute_normally = false;

    var activation_stack = try ActivationStack.init(vm.allocator, MaximumStackDepth);
    defer {
        if (!did_execute_normally) {
            // Since the execution was abruptly stopped the activation stack
            // wasn't properly unwound, so let's do that now.
            for (activation_stack.getStack()) |*activation| {
                activation.deinit();
            }
        }
        activation_stack.deinit(vm.allocator);
    }

    vm.heap.setActivationStack(&activation_stack);
    defer vm.heap.setActivationStack(null);

    var context = InterpreterContext{
        .vm = vm,
        .self_object = try vm.heap.track(vm.lobby()),
        .activation_stack = &activation_stack,
        .script = script,
    };
    var last_expression_result: ?Heap.Tracked = null;
    for (script.value.ast_root.?.statements) |statement| {
        std.debug.assert(activation_stack.depth == 0);

        if (last_expression_result) |result| {
            result.untrack(vm.heap);
        }

        var completion = try executeStatement(&context, statement);
        defer completion.deinit(vm);

        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try vm.heap.track(value);
            },
            .RuntimeError => |err| {
                std.debug.print("Received error at top level: {s}\n", .{err.message});
                runtime_error.printTraceFromActivationStack(activation_stack.getStack(), err.source_range);
                return null;
            },
            else => unreachable,
        }
    }

    did_execute_normally = true;
    if (last_expression_result) |result| {
        defer result.untrack(vm.heap);
        return result.getValue();
    }

    return null;
}

/// Execute a script object as a child script of the root script. The parent
/// interpreter context is passed in order to preserve the activation stack and
/// various other context objects.
///
/// Refs `script`.
pub fn executeSubScript(parent_context: *InterpreterContext, script: Script.Ref) InterpreterError!?Completion {
    script.ref();
    defer script.unref();

    var child_context = InterpreterContext{
        .vm = parent_context.vm,
        .self_object = try parent_context.vm.heap.track(parent_context.vm.lobby()),
        .activation_stack = parent_context.activation_stack,
        .script = script,
    };
    var last_expression_result: ?Heap.Tracked = null;
    for (script.value.ast_root.?.statements) |statement| {
        if (last_expression_result) |result| {
            result.untrack(parent_context.vm.heap);
        }

        var did_complete_statement_successfully = false;
        var completion = try executeStatement(&child_context, statement);
        defer {
            if (did_complete_statement_successfully) {
                completion.deinit(parent_context.vm);
            }
        }

        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try parent_context.vm.heap.track(value);
                did_complete_statement_successfully = true;
            },
            .RuntimeError => {
                // Allow the error to keep bubbling up.
                return completion;
            },
            else => unreachable,
        }
    }

    if (last_expression_result) |result| {
        defer result.untrack(parent_context.vm.heap);
        return Completion.initNormal(result.getValue());
    }

    return null;
}

/// Executes a statement. All refs are forwardded.
pub fn executeStatement(context: *InterpreterContext, statement: AST.StatementNode) InterpreterError!Completion {
    return try executeExpression(context, statement.expression);
}

/// Executes an expression. All refs are forwarded.
pub fn executeExpression(context: *InterpreterContext, expression: AST.ExpressionNode) InterpreterError!Completion {
    return switch (expression) {
        .Object => |object| try executeObject(context, object.*),
        .Block => |block| try executeBlock(context, block.*),
        .Message => |message| try message_interpreter.executeMessage(context, message.*),
        .Return => |return_node| return executeReturn(context, return_node.*),

        .Identifier => |identifier| try executeIdentifier(context, identifier),
        .String => |string| try executeString(context, string),
        .Number => |number| try executeNumber(context, number),
    };
}

/// Creates a new method object. All refs are forwarded. `arguments` and
/// `object_node`'s statements are copied.
fn executeMethod(context: *InterpreterContext, name: []const u8, object_node: AST.ObjectNode, arguments: [][]const u8) InterpreterError!Completion {
    // FIXME: Get a better source range for the method itself
    var source_range = SourceRange.init(context.script, object_node.range);
    defer source_range.deinit();

    if (arguments.len > MaximumArguments)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum argument limit exceeded for method", .{});

    var assignable_slot_count: usize = 0;
    for (object_node.slots) |slot| {
        if (slot.is_mutable) assignable_slot_count += 1;
    }
    if (arguments.len + object_node.slots.len > MaximumAssignableSlots)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for method", .{});

    var assignable_slot_values = std.BoundedArray(Heap.Tracked, MaximumAssignableSlots).init(0) catch unreachable;
    defer {
        for (assignable_slot_values.slice()) |value| {
            value.untrack(context.vm.heap);
        }
    }

    // This will prevent garbage collections until the execution of slots at
    // least.
    var required_memory: usize = ByteArray.requiredSizeForAllocation(name.len);
    required_memory += Object.Map.Method.requiredSizeForAllocation(@intCast(u32, object_node.slots.len + arguments.len));
    for (arguments) |argument| {
        required_memory += ByteArray.requiredSizeForAllocation(argument.len);
    }

    try context.vm.heap.ensureSpaceInEden(required_memory);

    context.script.ref();
    object_node.statements.ref();
    // NOTE: Once we create the method map successfully, the ref we just created
    // above is owned by the method_map, and we shouldn't try to unref in case
    // of an error.
    var method_map = blk: {
        errdefer context.script.unref();
        errdefer object_node.statements.unrefWithAllocator(context.vm.allocator);

        const method_name_in_heap = try ByteArray.createFromString(context.vm.heap, name);
        break :blk try Object.Map.Method.create(
            context.vm.heap,
            @intCast(u8, arguments.len),
            @intCast(u32, object_node.slots.len),
            object_node.statements,
            method_name_in_heap,
            context.script,
        );
    };

    var slot_init_offset: usize = 0;

    const argument_slots = method_map.getArgumentSlots();
    for (arguments) |argument| {
        const argument_in_heap = try ByteArray.createFromString(context.vm.heap, argument);
        argument_slots[slot_init_offset].initMutable(Object.Map.Method, method_map, argument_in_heap, .NotParent);
        // FIXME: Avoid this unnecessary tracking of the nil value for arguments.
        assignable_slot_values.append(try context.vm.heap.track(context.vm.nil())) catch unreachable;

        slot_init_offset += 1;
    }

    const tracked_method_map = try context.vm.heap.track(method_map.asValue());
    defer tracked_method_map.untrack(context.vm.heap);

    for (object_node.slots) |slot_node| {
        const slot_completion = try executeSlot(context, slot_node, Object.Map.Method, tracked_method_map.getValue().asObject().asMap().asMethodMap(), slot_init_offset);
        if (slot_completion) |completion| {
            if (completion.isNormal())
                assignable_slot_values.append(try context.vm.heap.track(completion.data.Normal)) catch unreachable
            else
                return completion;
        }

        slot_init_offset += 1;
    }

    // Ensure that creating the method object won't cause a garbage collection
    // before the assignable slot values are copied in.
    try context.vm.heap.ensureSpaceInEden(Object.Method.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len)));

    var current_assignable_slot_values = std.BoundedArray(Value, MaximumAssignableSlots).init(assignable_slot_values.len) catch unreachable;
    for (assignable_slot_values.constSlice()) |value, i| {
        current_assignable_slot_values.set(i, value.getValue());
    }

    const method_object = try Object.Method.create(
        context.vm.heap,
        tracked_method_map.getValue().asObject().asMap().asMethodMap(),
        current_assignable_slot_values.constSlice(),
    );
    return Completion.initNormal(method_object.asValue());
}

/// Creates a new slot. All refs are forwarded.
pub fn executeSlot(context: *InterpreterContext, slot_node: AST.SlotNode, comptime MapType: type, map: *MapType, slot_index: usize) InterpreterError!?Completion {
    const completion = blk: {
        if (slot_node.value == .Object and (slot_node.value.Object.statements.value.statements.len > 0 or slot_node.arguments.len > 0)) {
            break :blk try executeMethod(context, slot_node.name, slot_node.value.Object.*, slot_node.arguments);
        } else {
            break :blk try executeExpression(context, slot_node.value);
        }
    };
    if (!completion.isNormal()) {
        return completion;
    }

    const tracked_value = try context.vm.heap.track(completion.data.Normal);
    defer tracked_value.untrack(context.vm.heap);

    const slot_name = try ByteArray.createFromString(context.vm.heap, slot_node.name);
    if (slot_node.is_mutable) {
        map.getSlots()[slot_index].initMutable(MapType, map, slot_name, if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent);
        return Completion.initNormal(tracked_value.getValue());
    } else {
        map.getSlots()[slot_index].initConstant(slot_name, if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent, tracked_value.getValue());
        return null;
    }
}

/// Creates a new slots object. All refs are forwarded.
pub fn executeObject(context: *InterpreterContext, object_node: AST.ObjectNode) InterpreterError!Completion {
    // Verify that we are executing a slots object and not a method; methods
    // are created through executeSlot.
    if (object_node.statements.value.statements.len > 0) {
        @panic("!!! Attempted to execute a non-slots object! Methods must be created via executeSlot.");
    }

    var source_range = SourceRange.init(context.script, object_node.range);
    defer source_range.deinit();

    // Verify that the assignable slots in this object are within limits
    var assignable_slot_count: usize = 0;
    for (object_node.slots) |slot| {
        if (slot.is_mutable) assignable_slot_count += 1;
    }
    if (assignable_slot_count > MaximumAssignableSlots)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for slots object", .{});

    var assignable_slot_values = std.BoundedArray(Heap.Tracked, MaximumAssignableSlots).init(0) catch unreachable;
    defer {
        for (assignable_slot_values.slice()) |value| {
            value.untrack(context.vm.heap);
        }
    }

    var slots_map = try Object.Map.Slots.create(context.vm.heap, @intCast(u32, object_node.slots.len));
    const tracked_slots_map = try context.vm.heap.track(slots_map.asValue());
    defer tracked_slots_map.untrack(context.vm.heap);

    for (object_node.slots) |slot_node, i| {
        var slot_completion = try executeSlot(context, slot_node, Object.Map.Slots, tracked_slots_map.getValue().asObject().asMap().asSlotsMap(), i);
        if (slot_completion) |completion| {
            if (completion.isNormal()) {
                assignable_slot_values.append(try context.vm.heap.track(completion.data.Normal)) catch unreachable;
            } else {
                return completion;
            }
        }
    }

    // Ensure that creating the slots object won't cause a garbage collection
    // before the assignable slot values are copied in.
    try context.vm.heap.ensureSpaceInEden(Object.Slots.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len)));

    var current_assignable_slot_values = std.BoundedArray(Value, MaximumAssignableSlots).init(assignable_slot_values.len) catch unreachable;
    for (assignable_slot_values.constSlice()) |value, i| {
        current_assignable_slot_values.set(i, value.getValue());
    }

    const slots_object = try Object.Slots.create(
        context.vm.heap,
        tracked_slots_map.getValue().asObject().asMap().asSlotsMap(),
        current_assignable_slot_values.constSlice(),
    );
    return Completion.initNormal(slots_object.asValue());
}

pub fn executeBlock(context: *InterpreterContext, block: AST.BlockNode) InterpreterError!Completion {
    var argument_slot_count: usize = 0;
    var assignable_slot_count: usize = 0;
    for (block.slots) |slot_node| {
        if (slot_node.is_argument) {
            argument_slot_count += 1;
        } else if (slot_node.is_mutable) {
            assignable_slot_count += 1;
        }
    }

    var source_range = SourceRange.init(context.script, block.range);
    defer source_range.deinit();

    if (argument_slot_count > MaximumArguments)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum argument limit exceeded for block", .{});
    if (argument_slot_count + assignable_slot_count > MaximumAssignableSlots)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for block", .{});

    var assignable_slot_values = std.BoundedArray(Heap.Tracked, MaximumAssignableSlots).init(0) catch unreachable;
    defer {
        for (assignable_slot_values.slice()) |value| {
            value.untrack(context.vm.heap);
        }
    }

    // The latest activation is where the block was created, so it will always
    // be the parent activation (i.e., where we look for parent blocks' and the
    // method's slots).
    const parent_activation = &context.activation_stack.stack[context.activation_stack.depth - 1];
    // However, we want the _method_ as the non-local return target; because the
    // non-local return can only be returned by the method in which the block
    // making the non-local return was defined, this needs to be separate from
    // parent_activation. If the parent activation is a block, it will also
    // contain a target activation; if it's a method the target activation _is_
    // the parent.
    const nonlocal_return_target_activation = if (parent_activation.nonlocal_return_target_activation) |target| target else parent_activation;
    std.debug.assert(nonlocal_return_target_activation.nonlocal_return_target_activation == null);

    var required_memory: usize = Object.Map.Block.requiredSizeForAllocation(@intCast(u32, block.slots.len));
    for (block.slots) |slot_node| {
        if (slot_node.is_argument) {
            required_memory += ByteArray.requiredSizeForAllocation(slot_node.name.len);
        }
    }

    try context.vm.heap.ensureSpaceInEden(required_memory);

    context.script.ref();
    block.statements.ref();
    // NOTE: Once we create the block map successfully, the ref we just created
    // above is owned by the block_map, and we shouldn't try to unref in case
    // of an error.
    var block_map = blk: {
        errdefer context.script.unref();
        errdefer block.statements.unrefWithAllocator(context.vm.allocator);

        break :blk try Object.Map.Block.create(
            context.vm.heap,
            @intCast(u8, argument_slot_count),
            @intCast(u32, block.slots.len) - @intCast(u8, argument_slot_count),
            block.statements,
            parent_activation,
            nonlocal_return_target_activation,
            context.script,
        );
    };

    var slot_init_offset: usize = 0;

    // Add all the argument slots
    var argument_slots = block_map.getArgumentSlots();
    for (block.slots) |slot_node| {
        if (slot_node.is_argument) {
            const slot_name = try ByteArray.createFromString(context.vm.heap, slot_node.name);

            argument_slots[slot_init_offset].initMutable(
                Object.Map.Block,
                block_map,
                slot_name,
                if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent,
            );
            // FIXME: Avoid tracking all these nil values for arguments.
            assignable_slot_values.append(try context.vm.heap.track(context.vm.nil())) catch unreachable;

            slot_init_offset += 1;
        }
    }

    const tracked_block_map = try context.vm.heap.track(block_map.asValue());
    defer tracked_block_map.untrack(context.vm.heap);

    // Add all the non-argument slots
    for (block.slots) |slot_node| {
        if (!slot_node.is_argument) {
            var slot_completion = try executeSlot(
                context,
                slot_node,
                Object.Map.Block,
                tracked_block_map.getValue().asObject().asMap().asBlockMap(),
                slot_init_offset,
            );
            if (slot_completion) |completion| {
                if (completion.isNormal()) {
                    assignable_slot_values.append(try context.vm.heap.track(completion.data.Normal)) catch unreachable;
                } else {
                    return completion;
                }
            }

            slot_init_offset += 1;
        }
    }

    // Ensure that creating the block object won't cause a garbage collection
    // before the assignable slot values are copied in.
    try context.vm.heap.ensureSpaceInEden(Object.Block.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.len)));

    var current_assignable_slot_values = std.BoundedArray(Value, MaximumAssignableSlots).init(assignable_slot_values.len) catch unreachable;
    for (assignable_slot_values.constSlice()) |value, i| {
        current_assignable_slot_values.set(i, value.getValue());
    }

    const block_object = try Object.Block.create(context.vm.heap, tracked_block_map.getValue().asObject().asMap().asBlockMap(), current_assignable_slot_values.constSlice());
    return Completion.initNormal(block_object.asValue());
}

pub fn executeReturn(context: *InterpreterContext, return_node: AST.ReturnNode) InterpreterError!Completion {
    const latest_activation = context.activation_stack.stack[context.activation_stack.depth - 1];
    const target_activation = latest_activation.nonlocal_return_target_activation.?;
    std.debug.assert(target_activation.nonlocal_return_target_activation == null);

    const completion = try executeExpression(context, return_node.expression);
    switch (completion.data) {
        .Normal => {
            return Completion.initNonlocalReturn(target_activation.takeRef(), try context.vm.heap.track(completion.data.Normal));
        },
        .RuntimeError => return completion,
        .NonlocalReturn => {
            // It's completely normal to encounter a non-local return while performing one
            // ourselves. Consider the following situation:
            //
            // foo = (
            //     [ ^ [ ^ 'Well hello friends!' ] value ] value.
            // ).
            //
            // Here we would reach another non-local return before we reach the method in
            // which the block was defined in the activation stack. The best course
            // of action here is to use the non-local return that belongs to the completion
            // we have just encountered (as it wouldn't make sense to disrupt the flow
            // of the currently bubbling non-local return).
            return completion;
        },
        else => unreachable,
    }
}

/// Executes an identifier expression.
pub fn executeIdentifier(context: *InterpreterContext, identifier: AST.IdentifierNode) InterpreterError!Completion {
    var source_range = SourceRange.init(context.script, identifier.range);
    defer source_range.deinit();

    if (identifier.value[0] == '_') {
        var receiver = context.self_object.getValue();

        if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
            receiver = receiver.asObject().asActivationObject().findActivationReceiver();
        }

        var tracked_receiver = try context.vm.heap.track(receiver);
        defer tracked_receiver.untrack(context.vm.heap);

        return message_interpreter.executePrimitiveMessage(context, tracked_receiver, identifier.value, &.{}, source_range);
    }

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    {
        var receiver = context.self_object.getValue();
        if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
            receiver = receiver.asObject().asActivationObject().findActivationReceiver();
        }

        if (receiver.isObjectReference() and
            receiver.asObject().isBlockObject() and
            receiver.asObject().asBlockObject().isCorrectMessageForBlockExecution(identifier.value))
        {
            var tracked_receiver = try context.vm.heap.track(receiver);
            defer tracked_receiver.untrack(context.vm.heap);

            return message_interpreter.executeBlockMessage(context, tracked_receiver, &.{}, source_range);
        }
    }

    if (try context.self_object.getValue().lookup(.Read, context, identifier.value, source_range)) |lookup_completion| {
        if (!lookup_completion.isNormal()) {
            return lookup_completion;
        }

        const lookup_result = lookup_completion.data.Normal;
        if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
            var tracked_lookup_result = try context.vm.heap.track(lookup_result);
            defer tracked_lookup_result.untrack(context.vm.heap);

            return message_interpreter.executeMethodMessage(
                context,
                context.self_object,
                tracked_lookup_result,
                &.{},
                source_range,
            );
        } else {
            return Completion.initNormal(lookup_result);
        }
    } else {
        return Completion.initRuntimeError(context.vm, source_range, "Failed looking up \"{s}\"", .{identifier.value});
    }
}

/// Executes a string literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeString(context: *InterpreterContext, string: AST.StringNode) InterpreterError!Completion {
    try context.vm.heap.ensureSpaceInEden(
        ByteArray.requiredSizeForAllocation(string.value.len) +
            Object.Map.ByteArray.requiredSizeForAllocation() +
            Object.ByteArray.requiredSizeForAllocation(),
    );

    const byte_array = try ByteArray.createFromString(context.vm.heap, string.value);
    const byte_array_map = try Object.Map.ByteArray.create(context.vm.heap, byte_array);
    return Completion.initNormal((try Object.ByteArray.create(context.vm.heap, byte_array_map)).asValue());
}

/// Executes a number literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeNumber(context: *InterpreterContext, number: AST.NumberNode) InterpreterError!Completion {
    _ = context;

    return Completion.initNormal(switch (number.value) {
        .Integer => Value.fromInteger(number.value.Integer),
        .FloatingPoint => Value.fromFloatingPoint(number.value.FloatingPoint),
    });
}
