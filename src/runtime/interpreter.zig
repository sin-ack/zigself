// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
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
const Activation = @import("./activation.zig");
const ByteVector = @import("./byte_vector.zig");
const environment = @import("./environment.zig");
const runtime_error = @import("./error.zig");
const ASTCopyVisitor = @import("../language/ast_copy_visitor.zig");
const ActivationStack = Activation.ActivationStack;
const Completion = @import("./completion.zig");

const message_interpreter = @import("./interpreter/message.zig");

pub const MaximumStackDepth = 2048;

pub const InterpreterContext = struct {
    /// The object that is the current context. The identifier "self" will
    /// resolve to this object.
    self_object: Heap.Tracked,
    /// The root of the current Self world.
    lobby: Heap.Tracked,
    /// The method/block activation stack. This is used with blocks in order to
    /// verify that the block is executed within its enclosing method and for
    /// stack traces. When the activation completes, the activation object is
    /// popped; when a new activation occurs, it is pushed.
    activation_stack: *ActivationStack,
    /// The script file that is currently executing, used to resolve the
    /// relative paths of other script files.
    script: Script.Ref,
    /// A mapping from argument counts to the related block message names.
    /// Since block names will not be unique, this mapping allows us to store
    /// a single instance of each message name for the respective block arities.
    block_message_names: *std.AutoArrayHashMapUnmanaged(u8, Heap.Tracked),
};

pub const InterpreterError = Allocator.Error;

/// Executes a script node. `lobby` is ref'd for the function lifetime. The last
/// expression result is returned, or if no statements were available, null is
/// returned.
///
/// Refs `script`.
pub fn executeScript(allocator: Allocator, heap: *Heap, script: Script.Ref, lobby: Value) InterpreterError!?Value {
    script.ref();
    defer script.unref();

    // Did this script execute normally?
    var did_execute_normally = false;

    var activation_stack = try ActivationStack.init(allocator, MaximumStackDepth);
    defer {
        if (!did_execute_normally) {
            // Since the execution was abruptly stopped the activation stack
            // wasn't properly unwound, so let's do that now.
            for (activation_stack.getStack()) |*activation| {
                activation.deinit();
            }
        }
        activation_stack.deinit(allocator);
    }

    heap.setActivationStack(&activation_stack);
    defer heap.setActivationStack(null);

    var tracked_lobby = try heap.track(lobby);
    defer tracked_lobby.untrack(heap);

    var block_message_names = std.AutoArrayHashMapUnmanaged(u8, Heap.Tracked){};
    defer {
        var it = block_message_names.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.untrack(heap);
        }

        block_message_names.deinit(allocator);
    }

    var context = InterpreterContext{
        .self_object = tracked_lobby,
        .lobby = tracked_lobby,
        .activation_stack = &activation_stack,
        .script = script,
        .block_message_names = &block_message_names,
    };
    var last_expression_result: ?Heap.Tracked = null;
    for (script.value.ast_root.?.statements) |statement| {
        std.debug.assert(activation_stack.depth == 0);

        if (last_expression_result) |result| {
            result.untrack(heap);
        }

        var completion = try executeStatement(allocator, heap, statement, &context);
        defer completion.deinit(allocator);

        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try heap.track(value);
            },
            .RuntimeError => |error_message| {
                std.debug.print("Received error at top level: {s}\n", .{error_message});
                runtime_error.printTraceFromActivationStack(activation_stack.getStack());
                return null;
            },
            .NonlocalReturn => {
                std.debug.print("A non-local return has bubbled up to the top! This is a bug!", .{});
                runtime_error.printTraceFromActivationStack(activation_stack.getStack());
                return null;
            },
            else => unreachable,
        }
    }

    did_execute_normally = true;
    if (last_expression_result) |result| {
        defer result.untrack(heap);
        return result.getValue();
    }

    return null;
}

/// Execute a script object as a child script of the root script. The parent
/// interpreter context is passed in order to preserve the activation stack and
/// various other context objects.
///
/// Refs `script`.
pub fn executeSubScript(allocator: Allocator, heap: *Heap, script: Script.Ref, parent_context: *InterpreterContext) InterpreterError!?Completion {
    script.ref();
    defer script.unref();

    var child_context = InterpreterContext{
        .self_object = parent_context.lobby,
        .lobby = parent_context.lobby,
        .activation_stack = parent_context.activation_stack,
        .script = script,
        .block_message_names = parent_context.block_message_names,
    };
    var last_expression_result: ?Heap.Tracked = null;
    for (script.value.ast_root.?.statements) |statement| {
        if (last_expression_result) |result| {
            result.untrack(heap);
        }

        var did_complete_statement_successfully = false;
        var completion = try executeStatement(allocator, heap, statement, &child_context);
        defer {
            if (did_complete_statement_successfully) {
                completion.deinit(allocator);
            }
        }

        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try heap.track(value);
                did_complete_statement_successfully = true;
            },
            .RuntimeError => {
                // Allow the error to keep bubbling up.
                return completion;
            },
            .NonlocalReturn => {
                return try Completion.initRuntimeError(allocator, "A non-local return has bubbled up to the top of a sub-script! This is a bug!", .{});
            },
            else => unreachable,
        }
    }

    if (last_expression_result) |result| {
        defer result.untrack(heap);
        return Completion.initNormal(result.getValue());
    }

    return null;
}

/// Executes a statement. All refs are forwardded.
pub fn executeStatement(allocator: Allocator, heap: *Heap, statement: AST.StatementNode, context: *InterpreterContext) InterpreterError!Completion {
    return try executeExpression(allocator, heap, statement.expression, context);
}

/// Executes an expression. All refs are forwarded.
pub fn executeExpression(allocator: Allocator, heap: *Heap, expression: AST.ExpressionNode, context: *InterpreterContext) InterpreterError!Completion {
    return switch (expression) {
        .Object => |object| try executeObject(allocator, heap, object.*, context),
        .Block => |block| try executeBlock(allocator, heap, block.*, context),
        .Message => |message| try message_interpreter.executeMessage(allocator, heap, message.*, context),
        .Return => |return_node| return executeReturn(allocator, heap, return_node.*, context),

        .Identifier => |identifier| try executeIdentifier(allocator, heap, identifier, context),
        .String => |string| try executeString(allocator, heap, string, context),
        .Number => |number| try executeNumber(allocator, heap, number, context),
    };
}

/// Creates a new method object. All refs are forwarded. `arguments` and
/// `object_node`'s statements are copied.
fn executeMethod(
    allocator: Allocator,
    heap: *Heap,
    name: []const u8,
    object_node: AST.ObjectNode,
    arguments: [][]const u8,
    context: *InterpreterContext,
) InterpreterError!Completion {
    var assignable_slot_values = try std.ArrayList(Heap.Tracked).initCapacity(allocator, arguments.len);
    defer {
        for (assignable_slot_values.items) |value| {
            value.untrack(heap);
        }
        assignable_slot_values.deinit();
    }

    // This will prevent garbage collections until the execution of slots at
    // least.
    var required_memory: usize = ByteVector.requiredSizeForAllocation(name.len);
    required_memory += Object.Map.Method.requiredSizeForAllocation(@intCast(u32, object_node.slots.len + arguments.len));
    for (arguments) |argument| {
        required_memory += ByteVector.requiredSizeForAllocation(argument.len);
    }

    try heap.ensureSpaceInEden(required_memory);

    context.script.ref();
    object_node.statements.ref();
    // NOTE: Once we create the method map successfully, the ref we just created
    // above is owned by the method_map, and we shouldn't try to unref in case
    // of an error.
    var method_map = blk: {
        errdefer context.script.unref();
        errdefer object_node.statements.unrefWithAllocator(allocator);

        const method_name_in_heap = try ByteVector.createFromString(heap, name);
        break :blk try Object.Map.Method.create(
            heap,
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
        const argument_in_heap = try ByteVector.createFromString(heap, argument);
        argument_slots[slot_init_offset].initMutable(Object.Map.Method, method_map, argument_in_heap, .NotParent);
        assignable_slot_values.appendAssumeCapacity(try heap.track(environment.globalNil()));

        slot_init_offset += 1;
    }

    const tracked_method_map = try heap.track(method_map.asValue());
    defer tracked_method_map.untrack(heap);

    for (object_node.slots) |slot_node| {
        const slot_completion = try executeSlot(allocator, heap, slot_node, Object.Map.Method, tracked_method_map.getValue().asObject().asMap().asMethodMap(), slot_init_offset, context);
        if (slot_completion) |completion| {
            if (completion.isNormal())
                try assignable_slot_values.append(try heap.track(completion.data.Normal))
            else
                return completion;
        }

        slot_init_offset += 1;
    }

    // Ensure that creating the method object won't cause a garbage collection
    // before the assignable slot values are copied in.
    try heap.ensureSpaceInEden(Object.Method.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.items.len)));

    var current_assignable_slot_values = try allocator.alloc(Value, assignable_slot_values.items.len);
    defer allocator.free(current_assignable_slot_values);
    for (assignable_slot_values.items) |value, i| {
        current_assignable_slot_values[i] = value.getValue();
    }

    const method_object = try Object.Method.create(heap, tracked_method_map.getValue().asObject().asMap().asMethodMap(), current_assignable_slot_values);
    return Completion.initNormal(method_object.asValue());
}

/// Creates a new slot. All refs are forwarded.
pub fn executeSlot(
    allocator: Allocator,
    heap: *Heap,
    slot_node: AST.SlotNode,
    comptime MapType: type,
    map: *MapType,
    slot_index: usize,
    context: *InterpreterContext,
) InterpreterError!?Completion {
    const completion = blk: {
        if (slot_node.value == .Object and slot_node.value.Object.statements.value.statements.len > 0) {
            break :blk try executeMethod(allocator, heap, slot_node.name, slot_node.value.Object.*, slot_node.arguments, context);
        } else {
            break :blk try executeExpression(allocator, heap, slot_node.value, context);
        }
    };
    if (!completion.isNormal()) {
        return completion;
    }

    const tracked_value = try heap.track(completion.data.Normal);
    defer tracked_value.untrack(heap);

    const slot_name = try ByteVector.createFromString(heap, slot_node.name);
    if (slot_node.is_mutable) {
        map.getSlots()[slot_index].initMutable(MapType, map, slot_name, if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent);
        return Completion.initNormal(tracked_value.getValue());
    } else {
        map.getSlots()[slot_index].initConstant(slot_name, if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent, tracked_value.getValue());
        return null;
    }
}

/// Creates a new slots object. All refs are forwarded.
pub fn executeObject(allocator: Allocator, heap: *Heap, object_node: AST.ObjectNode, context: *InterpreterContext) InterpreterError!Completion {
    // Verify that we are executing a slots object and not a method; methods
    // are created through executeSlot.
    if (object_node.statements.value.statements.len > 0) {
        @panic("!!! Attempted to execute a non-slots object! Methods must be created via executeSlot.");
    }

    var assignable_slot_values = std.ArrayList(Heap.Tracked).init(allocator);
    defer {
        for (assignable_slot_values.items) |value| {
            value.untrack(heap);
        }
        assignable_slot_values.deinit();
    }

    var slots_map = try Object.Map.Slots.create(heap, @intCast(u32, object_node.slots.len));
    const tracked_slots_map = try heap.track(slots_map.asValue());
    defer tracked_slots_map.untrack(heap);

    for (object_node.slots) |slot_node, i| {
        var slot_completion = try executeSlot(allocator, heap, slot_node, Object.Map.Slots, tracked_slots_map.getValue().asObject().asMap().asSlotsMap(), i, context);
        if (slot_completion) |completion| {
            if (completion.isNormal()) {
                try assignable_slot_values.append(try heap.track(completion.data.Normal));
            } else {
                return completion;
            }
        }
    }

    // Ensure that creating the slots object won't cause a garbage collection
    // before the assignable slot values are copied in.
    try heap.ensureSpaceInEden(Object.Slots.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.items.len)));

    var current_assignable_slot_values = try allocator.alloc(Value, assignable_slot_values.items.len);
    defer allocator.free(current_assignable_slot_values);
    for (assignable_slot_values.items) |value, i| {
        current_assignable_slot_values[i] = value.getValue();
    }

    const slots_object = try Object.Slots.create(heap, tracked_slots_map.getValue().asObject().asMap().asSlotsMap(), current_assignable_slot_values);
    return Completion.initNormal(slots_object.asValue());
}

pub fn executeBlock(allocator: Allocator, heap: *Heap, block: AST.BlockNode, context: *InterpreterContext) InterpreterError!Completion {
    var argument_slot_count: u8 = 0;
    for (block.slots) |slot_node| {
        if (slot_node.is_argument) argument_slot_count += 1;
    }

    var assignable_slot_values = try std.ArrayList(Heap.Tracked).initCapacity(allocator, argument_slot_count);
    defer {
        for (assignable_slot_values.items) |value| {
            value.untrack(heap);
        }
        assignable_slot_values.deinit();
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
            required_memory += ByteVector.requiredSizeForAllocation(slot_node.name.len);
        }
    }

    try heap.ensureSpaceInEden(required_memory);

    context.script.ref();
    block.statements.ref();
    // NOTE: Once we create the block map successfully, the ref we just created
    // above is owned by the block_map, and we shouldn't try to unref in case
    // of an error.
    var block_map = blk: {
        errdefer context.script.unref();
        errdefer block.statements.unrefWithAllocator(allocator);

        break :blk try Object.Map.Block.create(
            heap,
            argument_slot_count,
            @intCast(u32, block.slots.len) - argument_slot_count,
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
            const slot_name = try ByteVector.createFromString(heap, slot_node.name);

            argument_slots[slot_init_offset].initMutable(
                Object.Map.Block,
                block_map,
                slot_name,
                if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent,
            );
            assignable_slot_values.appendAssumeCapacity(try heap.track(environment.globalNil()));

            slot_init_offset += 1;
        }
    }

    const tracked_block_map = try heap.track(block_map.asValue());
    defer tracked_block_map.untrack(heap);

    // Add all the non-argument slots
    for (block.slots) |slot_node| {
        if (!slot_node.is_argument) {
            var slot_completion = try executeSlot(
                allocator,
                heap,
                slot_node,
                Object.Map.Block,
                tracked_block_map.getValue().asObject().asMap().asBlockMap(),
                slot_init_offset,
                context,
            );
            if (slot_completion) |completion| {
                if (completion.isNormal()) {
                    try assignable_slot_values.append(try heap.track(completion.data.Normal));
                } else {
                    return completion;
                }
            }

            slot_init_offset += 1;
        }
    }

    // Ensure that creating the block object won't cause a garbage collection
    // before the assignable slot values are copied in.
    try heap.ensureSpaceInEden(Object.Block.requiredSizeForAllocation(@intCast(u8, assignable_slot_values.items.len)));

    var current_assignable_slot_values = try allocator.alloc(Value, assignable_slot_values.items.len);
    defer allocator.free(current_assignable_slot_values);
    for (assignable_slot_values.items) |value, i| {
        current_assignable_slot_values[i] = value.getValue();
    }

    const block_object = try Object.Block.create(heap, tracked_block_map.getValue().asObject().asMap().asBlockMap(), current_assignable_slot_values);
    return Completion.initNormal(block_object.asValue());
}

pub fn executeReturn(allocator: Allocator, heap: *Heap, return_node: AST.ReturnNode, context: *InterpreterContext) InterpreterError!Completion {
    _ = heap;
    const latest_activation = context.activation_stack.stack[context.activation_stack.depth - 1];
    const target_activation = latest_activation.nonlocal_return_target_activation.?;
    std.debug.assert(target_activation.nonlocal_return_target_activation == null);

    const completion = try executeExpression(allocator, heap, return_node.expression, context);
    switch (completion.data) {
        .Normal => {
            const target_activation_weak = target_activation.makeWeakRef();
            return Completion.initNonlocalReturn(target_activation_weak, try heap.track(completion.data.Normal));
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
pub fn executeIdentifier(allocator: Allocator, heap: *Heap, identifier: AST.IdentifierNode, context: *InterpreterContext) InterpreterError!Completion {
    _ = heap;
    if (identifier.value[0] == '_') {
        var receiver = context.self_object.getValue();

        if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
            receiver = receiver.asObject().asActivationObject().findActivationReceiver();
        }

        var tracked_receiver = try heap.track(receiver);
        defer tracked_receiver.untrack(heap);

        return message_interpreter.executePrimitiveMessage(allocator, heap, identifier.range, tracked_receiver, identifier.value, &[_]Heap.Tracked{}, context);
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
            var tracked_receiver = try heap.track(receiver);
            defer tracked_receiver.untrack(heap);

            return message_interpreter.executeBlockMessage(allocator, heap, identifier.range, tracked_receiver, &[_]Heap.Tracked{}, context);
        }
    }

    if (try context.self_object.getValue().lookup(.Read, identifier.value, allocator, context)) |lookup_completion| {
        if (!lookup_completion.isNormal()) {
            return lookup_completion;
        }

        const lookup_result = lookup_completion.data.Normal;
        if (lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject()) {
            var tracked_lookup_result = try heap.track(lookup_result);
            defer tracked_lookup_result.untrack(heap);

            return message_interpreter.executeMethodMessage(
                allocator,
                heap,
                identifier.range,
                context.self_object,
                tracked_lookup_result,
                &[_]Heap.Tracked{},
                context,
            );
        } else {
            return Completion.initNormal(lookup_result);
        }
    } else {
        return Completion.initRuntimeError(allocator, "Failed looking up \"{s}\"", .{identifier.value});
    }
}

/// Executes a string literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeString(allocator: Allocator, heap: *Heap, string: AST.StringNode, context: *InterpreterContext) InterpreterError!Completion {
    _ = allocator;
    _ = heap;
    _ = context;

    try heap.ensureSpaceInEden(
        ByteVector.requiredSizeForAllocation(string.value.len) +
            Object.Map.ByteVector.requiredSizeForAllocation() +
            Object.ByteVector.requiredSizeForAllocation(),
    );

    const byte_vector = try ByteVector.createFromString(heap, string.value);
    const byte_vector_map = try Object.Map.ByteVector.create(heap, byte_vector);
    return Completion.initNormal((try Object.ByteVector.create(heap, byte_vector_map)).asValue());
}

/// Executes a number literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeNumber(allocator: Allocator, heap: *Heap, number: AST.NumberNode, context: *InterpreterContext) InterpreterError!Completion {
    _ = allocator;
    _ = heap;
    _ = context;

    return Completion.initNormal(switch (number.value) {
        .Integer => Value.fromInteger(number.value.Integer),
        .FloatingPoint => Value.fromFloatingPoint(number.value.FloatingPoint),
    });
}
