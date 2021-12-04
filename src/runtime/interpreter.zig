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

const message_interpreter = @import("./interpreter/message.zig");

// FIXME: Track the self object and lobby in the heap
pub const InterpreterContext = struct {
    /// The object that is the current context. The identifier "self" will
    /// resolve to this object.
    self_object: Value,
    /// The root of the current Self world.
    lobby: Value,
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
        value: Value,
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
pub fn executeScript(allocator: *Allocator, heap: *Heap, script: Script.Ref, lobby: Value) InterpreterError!?Value {
    defer script.unref();

    var activation_stack = std.ArrayList(*Activation).init(allocator);
    defer activation_stack.deinit();
    errdefer {
        for (activation_stack.items) |activation| {
            activation.destroy();
        }
    }

    heap.setActivationStack(&activation_stack);
    defer heap.setActivationStack(null);

    var context = InterpreterContext{
        .self_object = lobby,
        .lobby = lobby,
        .activation_stack = &activation_stack,
        .script = script,
        .current_error = null,
        .current_nonlocal_return = null,
    };
    var last_expression_result: ?Value = null;
    for (script.value.ast_root.?.statements) |statement| {
        std.debug.assert(activation_stack.items.len == 0);

        // FIXME: stop tracking the last expression result

        const expression_result = executeStatement(allocator, heap, statement, &context) catch |err| {
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
                    context.current_nonlocal_return.?.target_activation.deinit();
                    // FIXME: Stop tracking the current non-local return's value

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
pub fn executeSubScript(allocator: *Allocator, heap: *Heap, script: Script.Ref, parent_context: *InterpreterContext) InterpreterError!?Value {
    defer script.unref();

    // FIXME: Track the parent context's lobby
    var child_context = InterpreterContext{
        .self_object = parent_context.lobby,
        .lobby = parent_context.lobby,
        .activation_stack = parent_context.activation_stack,
        .script = script,
        .current_error = null,
        .current_nonlocal_return = null,
    };
    var last_expression_result: ?Value = null;
    for (script.value.ast_root.?.statements) |statement| {
        // FIXME: Stop tracking the last expression result

        const expression_result = executeStatement(allocator, heap, statement, &child_context) catch |err| {
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
pub fn executeStatement(allocator: *Allocator, heap: *Heap, statement: AST.StatementNode, context: *InterpreterContext) InterpreterError!Value {
    return try executeExpression(allocator, heap, statement.expression, context);
}

/// Executes an expression. All refs are forwarded.
pub fn executeExpression(allocator: *Allocator, heap: *Heap, expression: AST.ExpressionNode, context: *InterpreterContext) InterpreterError!Value {
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
    allocator: *Allocator,
    heap: *Heap,
    name: []const u8,
    object_node: AST.ObjectNode,
    arguments: [][]const u8,
    context: *InterpreterContext,
) InterpreterError!Value {
    // FIXME: Track these values, and untrack when the values are passed to the
    //        object/on error
    var assignable_slot_values = try std.ArrayList(Value).initCapacity(allocator, arguments.len);
    defer assignable_slot_values.deinit();

    var statements = try std.ArrayList(AST.StatementNode).initCapacity(allocator, object_node.slots.len);
    errdefer {
        for (statements.items) |*statement| {
            statement.deinit(allocator);
        }
        statements.deinit();
    }

    for (object_node.statements) |statement| {
        var statement_copy = try ASTCopyVisitor.visitStatement(statement, allocator);
        errdefer statement_copy.deinit(allocator);

        try statements.append(statement_copy);
    }

    const method_name_in_heap = try ByteVector.createFromString(heap, name);
    context.script.ref();
    errdefer context.script.unref();
    // FIXME: Track the method map
    const method_map = try Object.Map.Method.create(
        heap,
        @intCast(u8, arguments.len),
        @intCast(u32, object_node.slots.len),
        statements.toOwnedSlice(),
        method_name_in_heap,
        context.script,
    );

    const argument_slots = method_map.getArgumentSlots();
    for (arguments) |argument, i| {
        const argument_in_heap = try ByteVector.createFromString(heap, argument);
        argument_slots[i].initMutable(Object.Map.Method, method_map, argument_in_heap, .NotParent);
        // FIXME: Track global nil here
        assignable_slot_values.appendAssumeCapacity(environment.globalNil());
    }

    for (object_node.slots) |slot_node, i| {
        // FIXME: Use the tracked method map here
        var slot_value = try executeSlot(allocator, heap, slot_node, Object.Map.Method, method_map, i, context);
        if (slot_value) |value| {
            // FIXME: Track the returned value here
            try assignable_slot_values.append(value);
        }
    }

    // FIXME: Extract the current memory locations of tracked assignable slots here
    var current_assignable_slot_values = try allocator.alloc(Value, assignable_slot_values.items.len);
    defer allocator.free(current_assignable_slot_values);
    for (assignable_slot_values.items) |value, i| {
        current_assignable_slot_values[i] = value;
    }

    // FIXME: Use the tracked method map here
    return (try Object.Method.create(heap, method_map, current_assignable_slot_values)).asValue();
}

/// Creates a new slot. All refs are forwarded.
pub fn executeSlot(
    allocator: *Allocator,
    heap: *Heap,
    slot_node: AST.SlotNode,
    comptime MapType: type,
    map: *MapType,
    slot_index: usize,
    context: *InterpreterContext,
) InterpreterError!?Value {
    var value = blk: {
        if (slot_node.value == .Object and slot_node.value.Object.statements.len > 0) {
            break :blk try executeMethod(allocator, heap, slot_node.name, slot_node.value.Object.*, slot_node.arguments, context);
        } else {
            break :blk try executeExpression(allocator, heap, slot_node.value, context);
        }
    };

    const slot_name = try ByteVector.createFromString(heap, slot_node.name);
    if (slot_node.is_mutable) {
        map.getSlots()[slot_index].initMutable(MapType, map, slot_name, if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent);
        return value;
    } else {
        map.getSlots()[slot_index].initConstant(slot_name, if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent, value);
        return null;
    }
}

/// Creates a new slots object. All refs are forwarded.
pub fn executeObject(allocator: *Allocator, heap: *Heap, object_node: AST.ObjectNode, context: *InterpreterContext) InterpreterError!Value {
    // Verify that we are executing a slots object and not a method; methods
    // are created through executeSlot.
    if (object_node.statements.len > 0) {
        @panic("!!! Attempted to execute a non-slots object! Methods must be created via executeSlot.");
    }

    // FIXME: Track values, and untrack on error/exit
    var assignable_slot_values = std.ArrayList(Value).init(allocator);
    defer assignable_slot_values.deinit();

    // FIXME: Track the slots map
    var slots_map = try Object.Map.Slots.create(heap, @intCast(u32, object_node.slots.len));

    for (object_node.slots) |slot_node, i| {
        // FIXME: Use the tracked slots map here
        var slot_value = try executeSlot(allocator, heap, slot_node, Object.Map.Slots, slots_map, i, context);
        if (slot_value) |value| {
            // FIXME: Track the returned value here
            try assignable_slot_values.append(value);
        }
    }

    // FIXME: Ensure enough memory for slots before value extraction

    // FIXME: Extract the current memory locations of tracked assignable slots here
    // FIXME: Does this have to be an allocation?
    var current_assignable_slot_values = try allocator.alloc(Value, assignable_slot_values.items.len);
    defer allocator.free(current_assignable_slot_values);

    for (assignable_slot_values.items) |value, i| {
        current_assignable_slot_values[i] = value;
    }

    // FIXME: Use the tracked slots map here
    return (try Object.Slots.create(heap, slots_map, current_assignable_slot_values)).asValue();
}

pub fn executeBlock(allocator: *Allocator, heap: *Heap, block: AST.BlockNode, context: *InterpreterContext) InterpreterError!Value {
    var argument_slot_count: u8 = 0;
    for (block.slots) |slot_node| {
        if (slot_node.is_argument) argument_slot_count += 1;
    }

    // FIXME: Track slot values and untrack them on error/return
    var assignable_slot_values = try std.ArrayList(Value).initCapacity(allocator, argument_slot_count);
    defer assignable_slot_values.deinit();

    var statements = try std.ArrayList(AST.StatementNode).initCapacity(allocator, block.statements.len);
    errdefer {
        for (statements.items) |*statement| {
            statement.deinit(allocator);
        }
        statements.deinit();
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
    // FIXME: Track the block map
    const block_map = try Object.Map.Block.create(
        heap,
        argument_slot_count,
        @intCast(u32, block.slots.len) - argument_slot_count,
        statements.toOwnedSlice(),
        parent_activation,
        nonlocal_return_target_activation,
        context.script,
    );

    // Add all the argument slots
    for (block.slots) |slot_node, i| {
        if (slot_node.is_argument) {
            const slot_name = try ByteVector.createFromString(heap, slot_node.name);
            // FIXME: Use the tracked block map
            block_map.getArgumentSlots()[i].initMutable(
                Object.Map.Block,
                block_map,
                slot_name,
                if (slot_node.is_parent) Slot.ParentFlag.Parent else Slot.ParentFlag.NotParent,
            );
            // FIXME: Track the global nil
            assignable_slot_values.appendAssumeCapacity(environment.globalNil());
        }
    }

    // Add all the non-argument slots
    for (block.slots) |slot_node, i| {
        if (!slot_node.is_argument) {
            // FIXME: Use the tracked block map
            var slot_value = try executeSlot(allocator, heap, slot_node, Object.Map.Block, block_map, i, context);
            if (slot_value) |value| {
                // FIXME: Track the returned value
                try assignable_slot_values.append(value);
            }
        }
    }

    // FIXME: Ensure enough space for block object before value extraction

    // FIXME: Extract the current memory locations of tracked assignable slots here
    var current_assignable_slot_values = try allocator.alloc(Value, assignable_slot_values.items.len);
    defer allocator.free(current_assignable_slot_values);
    for (assignable_slot_values.items) |value, i| {
        current_assignable_slot_values[i] = value;
    }

    return (try Object.Block.create(heap, block_map, current_assignable_slot_values)).asValue();
}

pub fn executeReturn(allocator: *Allocator, heap: *Heap, return_node: AST.ReturnNode, context: *InterpreterContext) InterpreterError {
    _ = heap;
    const latest_activation = context.activation_stack.items[context.activation_stack.items.len - 1];
    const target_activation = latest_activation.nonlocal_return_target_activation.?;
    std.debug.assert(target_activation.nonlocal_return_target_activation == null);

    const value = try executeExpression(allocator, heap, return_node.expression, context);
    const target_activation_weak = target_activation.makeWeakRef();
    context.current_nonlocal_return = .{ .target_activation = target_activation_weak, .value = value };

    return NonlocalReturnError.NonlocalReturn;
}

/// Executes an identifier expression. If the looked up value exists, the value
/// gains a ref. `self_object` gains a ref during a method execution.
pub fn executeIdentifier(allocator: *Allocator, heap: *Heap, identifier: AST.IdentifierNode, context: *InterpreterContext) InterpreterError!Value {
    _ = heap;
    if (identifier.value[0] == '_') {
        var receiver = context.self_object;

        if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
            receiver = receiver.asObject().asActivationObject().findActivationReceiver();
        }

        return try message_interpreter.executePrimitiveMessage(allocator, heap, identifier.range, receiver, identifier.value, &[_]Value{}, context);
    }

    // Check for block activation. Note that this isn't the same as calling a
    // method on traits block, this is actually executing the block itself via
    // the virtual method.
    {
        var receiver = context.self_object;
        if (receiver.isObjectReference() and receiver.asObject().isActivationObject()) {
            receiver = receiver.asObject().asActivationObject().findActivationReceiver();
        }

        if (receiver.value.is(.Block) and receiver.value.isCorrectMessageForBlockExecution(identifier.value)) {
            return try message_interpreter.executeBlockMessage(allocator, heap, identifier.range, receiver, &[_]Value{}, context);
        }
    }

    if (try context.self_object.lookup(allocator, context, identifier.value, .Value)) |value| {
        if (value.isObjectReference() and value.asObject().isMethodObject()) {
            return try message_interpreter.executeMethodMessage(allocator, heap, identifier.range, context.self_object, value, &[_]Value{}, context);
        } else {
            return value;
        }
    } else {
        return runtime_error.raiseError(allocator, context, "Failed looking up \"{s}\"", .{identifier.value});
    }
}

/// Executes a string literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeString(allocator: *Allocator, heap: *Heap, string: AST.StringNode, context: *InterpreterContext) InterpreterError!Value {
    _ = allocator;
    _ = heap;
    _ = context;

    // FIXME: Track the byte vector
    const byte_vector = try ByteVector.createFromString(heap, string.value);
    // FIXME: Track the byte vector map
    const byte_vector_map = try Object.Map.ByteVector.create(heap, byte_vector);
    return (try Object.ByteVector.create(heap, byte_vector_map)).asValue();
}

/// Executes a number literal expression. `lobby` gains a ref during the
/// lifetime of the function.
pub fn executeNumber(allocator: *Allocator, heap: *Heap, number: AST.NumberNode, context: *InterpreterContext) InterpreterError!Value {
    _ = allocator;
    _ = heap;
    _ = context;

    return switch (number.value) {
        .Integer => Value.fromInteger(number.value.Integer),
        .FloatingPoint => Value.fromFloatingPoint(number.value.FloatingPoint),
    };
}
