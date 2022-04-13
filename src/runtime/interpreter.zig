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
const ProtoSlot = @import("./slot.zig").ProtoSlot;
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
pub const MaximumInheritedSlotsInObject = 2;

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

/// Executes a script node. The last statement result is returned. If no
/// statements were available or there was an error, null is returned.
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
    var last_expression_result: ?Heap.Tracked = try vm.heap.track(vm.nil());
    for (script.value.ast_root.?.statements.value.statements) |expression| {
        std.debug.assert(activation_stack.depth == 0);

        if (last_expression_result) |result| {
            result.untrack(vm.heap);
        }

        var completion = try executeExpression(&context, expression);
        defer completion.deinit(vm);

        switch (completion.data) {
            .Normal => |value| {
                last_expression_result = try vm.heap.track(value);
            },
            .RuntimeError => |err| {
                if (!vm.silent_errors) {
                    std.debug.print("Received error at top level: {s}\n", .{err.message});
                    runtime_error.printTraceFromActivationStack(activation_stack.getStack(), err.source_range);
                }
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
    for (script.value.ast_root.?.statements.value.statements) |expression| {
        if (last_expression_result) |result| {
            result.untrack(parent_context.vm.heap);
        }

        var did_complete_statement_successfully = false;
        var completion = try executeExpression(&child_context, expression);
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

const InheritedSlotValues = std.BoundedArray(Heap.Tracked, MaximumInheritedSlotsInObject);

/// Creates a new slot and adds it to the given map builder.
pub fn executeSlot(
    context: *InterpreterContext,
    map_builder: anytype,
    slot_node: AST.SlotNode,
    inherited_slot_values: ?*InheritedSlotValues,
) InterpreterError!?Completion {
    {
        const map_builder_type = @typeInfo(@TypeOf(map_builder)).Pointer.child;
        if (!@hasDecl(map_builder_type, "is_map_builder"))
            @compileError("executeSlot must be called with a map builder");
    }

    var inherited_slot_value_offset: usize = 0;

    const completion = blk: {
        if (slot_node.is_inherited) {
            var inherited_slot_value = inherited_slot_values.?.constSlice()[inherited_slot_value_offset].getValue();
            inherited_slot_value_offset += 1;
            break :blk Completion.initNormal(inherited_slot_value);
        }

        if (slot_node.value) |value| {
            if (value == .Object) {
                var has_argument = false;
                for (value.Object.slots) |slot| {
                    if (slot.is_argument) {
                        has_argument = true;
                        break;
                    }
                }

                if (value.Object.statements.value.statements.len > 0 or has_argument)
                    break :blk try executeMethod(context, slot_node.name, value.Object.*);
            }

            break :blk try executeExpression(context, value);
        }

        break :blk Completion.initNormal(context.vm.nil());
    };
    if (!completion.isNormal()) {
        return completion;
    }

    // In case the slot name allocation causes a GC.
    const tracked_value = try context.vm.heap.track(completion.data.Normal);
    defer tracked_value.untrack(context.vm.heap);

    const slot_name = try ByteArray.createFromString(context.vm.heap, slot_node.name);
    const slot = blk: {
        if (slot_node.is_inherited) {
            break :blk Slot.initInherited(slot_name, tracked_value.getValue());
        } else if (slot_node.is_argument) {
            break :blk Slot.initArgument(slot_name);
        } else if (slot_node.is_mutable) {
            break :blk Slot.initAssignable(slot_name, if (slot_node.is_parent) .Parent else .NotParent, tracked_value.getValue());
        } else {
            break :blk Slot.initConstant(slot_name, if (slot_node.is_parent) .Parent else .NotParent, tracked_value.getValue());
        }
    };

    try map_builder.addSlot(slot);
    return null;
}

/// Takes a map type, creates a map builder from it, and executes all the given
/// slots. Returns an object created from this map builder.
pub fn createSlotsObjectFromMap(
    context: *InterpreterContext,
    map: anytype,
    slots: []AST.SlotNode,
    inherited_slot_values: ?*InheritedSlotValues,
) InterpreterError!Completion {
    // Make sure that we are really being called with a pointer to a map type.
    blk: {
        const map_type = @typeInfo(@TypeOf(map)).Pointer.child;
        inline for (comptime std.meta.declarations(Object.Map)) |decl| {
            if (@field(Object.Map, decl.name) == map_type)
                break :blk;
        }

        @compileError("createSlotsObjectFromMap must be called with a map");
    }

    var map_builder = try map.getMapBuilder(context.vm.heap);
    defer map_builder.deinit();

    for (slots) |slot_node| {
        if (try executeSlot(context, &map_builder, slot_node, inherited_slot_values)) |completion|
            return completion;
    }

    const object = try map_builder.createObject();
    return Completion.initNormal(object.asValue());
}

/// Return how many slots the map will need to allocate.
fn getSlotCountForMap(
    context: *InterpreterContext,
    slots: []AST.SlotNode,
    inherited_slot_values: ?*InheritedSlotValues,
    out_slot_count: *u32,
) !?Completion {
    out_slot_count.* = 0;

    for (slots) |slot_node, i| {
        const proto_slot = blk: {
            if (slot_node.is_inherited) {
                const inherited_value_completion = try executeExpression(context, slot_node.value.?);
                if (!inherited_value_completion.isNormal()) {
                    return inherited_value_completion;
                }

                const inherited_value = inherited_value_completion.data.Normal;
                std.debug.assert(inherited_value.isObjectReference() and inherited_value.asObject().isSlotsObject());

                inherited_slot_values.?.appendAssumeCapacity(try context.vm.heap.track(inherited_value));
                break :blk ProtoSlot.initInherited(slot_node.name, inherited_value);
            } else if (slot_node.is_argument) {
                break :blk ProtoSlot.initArgument(slot_node.name);
            } else if (slot_node.is_mutable) {
                break :blk ProtoSlot.initAssignable(slot_node.name, if (slot_node.is_parent) .Parent else .NotParent);
            } else {
                break :blk ProtoSlot.initConstant(slot_node.name, if (slot_node.is_parent) .Parent else .NotParent);
            }
        };

        const inherited_slot_values_slice = if (inherited_slot_values) |values|
            values.constSlice()
        else
            &[_]Heap.Tracked{};

        out_slot_count.* += proto_slot.requiredSlotSpace(slots[0..i], inherited_slot_values_slice);
    }

    return null;
}

pub fn executeObject(context: *InterpreterContext, object_node: AST.ObjectNode) InterpreterError!Completion {
    // Verify that we are executing a slots object and not a method; methods
    // are created through executeMethod.
    if (object_node.statements.value.statements.len > 0) {
        @panic("!!! Attempted to execute a non-slots object! Methods must be created via executeMethod.");
    }

    var source_range = SourceRange.init(context.script, object_node.range);
    defer source_range.deinit();

    // Verify that the assignable slots in this object are within limits
    var assignable_slot_count: usize = 0;
    var inherited_slot_count: usize = 0;
    for (object_node.slots) |slot| {
        if (slot.is_inherited) inherited_slot_count += 1;
        if (slot.is_mutable) assignable_slot_count += 1;
    }
    if (assignable_slot_count > MaximumAssignableSlots)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for slots object", .{});
    if (inherited_slot_count > MaximumInheritedSlotsInObject)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum inherited slot limit exceeded for slots object", .{});

    var inherited_slot_values = InheritedSlotValues.init(0) catch unreachable;
    defer for (inherited_slot_values.constSlice()) |value| {
        value.untrack(context.vm.heap);
    };

    var slot_count: u32 = undefined;
    if (try getSlotCountForMap(context, object_node.slots, &inherited_slot_values, &slot_count)) |completion|
        return completion;

    var slots_map = try Object.Map.Slots.create(context.vm.heap, slot_count);
    return try createSlotsObjectFromMap(context, slots_map, object_node.slots, &inherited_slot_values);
}

fn executeMethod(context: *InterpreterContext, name: []const u8, object_node: AST.ObjectNode) InterpreterError!Completion {
    // FIXME: Get a better source range for the method itself
    var source_range = SourceRange.init(context.script, object_node.range);
    defer source_range.deinit();

    var assignable_slot_count: usize = 0;
    var argument_slot_count: usize = 0;
    for (object_node.slots) |slot| {
        if (slot.is_argument) argument_slot_count += 1;
        if (slot.is_mutable) assignable_slot_count += 1;
    }

    if (argument_slot_count > MaximumArguments)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum argument limit exceeded for method", .{});
    if (assignable_slot_count > MaximumAssignableSlots)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for method", .{});

    // This will prevent garbage collections until the map is created.
    var required_memory: usize = ByteArray.requiredSizeForAllocation(name.len);
    required_memory += Object.Map.Method.requiredSizeForAllocation(@intCast(u32, object_node.slots.len));
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
        var slot_count: u32 = undefined;
        if (try getSlotCountForMap(context, object_node.slots, null, &slot_count)) |completion|
            return completion;

        break :blk try Object.Map.Method.create(
            context.vm.heap,
            @intCast(u8, argument_slot_count),
            slot_count,
            object_node.statements,
            method_name_in_heap,
            context.script,
        );
    };

    return try createSlotsObjectFromMap(context, method_map, object_node.slots, null);
}

pub fn executeBlock(context: *InterpreterContext, block: AST.BlockNode) InterpreterError!Completion {
    var source_range = SourceRange.init(context.script, block.range);
    defer source_range.deinit();

    var argument_slot_count: usize = 0;
    var assignable_slot_count: usize = 0;
    for (block.slots) |slot_node| {
        if (slot_node.is_argument) argument_slot_count += 1;
        if (slot_node.is_mutable) assignable_slot_count += 1;
    }

    if (argument_slot_count > MaximumArguments)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum argument limit exceeded for block", .{});
    if (assignable_slot_count > MaximumAssignableSlots)
        return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for block", .{});

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
    try context.vm.heap.ensureSpaceInEden(required_memory);

    context.script.ref();
    block.statements.ref();
    // NOTE: Once we create the block map successfully, the ref we just created
    // above is owned by the block_map, and we shouldn't try to unref in case
    // of an error.
    var block_map = blk: {
        errdefer context.script.unref();
        errdefer block.statements.unrefWithAllocator(context.vm.allocator);

        var slot_count: u32 = undefined;
        if (try getSlotCountForMap(context, block.slots, null, &slot_count)) |completion|
            return completion;

        break :blk try Object.Map.Block.create(
            context.vm.heap,
            @intCast(u8, argument_slot_count),
            slot_count,
            block.statements,
            parent_activation,
            nonlocal_return_target_activation,
            context.script,
        );
    };

    return try createSlotsObjectFromMap(context, block_map, block.slots, null);
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

pub fn executeNumber(context: *InterpreterContext, number: AST.NumberNode) InterpreterError!Completion {
    _ = context;

    return Completion.initNormal(switch (number.value) {
        .Integer => Value.fromInteger(number.value.Integer),
        .FloatingPoint => Value.fromFloatingPoint(number.value.FloatingPoint),
    });
}
