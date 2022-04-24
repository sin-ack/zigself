// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../language/ast.zig");
const debug = @import("../debug.zig");
const Block = @import("./bytecode/Block.zig");
const Script = @import("../language/script.zig");
const Opcode = @import("./bytecode/Opcode.zig");
const Executable = @import("./bytecode/Executable.zig");
const RegisterLocation = @import("./bytecode/register_location.zig").RegisterLocation;

const EXECUTABLE_DUMP_DEBUG = debug.EXECUTABLE_DUMP_DEBUG;

// FIXME: Move this to a constants file
pub const MaximumArguments = 128; // Reasonable limit
pub const MaximumAssignableSlots = 255;

method_execution_depth: usize = 0,

const Codegen = @This();
const CodegenError = Allocator.Error || error{CodegenFailure};

/// Given a script, generate an Executable for it with its entrypoint being the
/// first block it contains.
pub fn generateExecutableFromScript(allocator: Allocator, script: Script.Ref) CodegenError!Executable.Ref {
    const executable = try Executable.create(allocator, script);
    errdefer executable.unref();

    var c = Codegen{};
    try c.generateScript(executable.value, script.value.ast_root.?);
    try executable.value.seal();

    if (EXECUTABLE_DUMP_DEBUG)
        std.debug.print("Executable dump: {}\n", .{executable.value});

    return executable;
}

fn generateScript(self: *Codegen, executable: *Executable, script_node: ast.ScriptNode) CodegenError!void {
    const script_block_index = try executable.makeBlock();
    const script_block = executable.getBlock(script_block_index);
    const last_expression_location = try self.generateStatementList(executable, script_block, script_node.statements.value.statements);
    _ = try script_block.addOpcode(executable.allocator, Opcode.exitActivation(last_expression_location));
}

fn generateStatementList(self: *Codegen, executable: *Executable, block: *Block, statements: []ast.ExpressionNode) CodegenError!RegisterLocation {
    var last_expression_location = RegisterLocation.Nil;
    for (statements) |expression| {
        last_expression_location = try self.generateExpression(executable, block, expression);
    }

    return last_expression_location;
}

fn generateExpression(self: *Codegen, executable: *Executable, block: *Block, expression: ast.ExpressionNode) CodegenError!RegisterLocation {
    return switch (expression) {
        .Object => |object| self.generateObject(executable, block, object),
        .Block => |block_node| self.generateBlock(executable, block, block_node),
        .Message => |message| self.generateMessage(executable, block, message),
        .Return => |return_node| self.generateReturn(executable, block, return_node),

        .Identifier => |identifier| self.generateIdentifier(executable, block, identifier),
        .String => |string| self.generateString(executable, block, string),
        .Number => |number| self.generateNumber(executable, block, number),
    };
}

fn generateObject(self: *Codegen, executable: *Executable, block: *Block, object: *ast.ObjectNode) CodegenError!RegisterLocation {
    // Verify that we are generating bytecode for a slots object and not a
    // method; methods are generated through generateMethod.
    if (object.statements.value.statements.len > 0) {
        @panic("!!! Attempted to generate a slots object from a non-slots object! Methods must codegen via generateMethod.");
    }

    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    // FIXME: Insert source range

    var assignable_slot_count: usize = 0;
    for (object.slots) |slot| {
        if (slot.is_mutable) assignable_slot_count += 1;
    }

    if (assignable_slot_count > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for slots object", .{});
        return error.CodegenFailure;
    }

    try self.generateSlotList(executable, block, object.slots);

    return try block.addOpcode(executable.allocator, Opcode.createObject(@intCast(u32, object.slots.len)));
}

fn generateBlock(self: *Codegen, executable: *Executable, block: *Block, block_node: *ast.BlockNode) CodegenError!RegisterLocation {
    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, block_node.slots, block_node.statements.value.statements);
    return try block.addOpcode(executable.allocator, Opcode.createBlock(@intCast(u32, block_node.slots.len), block_index));
}

fn generateMethod(self: *Codegen, executable: *Executable, block: *Block, method_name: []const u8, method: *ast.ObjectNode) CodegenError!RegisterLocation {
    self.method_execution_depth += 1;
    defer self.method_execution_depth -= 1;

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, method.slots, method.statements.value.statements);
    const method_name_location = try block.addOpcode(executable.allocator, Opcode.createByteArray(method_name));

    if (self.method_execution_depth > 1) {
        _ = try block.addOpcode(executable.allocator, Opcode.setMethodInline());
    }

    return try block.addOpcode(executable.allocator, Opcode.createMethod(method_name_location, @intCast(u32, method.slots.len), block_index));
}

fn generateArgumentList(self: *Codegen, executable: *Executable, block: *Block, arguments: []ast.ExpressionNode) !void {
    for (arguments) |argument| {
        const argument_location = try self.generateExpression(executable, block, argument);
        _ = try block.addOpcode(executable.allocator, Opcode.pushArg(argument_location));
    }
}

fn generateMessage(self: *Codegen, executable: *Executable, block: *Block, message: *ast.MessageNode) CodegenError!RegisterLocation {
    if (message.receiver) |receiver| {
        const receiver_location = try self.generateExpression(executable, block, receiver);
        try self.generateArgumentList(executable, block, message.arguments);

        return if (message.message_name[0] == '_')
            try block.addOpcode(executable.allocator, Opcode.primSend(receiver_location, message.message_name[1..]))
        else
            try block.addOpcode(executable.allocator, Opcode.send(receiver_location, message.message_name));
    }

    try self.generateArgumentList(executable, block, message.arguments);

    return if (message.message_name[0] == '_')
        try block.addOpcode(executable.allocator, Opcode.selfPrimSend(message.message_name[1..]))
    else
        try block.addOpcode(executable.allocator, Opcode.selfSend(message.message_name));
}

fn generateReturn(self: *Codegen, executable: *Executable, block: *Block, return_node: *ast.ReturnNode) CodegenError!RegisterLocation {
    const expr_location = try self.generateExpression(executable, block, return_node.expression);
    _ = try block.addOpcode(executable.allocator, Opcode.nonlocalReturn(expr_location));
    return RegisterLocation.Nil;
}

// TODO: Evaluate ownership of the string for generateIdentifier and
//       generateString. Right now since the lifetime of the script is tied to
//       the lifetime of the executable it will be alive, in the future we might
//       want to delete the AST after codegen.

fn generateIdentifier(self: *Codegen, executable: *Executable, block: *Block, identifier: ast.IdentifierNode) CodegenError!RegisterLocation {
    _ = self;

    return if (identifier.value[0] == '_')
        try block.addOpcode(executable.allocator, Opcode.selfPrimSend(identifier.value[1..]))
    else
        try block.addOpcode(executable.allocator, Opcode.selfSend(identifier.value));
}

fn generateString(self: *Codegen, executable: *Executable, block: *Block, string: ast.StringNode) CodegenError!RegisterLocation {
    _ = self;

    return try block.addOpcode(executable.allocator, Opcode.createByteArray(string.value));
}

fn generateNumber(self: *Codegen, executable: *Executable, block: *Block, number: ast.NumberNode) CodegenError!RegisterLocation {
    _ = self;

    return switch (number.value) {
        .Integer => |i| try block.addOpcode(executable.allocator, Opcode.createInteger(i)),
        .FloatingPoint => |f| try block.addOpcode(executable.allocator, Opcode.createFloatingPoint(f)),
    };
}

fn generateSlotsAndCodeCommon(self: *Codegen, executable: *Executable, parent_block: *Block, slots: []ast.SlotNode, statements: []ast.ExpressionNode) CodegenError!u32 {
    var assignable_slot_count: usize = 0;
    var argument_slot_count: usize = 0;
    for (slots) |slot| {
        if (slot.is_mutable) assignable_slot_count += 1;
        if (slot.is_argument) argument_slot_count += 1;
    }

    if (argument_slot_count > MaximumArguments) {
        // FIXME: Return rich errors
        // return Completion.initRuntimeError(context.vm, source_range, "Maximum argument slot limit exceeded for block object", .{});
        return error.CodegenFailure;
    }

    if (assignable_slot_count > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for block object", .{});
        return error.CodegenFailure;
    }

    try self.generateSlotList(executable, parent_block, slots);

    const child_block_index = try executable.makeBlock();
    const child_block = executable.getBlock(child_block_index);

    const last_expression_location = try self.generateStatementList(executable, child_block, statements);
    _ = try child_block.addOpcode(executable.allocator, Opcode.exitActivation(last_expression_location));

    return child_block_index;
}

fn generateSlotList(self: *Codegen, executable: *Executable, block: *Block, slots: []ast.SlotNode) CodegenError!void {
    for (slots) |slot| {
        const slot_value_index = blk: {
            if (slot.value) |value| {
                if (value == .Object) {
                    var has_argument = false;
                    for (value.Object.slots) |value_slot| {
                        if (value_slot.is_argument) {
                            has_argument = true;
                            break;
                        }
                    }

                    if (value.Object.statements.value.statements.len > 0 or has_argument)
                        break :blk try self.generateMethod(executable, block, slot.name, value.Object);
                }

                break :blk try self.generateExpression(executable, block, value);
            }

            break :blk RegisterLocation.Nil;
        };

        const slot_name_index = try block.addOpcode(executable.allocator, Opcode.createByteArray(slot.name));

        if (slot.is_inherited) {
            std.debug.assert(!slot.is_mutable);
            std.debug.assert(!slot.is_argument);
            std.debug.assert(!slot.is_parent);

            _ = try block.addOpcode(executable.allocator, Opcode.pushInheritedSlot(slot_name_index, slot_value_index));
        } else if (slot.is_argument) {
            std.debug.assert(slot.is_mutable);
            std.debug.assert(!slot.is_parent);

            _ = try block.addOpcode(executable.allocator, Opcode.pushArgumentSlot(slot_name_index, slot_value_index));
        } else if (slot.is_mutable) {
            _ = try block.addOpcode(executable.allocator, Opcode.pushAssignableSlot(slot_name_index, slot.is_parent, slot_value_index));
        } else {
            _ = try block.addOpcode(executable.allocator, Opcode.pushConstantSlot(slot_name_index, slot.is_parent, slot_value_index));
        }
    }
}
