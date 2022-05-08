// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../language/ast.zig");
const debug = @import("../debug.zig");
const Block = @import("./astcode/Block.zig");
const Script = @import("../language/script.zig");
const Instruction = @import("./astcode/Instruction.zig");
const Executable = @import("./astcode/Executable.zig");
const RegisterLocation = @import("./astcode/register_location.zig").RegisterLocation;

const AST_EXECUTABLE_DUMP_DEBUG = debug.AST_EXECUTABLE_DUMP_DEBUG;

// FIXME: Move this to a constants file
pub const MaximumArguments = 128; // Reasonable limit
pub const MaximumAssignableSlots = 255;

method_execution_depth: usize = 0,

const AstGen = @This();
const AstGenError = Allocator.Error || error{AstGenFailure};

/// Given a script, generate an Executable for it with its entrypoint being the
/// first block it contains.
pub fn generateExecutableFromScript(allocator: Allocator, script: Script.Ref) AstGenError!*Executable {
    const executable = try Executable.create(allocator, script);
    errdefer executable.destroy();

    var g = AstGen{};
    try g.generateScript(executable, script.value.ast_root.?);

    if (AST_EXECUTABLE_DUMP_DEBUG)
        std.debug.print("Executable dump: {}\n", .{executable});

    return executable;
}

fn generateScript(self: *AstGen, executable: *Executable, script_node: ast.ScriptNode) AstGenError!void {
    const script_block_index = try executable.makeBlock();
    const script_block = executable.getBlock(script_block_index);
    const last_expression_location = try self.generateStatementList(executable, script_block, script_node.statements.value.statements);

    _ = try script_block.addInstruction(executable.allocator, Instruction.exitActivation(last_expression_location));
}

fn generateStatementList(self: *AstGen, executable: *Executable, block: *Block, statements: []ast.ExpressionNode) AstGenError!RegisterLocation {
    var last_expression_location = RegisterLocation.Nil;
    for (statements) |expression| {
        last_expression_location = try self.generateExpression(executable, block, expression);
    }

    return last_expression_location;
}

fn generateExpression(self: *AstGen, executable: *Executable, block: *Block, expression: ast.ExpressionNode) AstGenError!RegisterLocation {
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

fn generateObject(self: *AstGen, executable: *Executable, block: *Block, object: *ast.ObjectNode) AstGenError!RegisterLocation {
    // Verify that we are generating bytecode for a slots object and not a
    // method; methods are generated through generateMethod.
    if (object.statements.value.statements.len > 0) {
        @panic("!!! Attempted to generate a slots object from a non-slots object! Methods must codegen via generateMethod.");
    }

    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    var assignable_slot_count: usize = 0;
    for (object.slots) |slot| {
        if (slot.is_mutable) assignable_slot_count += 1;
    }

    if (assignable_slot_count > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for slots object", .{});
        return error.AstGenFailure;
    }

    try self.generateSlotList(executable, block, object.slots);

    _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(object.range));
    const object_location = try block.addInstruction(executable.allocator, Instruction.createObject(@intCast(u32, object.slots.len)));

    if (std.debug.runtime_safety)
        _ = try block.addInstruction(executable.allocator, Instruction.verifySlotSentinel());

    return object_location;
}

fn generateBlock(self: *AstGen, executable: *Executable, block: *Block, block_node: *ast.BlockNode) AstGenError!RegisterLocation {
    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, block_node.slots, block_node.statements.value.statements);
    _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(block_node.range));
    const block_location = try block.addInstruction(executable.allocator, Instruction.createBlock(@intCast(u32, block_node.slots.len), block_index));

    if (std.debug.runtime_safety)
        _ = try block.addInstruction(executable.allocator, Instruction.verifySlotSentinel());

    return block_location;
}

fn generateMethod(self: *AstGen, executable: *Executable, block: *Block, method_name: []const u8, method: *ast.ObjectNode) AstGenError!RegisterLocation {
    self.method_execution_depth += 1;
    defer self.method_execution_depth -= 1;

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, method.slots, method.statements.value.statements);

    _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(method.range));
    const method_name_location = try block.addInstruction(executable.allocator, Instruction.createByteArray(method_name));

    if (self.method_execution_depth > 1) {
        _ = try block.addInstruction(executable.allocator, Instruction.setMethodInline());
    }

    const method_location = try block.addInstruction(executable.allocator, Instruction.createMethod(method_name_location, @intCast(u32, method.slots.len), block_index));

    if (std.debug.runtime_safety)
        _ = try block.addInstruction(executable.allocator, Instruction.verifySlotSentinel());

    return method_location;
}

fn generateArgumentList(self: *AstGen, executable: *Executable, block: *Block, arguments: []ast.ExpressionNode) !void {
    if (std.debug.runtime_safety)
        _ = try block.addInstruction(executable.allocator, Instruction.pushArgumentSentinel());

    for (arguments) |argument| {
        const argument_location = try self.generateExpression(executable, block, argument);
        _ = try block.addInstruction(executable.allocator, Instruction.pushArg(argument_location));
    }
}

fn generateMessage(self: *AstGen, executable: *Executable, block: *Block, message: *ast.MessageNode) AstGenError!RegisterLocation {
    const message_location = message_location: {
        if (message.receiver) |receiver| {
            const receiver_location = try self.generateExpression(executable, block, receiver);
            try self.generateArgumentList(executable, block, message.arguments);

            _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(message.range));
            break :message_location if (message.message_name[0] == '_')
                try block.addInstruction(executable.allocator, Instruction.primSend(receiver_location, message.message_name[1..]))
            else
                try block.addInstruction(executable.allocator, Instruction.send(receiver_location, message.message_name));
        }

        try self.generateArgumentList(executable, block, message.arguments);

        _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(message.range));
        break :message_location if (message.message_name[0] == '_')
            try block.addInstruction(executable.allocator, Instruction.selfPrimSend(message.message_name[1..]))
        else
            try block.addInstruction(executable.allocator, Instruction.selfSend(message.message_name));
    };

    if (std.debug.runtime_safety)
        _ = try block.addInstruction(executable.allocator, Instruction.verifyArgumentSentinel());
    return message_location;
}

fn generateReturn(self: *AstGen, executable: *Executable, block: *Block, return_node: *ast.ReturnNode) AstGenError!RegisterLocation {
    const expr_location = try self.generateExpression(executable, block, return_node.expression);
    _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(return_node.range));
    _ = try block.addInstruction(executable.allocator, Instruction.nonlocalReturn(expr_location));
    return RegisterLocation.Nil;
}

// TODO: Evaluate ownership of the string for generateIdentifier and
//       generateString. Right now since the lifetime of the script is tied to
//       the lifetime of the executable it will be alive, in the future we might
//       want to delete the AST after codegen.

fn generateIdentifier(self: *AstGen, executable: *Executable, block: *Block, identifier: ast.IdentifierNode) AstGenError!RegisterLocation {
    _ = self;

    _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(identifier.range));
    return if (identifier.value[0] == '_')
        try block.addInstruction(executable.allocator, Instruction.selfPrimSend(identifier.value[1..]))
    else
        try block.addInstruction(executable.allocator, Instruction.selfSend(identifier.value));
}

fn generateString(self: *AstGen, executable: *Executable, block: *Block, string: ast.StringNode) AstGenError!RegisterLocation {
    _ = self;

    _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(string.range));
    return try block.addInstruction(executable.allocator, Instruction.createByteArray(string.value));
}

fn generateNumber(self: *AstGen, executable: *Executable, block: *Block, number: ast.NumberNode) AstGenError!RegisterLocation {
    _ = self;

    _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(number.range));
    return switch (number.value) {
        .Integer => |i| try block.addInstruction(executable.allocator, Instruction.createInteger(i)),
        .FloatingPoint => |f| try block.addInstruction(executable.allocator, Instruction.createFloatingPoint(f)),
    };
}

fn generateSlotsAndCodeCommon(self: *AstGen, executable: *Executable, parent_block: *Block, slots: []ast.SlotNode, statements: []ast.ExpressionNode) AstGenError!u32 {
    var assignable_slot_count: usize = 0;
    var argument_slot_count: usize = 0;
    for (slots) |slot| {
        if (slot.is_mutable) assignable_slot_count += 1;
        if (slot.is_argument) argument_slot_count += 1;
    }

    if (argument_slot_count > MaximumArguments) {
        // FIXME: Return rich errors
        // return Completion.initRuntimeError(context.vm, source_range, "Maximum argument slot limit exceeded for block object", .{});
        return error.AstGenFailure;
    }

    if (assignable_slot_count > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return Completion.initRuntimeError(context.vm, source_range, "Maximum assignable slot limit exceeded for block object", .{});
        return error.AstGenFailure;
    }

    try self.generateSlotList(executable, parent_block, slots);

    const child_block_index = try executable.makeBlock();
    const child_block = executable.getBlock(child_block_index);

    const last_expression_location = try self.generateStatementList(executable, child_block, statements);
    _ = try child_block.addInstruction(executable.allocator, Instruction.exitActivation(last_expression_location));

    return child_block_index;
}

fn generateSlotList(self: *AstGen, executable: *Executable, block: *Block, slots: []ast.SlotNode) AstGenError!void {
    if (std.debug.runtime_safety)
        _ = try block.addInstruction(executable.allocator, Instruction.pushSlotSentinel());

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

        _ = try block.addInstruction(executable.allocator, Instruction.sourceRange(slot.range));
        const slot_name_index = try block.addInstruction(executable.allocator, Instruction.createByteArray(slot.name));

        if (slot.is_inherited) {
            std.debug.assert(!slot.is_mutable);
            std.debug.assert(!slot.is_argument);
            std.debug.assert(!slot.is_parent);

            _ = try block.addInstruction(executable.allocator, Instruction.pushInheritedSlot(slot_name_index, slot_value_index));
        } else if (slot.is_argument) {
            std.debug.assert(slot.is_mutable);
            std.debug.assert(!slot.is_parent);

            _ = try block.addInstruction(executable.allocator, Instruction.pushArgumentSlot(slot_name_index, slot_value_index));
        } else if (slot.is_mutable) {
            _ = try block.addInstruction(executable.allocator, Instruction.pushAssignableSlot(slot_name_index, slot.is_parent, slot_value_index));
        } else {
            _ = try block.addInstruction(executable.allocator, Instruction.pushConstantSlot(slot_name_index, slot.is_parent, slot_value_index));
        }
    }
}
