// Copyright (c) 2022-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../../language/ast.zig");
const debug = @import("../../debug.zig");
const Range = @import("../../language/Range.zig");
const Script = @import("../../language/Script.zig");
const astcode = @import("./astcode.zig");
const Selector = @import("../Selector.zig");
const primitives = @import("../primitives.zig");

const Block = astcode.Block;
const Executable = astcode.Executable;
const RegisterLocation = astcode.RegisterLocation;

const AST_EXECUTABLE_DUMP_DEBUG = debug.AST_EXECUTABLE_DUMP_DEBUG;

// FIXME: Move this to a constants file
pub const MaximumArguments = 128; // Reasonable limit
pub const MaximumAssignableSlots = 255;

method_execution_depth: usize = 0,
register_id_stack: std.ArrayList(u32),

const AstGen = @This();
const AstGenError = Allocator.Error || error{AstGenFailure};

/// Given a script, generate an Executable for it with its entrypoint being the
/// first block it contains.
pub fn generateExecutableFromScript(allocator: Allocator, script: Script.Ref) AstGenError!Executable.Ref {
    const executable = try Executable.create(allocator, script);
    errdefer executable.unref();

    var g = try AstGen.init(allocator);
    defer g.deinit();

    try g.generateScript(executable.value, script.value.ast_root.?);

    if (AST_EXECUTABLE_DUMP_DEBUG)
        std.debug.print("Executable dump: {}\n", .{executable.value});

    return executable;
}

fn init(allocator: Allocator) !AstGen {
    return AstGen{
        .register_id_stack = std.ArrayList(u32).init(allocator),
    };
}

fn deinit(self: *AstGen) void {
    self.register_id_stack.deinit();
}

fn pushRegisterID(self: *AstGen) !void {
    try self.register_id_stack.append(1);
}

fn allocateRegister(self: *AstGen) RegisterLocation {
    const latest_id = &self.register_id_stack.items[self.register_id_stack.items.len - 1];
    defer latest_id.* += 1;

    return RegisterLocation.fromIndex(latest_id.*);
}

fn popRegisterID(self: *AstGen) void {
    _ = self.register_id_stack.pop();
}

fn generateScript(self: *AstGen, executable: *Executable, script_node: ast.ScriptNode) AstGenError!void {
    const script_block_index = try executable.makeBlock();
    const script_block = executable.getBlock(script_block_index);

    try self.pushRegisterID();
    defer self.popRegisterID();

    const last_expression_location = try self.generateStatementList(executable, script_block, script_node.statements.value.statements);
    try script_block.addInstruction(executable.allocator, .Return, self.allocateRegister(), .{ .value_location = last_expression_location }, script_node.range);

    script_block.seal();
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
        // return "Maximum assignable slot limit exceeded for slots object"
        return error.AstGenFailure;
    }

    try self.generateSlotList(executable, block, object.slots, object.range);

    const object_location = self.allocateRegister();
    const slot_count: u16 = @intCast(object.slots.len);
    try block.addInstruction(executable.allocator, .CreateObject, object_location, .{ .slot_count = slot_count }, object.range);

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .VerifySlotSentinel, .Nil, {}, object.range);

    return object_location;
}

fn generateBlock(self: *AstGen, executable: *Executable, block: *Block, block_node: *ast.BlockNode) AstGenError!RegisterLocation {
    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, block_node.slots, block_node.statements.value.statements, block_node.range);
    const block_location = self.allocateRegister();
    const slot_count: u16 = @intCast(block_node.slots.len);
    try block.addInstruction(executable.allocator, .CreateBlock, block_location, .{
        .slot_count = slot_count,
        .block_index = block_index,
    }, block_node.range);

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .VerifySlotSentinel, .Nil, {}, block_node.range);

    return block_location;
}

fn generateMethod(self: *AstGen, executable: *Executable, block: *Block, method_name_location: RegisterLocation, method: *ast.ObjectNode) AstGenError!RegisterLocation {
    self.method_execution_depth += 1;
    defer self.method_execution_depth -= 1;

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, method.slots, method.statements.value.statements, method.range);

    if (self.method_execution_depth > 1) {
        try block.addInstruction(executable.allocator, .SetMethodInline, .Nil, {}, method.range);
    }

    const method_location = self.allocateRegister();
    const slot_count: u16 = @intCast(method.slots.len);
    try block.addInstruction(executable.allocator, .CreateMethod, method_location, .{
        .method_name_location = method_name_location,
        .slot_count = slot_count,
        .block_index = block_index,
    }, method.range);

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .VerifySlotSentinel, .Nil, {}, method.range);

    return method_location;
}

fn generateArgumentList(self: *AstGen, executable: *Executable, block: *Block, arguments: []ast.ExpressionNode, source_range: Range) !void {
    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .PushArgumentSentinel, .Nil, {}, source_range);

    for (arguments) |argument| {
        const argument_location = try self.generateExpression(executable, block, argument);
        try block.addInstruction(executable.allocator, .PushArg, .Nil, .{ .argument_location = argument_location }, source_range);
    }
}

fn generateMessage(self: *AstGen, executable: *Executable, block: *Block, message: *ast.MessageNode) AstGenError!RegisterLocation {
    const message_location = message_location: {
        if (message.receiver) |receiver| {
            const receiver_location = try self.generateExpression(executable, block, receiver);
            try self.generateArgumentList(executable, block, message.arguments, message.range);

            const message_location = self.allocateRegister();
            if (message.message_name[0] == '_') {
                if (primitives.getPrimitiveIndex(message.message_name[1..])) |index| {
                    try block.addInstruction(executable.allocator, .PrimSend, message_location, .{
                        .receiver_location = receiver_location,
                        .index = index,
                    }, message.range);
                } else {
                    // FIXME: Return rich errors
                    // Error: "Unknown primitive selector _{s}"
                    return error.AstGenFailure;
                }
            } else {
                try block.addInstruction(executable.allocator, .Send, message_location, .{
                    .receiver_location = receiver_location,
                    .selector = Selector.fromName(message.message_name),
                    .send_index = block.makeSendIndex(),
                }, message.range);
            }

            break :message_location message_location;
        }

        try self.generateArgumentList(executable, block, message.arguments, message.range);

        const message_location = self.allocateRegister();
        if (message.message_name[0] == '_') {
            if (primitives.getPrimitiveIndex(message.message_name[1..])) |index| {
                try block.addInstruction(executable.allocator, .SelfPrimSend, message_location, .{ .index = index }, message.range);
            } else {
                // FIXME: Return rich errors
                // Error: "Unknown primitive selector _{s}"
                return error.AstGenFailure;
            }
        } else {
            try block.addInstruction(executable.allocator, .SelfSend, message_location, .{
                .selector = Selector.fromName(message.message_name),
                .send_index = block.makeSendIndex(),
            }, message.range);
        }

        break :message_location message_location;
    };

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .VerifyArgumentSentinel, .Nil, {}, message.range);
    return message_location;
}

fn generateReturn(self: *AstGen, executable: *Executable, block: *Block, return_node: *ast.ReturnNode) AstGenError!RegisterLocation {
    const expr_location = try self.generateExpression(executable, block, return_node.expression);
    try block.addInstruction(executable.allocator, .NonlocalReturn, .Nil, .{ .value_location = expr_location }, return_node.range);
    return RegisterLocation.Nil;
}

// TODO: Evaluate ownership of the string for generateIdentifier and
//       generateString. Right now since the lifetime of the script is tied to
//       the lifetime of the executable it will be alive, in the future we might
//       want to delete the AST after codegen.

fn generateIdentifier(self: *AstGen, executable: *Executable, block: *Block, identifier: ast.IdentifierNode) AstGenError!RegisterLocation {
    const identifier_location = self.allocateRegister();
    if (identifier.value[0] == '_') {
        if (primitives.getPrimitiveIndex(identifier.value[1..])) |index| {
            try block.addInstruction(executable.allocator, .SelfPrimSend, identifier_location, .{ .index = index }, identifier.range);
        } else {
            // FIXME: Return rich errors
            // Error: "Unknown primitive selector _{s}"
            return error.AstGenFailure;
        }
    } else {
        try block.addInstruction(executable.allocator, .SelfSend, identifier_location, .{
            .selector = Selector.fromName(identifier.value),
            .send_index = block.makeSendIndex(),
        }, identifier.range);
    }

    return identifier_location;
}

fn generateString(self: *AstGen, executable: *Executable, block: *Block, string: ast.StringNode) AstGenError!RegisterLocation {
    const string_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, .CreateByteArray, string_location, string.value, string.range);

    return string_location;
}

fn generateNumber(self: *AstGen, executable: *Executable, block: *Block, number: ast.NumberNode) AstGenError!RegisterLocation {
    const number_location = self.allocateRegister();
    switch (number.value) {
        .Integer => |i| {
            const payload: i62 = @intCast(i);
            try block.addInstruction(executable.allocator, .CreateInteger, number_location, payload, number.range);
        },
        .FloatingPoint => |f| try block.addInstruction(executable.allocator, .CreateFloatingPoint, number_location, f, number.range),
    }

    return number_location;
}

fn generateSlotsAndCodeCommon(
    self: *AstGen,
    executable: *Executable,
    parent_block: *Block,
    slots: []ast.SlotNode,
    statements: []ast.ExpressionNode,
    source_range: Range,
) AstGenError!u32 {
    var assignable_slot_count: usize = 0;
    var argument_slot_count: usize = 0;
    for (slots) |slot| {
        if (slot.is_mutable) assignable_slot_count += 1;
        if (slot.is_argument) argument_slot_count += 1;
    }

    if (argument_slot_count > MaximumArguments) {
        // FIXME: Return rich errors
        // return "Maximum argument slot limit exceeded for block object"
        return error.AstGenFailure;
    }

    if (assignable_slot_count > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return "Maximum assignable slot limit exceeded for block object"
        return error.AstGenFailure;
    }

    try self.generateSlotList(executable, parent_block, slots, source_range);

    const child_block_index = try executable.makeBlock();
    const child_block = executable.getBlock(child_block_index);

    try self.pushRegisterID();
    defer self.popRegisterID();

    const last_expression_location = try self.generateStatementList(executable, child_block, statements);
    try child_block.addInstruction(executable.allocator, .Return, .Nil, .{ .value_location = last_expression_location }, source_range);

    child_block.seal();
    return child_block_index;
}

fn generateSlotList(self: *AstGen, executable: *Executable, block: *Block, slots: []ast.SlotNode, source_range: Range) AstGenError!void {
    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .PushSlotSentinel, .Nil, {}, source_range);

    for (slots) |slot| {
        const slot_name_location = self.allocateRegister();
        try block.addInstruction(executable.allocator, .CreateByteArray, slot_name_location, slot.name, slot.range);

        const slot_value_location = blk: {
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
                        break :blk try self.generateMethod(executable, block, slot_name_location, value.Object);
                }

                break :blk try self.generateExpression(executable, block, value);
            }

            break :blk RegisterLocation.Nil;
        };

        if (slot.is_argument) {
            std.debug.assert(slot.is_mutable);
            std.debug.assert(!slot.is_parent);

            try block.addInstruction(executable.allocator, .PushArgumentSlot, .Nil, .{
                .name_location = slot_name_location,
                .value_location = slot_value_location,
            }, slot.range);
        } else if (slot.is_mutable) {
            try block.addInstruction(executable.allocator, .PushAssignableSlot, .Nil, .{
                .name_location = slot_name_location,
                .value_location = slot_value_location,
                .is_parent = slot.is_parent,
            }, slot.range);
        } else {
            try block.addInstruction(executable.allocator, .PushConstantSlot, .Nil, .{
                .name_location = slot_name_location,
                .value_location = slot_value_location,
                .is_parent = slot.is_parent,
            }, slot.range);
        }
    }
}
