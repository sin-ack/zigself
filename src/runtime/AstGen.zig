// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("../language/ast.zig");
const debug = @import("../debug.zig");
const Script = @import("../language/script.zig");
const bytecode = @import("./bytecode.zig");

const Block = bytecode.AstcodeBlock;
const Executable = bytecode.AstcodeExecutable;
const Instruction = bytecode.AstcodeInstruction;
const RegisterLocation = bytecode.astcode.RegisterLocation;

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
        std.debug.print("Executable dump: {}\n", .{executable});

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
    var latest_id = &self.register_id_stack.items[self.register_id_stack.items.len - 1];
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
    try script_block.addInstruction(executable.allocator, Instruction.init(self.allocateRegister(), .{ .Return = .{ .value_location = last_expression_location } }));
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

    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = object.range }));

    const object_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, Instruction.init(object_location, .{
        .CreateObject = .{ .slot_count = @intCast(u32, object.slots.len) },
    }));

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .VerifySlotSentinel = {} }));

    return object_location;
}

fn generateBlock(self: *AstGen, executable: *Executable, block: *Block, block_node: *ast.BlockNode) AstGenError!RegisterLocation {
    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = block_node.range }));

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, block_node.slots, block_node.statements.value.statements);
    const block_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, Instruction.init(block_location, .{
        .CreateBlock = .{
            .slot_count = @intCast(u32, block_node.slots.len),
            .block_index = block_index,
        },
    }));

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .VerifySlotSentinel = {} }));

    return block_location;
}

fn generateMethod(self: *AstGen, executable: *Executable, block: *Block, method_name: []const u8, method: *ast.ObjectNode) AstGenError!RegisterLocation {
    self.method_execution_depth += 1;
    defer self.method_execution_depth -= 1;

    const block_index = try self.generateSlotsAndCodeCommon(executable, block, method.slots, method.statements.value.statements);

    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = method.range }));
    const method_name_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, Instruction.init(method_name_location, .{ .CreateByteArray = method_name }));

    if (self.method_execution_depth > 1) {
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SetMethodInline = {} }));
    }

    const method_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, Instruction.init(method_location, .{
        .CreateMethod = .{
            .method_name_location = method_name_location,
            .slot_count = @intCast(u32, method.slots.len),
            .block_index = block_index,
        },
    }));

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .VerifySlotSentinel = {} }));

    return method_location;
}

fn generateArgumentList(self: *AstGen, executable: *Executable, block: *Block, arguments: []ast.ExpressionNode) !void {
    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .PushArgumentSentinel = {} }));

    for (arguments) |argument| {
        const argument_location = try self.generateExpression(executable, block, argument);
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .PushArg = .{ .argument_location = argument_location } }));
    }
}

fn generateMessage(self: *AstGen, executable: *Executable, block: *Block, message: *ast.MessageNode) AstGenError!RegisterLocation {
    const message_location = message_location: {
        if (message.receiver) |receiver| {
            const receiver_location = try self.generateExpression(executable, block, receiver);
            try self.generateArgumentList(executable, block, message.arguments);

            try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = message.range }));

            const message_location = self.allocateRegister();
            if (message.message_name[0] == '_') {
                try block.addInstruction(executable.allocator, Instruction.init(message_location, .{
                    .PrimSend = .{
                        .receiver_location = receiver_location,
                        .message_name = message.message_name[1..],
                    },
                }));
            } else {
                try block.addInstruction(executable.allocator, Instruction.init(message_location, .{
                    .Send = .{
                        .receiver_location = receiver_location,
                        .message_name = message.message_name,
                    },
                }));
            }

            break :message_location message_location;
        }

        try self.generateArgumentList(executable, block, message.arguments);

        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = message.range }));

        const message_location = self.allocateRegister();
        if (message.message_name[0] == '_') {
            try block.addInstruction(executable.allocator, Instruction.init(message_location, .{ .SelfPrimSend = .{ .message_name = message.message_name[1..] } }));
        } else {
            try block.addInstruction(executable.allocator, Instruction.init(message_location, .{ .SelfSend = .{ .message_name = message.message_name } }));
        }

        break :message_location message_location;
    };

    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .VerifyArgumentSentinel = {} }));
    return message_location;
}

fn generateReturn(self: *AstGen, executable: *Executable, block: *Block, return_node: *ast.ReturnNode) AstGenError!RegisterLocation {
    const expr_location = try self.generateExpression(executable, block, return_node.expression);
    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = return_node.range }));
    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .NonlocalReturn = .{ .value_location = expr_location } }));
    return RegisterLocation.Nil;
}

// TODO: Evaluate ownership of the string for generateIdentifier and
//       generateString. Right now since the lifetime of the script is tied to
//       the lifetime of the executable it will be alive, in the future we might
//       want to delete the AST after codegen.

fn generateIdentifier(self: *AstGen, executable: *Executable, block: *Block, identifier: ast.IdentifierNode) AstGenError!RegisterLocation {
    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = identifier.range }));

    const identifier_location = self.allocateRegister();
    if (identifier.value[0] == '_') {
        try block.addInstruction(executable.allocator, Instruction.init(identifier_location, .{ .SelfPrimSend = .{ .message_name = identifier.value[1..] } }));
    } else {
        try block.addInstruction(executable.allocator, Instruction.init(identifier_location, .{ .SelfSend = .{ .message_name = identifier.value } }));
    }

    return identifier_location;
}

fn generateString(self: *AstGen, executable: *Executable, block: *Block, string: ast.StringNode) AstGenError!RegisterLocation {
    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = string.range }));

    const string_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, Instruction.init(string_location, .{ .CreateByteArray = string.value }));

    return string_location;
}

fn generateNumber(self: *AstGen, executable: *Executable, block: *Block, number: ast.NumberNode) AstGenError!RegisterLocation {
    try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = number.range }));

    const number_location = self.allocateRegister();
    switch (number.value) {
        .Integer => |i| try block.addInstruction(executable.allocator, Instruction.init(number_location, .{ .CreateInteger = @intCast(i62, i) })),
        .FloatingPoint => |f| try block.addInstruction(executable.allocator, Instruction.init(number_location, .{ .CreateFloatingPoint = f })),
    }

    return number_location;
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

    try self.pushRegisterID();
    defer self.popRegisterID();

    const last_expression_location = try self.generateStatementList(executable, child_block, statements);
    try child_block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .Return = .{ .value_location = last_expression_location } }));

    return child_block_index;
}

fn generateSlotList(self: *AstGen, executable: *Executable, block: *Block, slots: []ast.SlotNode) AstGenError!void {
    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .PushSlotSentinel = {} }));

    for (slots) |slot| {
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
                        break :blk try self.generateMethod(executable, block, slot.name, value.Object);
                }

                break :blk try self.generateExpression(executable, block, value);
            }

            break :blk RegisterLocation.Nil;
        };

        try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{ .SourceRange = slot.range }));
        const slot_name_location = self.allocateRegister();
        try block.addInstruction(executable.allocator, Instruction.init(slot_name_location, .{ .CreateByteArray = slot.name }));

        if (slot.is_inherited) {
            std.debug.assert(!slot.is_mutable);
            std.debug.assert(!slot.is_argument);
            std.debug.assert(!slot.is_parent);

            try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{
                .PushInheritedSlot = .{
                    .name_location = slot_name_location,
                    .value_location = slot_value_location,
                },
            }));
        } else if (slot.is_argument) {
            std.debug.assert(slot.is_mutable);
            std.debug.assert(!slot.is_parent);

            try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{
                .PushArgumentSlot = .{
                    .name_location = slot_name_location,
                    .value_location = slot_value_location,
                },
            }));
        } else if (slot.is_mutable) {
            try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{
                .PushAssignableSlot = .{
                    .name_location = slot_name_location,
                    .value_location = slot_value_location,
                    .is_parent = slot.is_parent,
                },
            }));
        } else {
            try block.addInstruction(executable.allocator, Instruction.init(.Nil, .{
                .PushConstantSlot = .{
                    .name_location = slot_name_location,
                    .value_location = slot_value_location,
                    .is_parent = slot.is_parent,
                },
            }));
        }
    }
}
