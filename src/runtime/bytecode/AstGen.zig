// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
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
const ObjectDescriptor = @import("ObjectDescriptor.zig");

const Block = astcode.Block;
const Executable = astcode.Executable;
const RegisterLocation = astcode.RegisterLocation;

const AST_EXECUTABLE_DUMP_DEBUG = debug.AST_EXECUTABLE_DUMP_DEBUG;

// FIXME: Move this to a constants file
pub const MaximumArguments = 128; // Reasonable limit
pub const MaximumAssignableSlots = 255;

method_execution_depth: usize = 0,
argument_sentinel_id: usize = 0,
allocator: Allocator,
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
        std.debug.print("Executable dump: {f}\n", .{executable.value});

    return executable;
}

fn init(allocator: Allocator) !AstGen {
    return AstGen{
        .allocator = allocator,
        .register_id_stack = .empty,
    };
}

fn deinit(self: *AstGen) void {
    self.register_id_stack.deinit(self.allocator);
}

fn pushRegisterID(self: *AstGen) !void {
    try self.register_id_stack.append(self.allocator, 1);
}

fn allocateRegister(self: *AstGen) RegisterLocation {
    const latest_id = &self.register_id_stack.items[self.register_id_stack.items.len - 1];
    defer latest_id.* += 1;

    return RegisterLocation.fromIndex(latest_id.*);
}

fn popRegisterID(self: *AstGen) void {
    _ = self.register_id_stack.pop();
}

fn makeArgumentSentinel(self: *AstGen) usize {
    const id = self.argument_sentinel_id;
    self.argument_sentinel_id += 1;
    return id;
}

fn generateScript(self: *AstGen, executable: *Executable, script_node: ast.ScriptNode) AstGenError!void {
    const script_block_index = try executable.makeBlock();
    const script_block = executable.getBlock(script_block_index);

    try self.pushRegisterID();
    defer self.popRegisterID();

    // TODO: Top-level scripts should be able to have slots.
    const object_descriptor = ObjectDescriptor.init(&.{});

    const last_expression_location = try self.generateStatementList(executable, script_block, &object_descriptor, script_node.statements.value.statements);
    try script_block.addInstruction(executable.allocator, .Return, self.allocateRegister(), .{ .value_location = last_expression_location }, script_node.range);

    script_block.seal();
}

fn generateStatementList(self: *AstGen, executable: *Executable, block: *Block, object_descriptor: *const ObjectDescriptor, statements: []ast.ExpressionNode) AstGenError!RegisterLocation {
    var last_expression_location = RegisterLocation.Nil;
    for (statements) |expression| {
        last_expression_location = try self.generateExpression(executable, block, object_descriptor, expression);
    }

    return last_expression_location;
}

fn generateExpression(self: *AstGen, executable: *Executable, block: *Block, object_descriptor: *const ObjectDescriptor, expression: ast.ExpressionNode) AstGenError!RegisterLocation {
    return switch (expression) {
        .Object => |object| self.generateObject(executable, block, object_descriptor, object),
        .Block => |block_node| self.generateBlock(executable, block, object_descriptor, block_node),
        .Message => |message| self.generateMessage(executable, block, object_descriptor, message),
        .Return => |return_node| self.generateReturn(executable, block, object_descriptor, return_node),
        .Identifier => |identifier| self.generateIdentifier(executable, block, object_descriptor, identifier),

        .String => |string| self.generateString(executable, block, string),
        .Number => |number| self.generateNumber(executable, block, number),
    };
}

fn generateObject(self: *AstGen, executable: *Executable, block: *Block, parent_object_descriptor: *const ObjectDescriptor, object: *ast.ObjectNode) AstGenError!RegisterLocation {
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

    const object_descriptor = try self.generateSlotList(executable, block, parent_object_descriptor, object.slots, object.range);
    errdefer object_descriptor.deinit(executable.allocator);

    const object_descriptor_index = try executable.addObjectDescriptor(object_descriptor);
    const object_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, .CreateObject, object_location, .{ .descriptor_index = object_descriptor_index }, object.range);

    return object_location;
}

fn generateBlock(self: *AstGen, executable: *Executable, block: *Block, parent_object_descriptor: *const ObjectDescriptor, block_node: *ast.BlockNode) AstGenError!RegisterLocation {
    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    const result = try self.generateSlotsAndCodeCommon(executable, block, parent_object_descriptor, block_node.slots, block_node.statements.value.statements, block_node.range);
    errdefer result.object_descriptor.deinit(executable.allocator);

    const object_descriptor_index = try executable.addObjectDescriptor(result.object_descriptor);
    const block_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, .CreateBlock, block_location, .{
        .descriptor_index = object_descriptor_index,
        .block_index = result.block_index,
    }, block_node.range);

    return block_location;
}

fn generateMethod(self: *AstGen, executable: *Executable, parent_block: *Block, parent_object_descriptor: *const ObjectDescriptor, method_name: []const u8, method: *ast.ObjectNode) AstGenError!RegisterLocation {
    self.method_execution_depth += 1;
    defer self.method_execution_depth -= 1;

    const result = try self.generateSlotsAndCodeCommon(executable, parent_block, parent_object_descriptor, method.slots, method.statements.value.statements, method.range);
    errdefer result.object_descriptor.deinit(executable.allocator);

    const method_name_location = self.allocateRegister();
    try parent_block.addInstruction(executable.allocator, .CreateByteArray, method_name_location, method_name, method.range);

    const object_descriptor_index = try executable.addObjectDescriptor(result.object_descriptor);
    const method_location = self.allocateRegister();
    try parent_block.addInstruction(executable.allocator, .CreateMethod, method_location, .{
        .method_name_location = method_name_location,
        .descriptor_index = object_descriptor_index,
        .block_index = result.block_index,
        .is_inline = self.method_execution_depth > 1,
    }, method.range);

    return method_location;
}

fn generateArgumentList(self: *AstGen, executable: *Executable, block: *Block, object_descriptor: *const ObjectDescriptor, arguments: []ast.ExpressionNode, source_range: Range) !usize {
    const argument_sentinel = self.makeArgumentSentinel();
    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .PushArgumentSentinel, .Nil, argument_sentinel, source_range);

    for (arguments) |argument| {
        const argument_location = try self.generateExpression(executable, block, object_descriptor, argument);
        try block.addInstruction(executable.allocator, .PushArg, .Nil, .{ .argument_location = argument_location }, source_range);
    }

    return argument_sentinel;
}

/// Try to get the assignment target from this selector, if it looks like
/// an assignment. The selector must be a keyword selector with exactly
/// one keyword.
///
/// Examples: `foo:` -> `foo`, `foo:Bar:` -> null, `foo` -> null
fn messageAsAssignmentTarget(selector: []const u8) ?[]const u8 {
    const colon_index = std.mem.indexOfScalar(u8, selector, ':') orelse return null;
    if (colon_index != selector.len - 1) return null;
    return selector[0..colon_index];
}

fn generateMessage(self: *AstGen, executable: *Executable, block: *Block, object_descriptor: *const ObjectDescriptor, message: *ast.MessageNode) AstGenError!RegisterLocation {
    var argument_sentinel: ?usize = null;
    const message_location = message_location: {
        if (message.receiver) |receiver| {
            const receiver_location = try self.generateExpression(executable, block, object_descriptor, receiver);
            argument_sentinel = try self.generateArgumentList(executable, block, object_descriptor, message.arguments, message.range);

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

        // Try to see if this message would assign to a local (as we don't have a receiver,
        // we're pointing at the current activation record).
        if (messageAsAssignmentTarget(message.message_name)) |assignment_target| {
            std.debug.assert(message.arguments.len == 1);

            for (object_descriptor.slots) |slot| {
                if (std.mem.eql(u8, slot.name, assignment_target)) {
                    if (slot.assignable_index) |local_index| {
                        // Okay, this is actually a local put. We need to generate
                        // our argument first though.
                        const value_location = try self.generateExpression(executable, block, object_descriptor, message.arguments[0]);
                        const message_location = self.allocateRegister();
                        try block.addInstruction(executable.allocator, .PutLocal, message_location, .{
                            // FIXME: Will we ever get more than 256 locals? We should have a check here.
                            .local_index = .init(@intCast(local_index)),
                            .value_location = value_location,
                        }, message.range);
                        break :message_location message_location;
                    }

                    // Not assignable, drop through to normal send.
                    break;
                }
            }
        }

        argument_sentinel = try self.generateArgumentList(executable, block, object_descriptor, message.arguments, message.range);

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

    if (std.debug.runtime_safety) {
        if (argument_sentinel) |sentinel|
            try block.addInstruction(executable.allocator, .VerifyArgumentSentinel, .Nil, sentinel, message.range);
    }
    return message_location;
}

fn generateReturn(self: *AstGen, executable: *Executable, block: *Block, object_descriptor: *const ObjectDescriptor, return_node: *ast.ReturnNode) AstGenError!RegisterLocation {
    const expr_location = try self.generateExpression(executable, block, object_descriptor, return_node.expression);
    try block.addInstruction(executable.allocator, .NonlocalReturn, .Nil, .{ .value_location = expr_location }, return_node.range);
    return RegisterLocation.Nil;
}

// TODO: Evaluate ownership of the string for generateIdentifier and
//       generateString. Right now since the lifetime of the script is tied to
//       the lifetime of the executable it will be alive, in the future we might
//       want to delete the AST after codegen.

fn generateIdentifier(self: *AstGen, executable: *Executable, block: *Block, object_descriptor: *const ObjectDescriptor, identifier: ast.IdentifierNode) AstGenError!RegisterLocation {
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
        // Try to see if our message matches a local.
        for (object_descriptor.slots) |slot| {
            if (std.mem.eql(u8, slot.name, identifier.value)) {
                if (slot.assignable_index) |local_index| {
                    // FIXME: Will we ever get more than 256 locals? We should have a check here.
                    try block.addInstruction(executable.allocator, .GetLocal, identifier_location, .{ .local_index = .init(@intCast(local_index)) }, identifier.range);
                    return identifier_location;
                }

                // TODO: Constants can be optimized even further by just pointing
                //       to the constant table of a given executable. For now we
                //       drop through to a normal self-send.
                break;
            }
        }

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

const SlotsAndCodeResult = struct {
    block_index: u32,
    object_descriptor: ObjectDescriptor,
};

fn generateSlotsAndCodeCommon(
    self: *AstGen,
    executable: *Executable,
    parent_block: *Block,
    parent_object_descriptor: *const ObjectDescriptor,
    slots: []ast.SlotNode,
    statements: []ast.ExpressionNode,
    source_range: Range,
) AstGenError!SlotsAndCodeResult {
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

    const object_descriptor = try self.generateSlotList(executable, parent_block, parent_object_descriptor, slots, source_range);
    errdefer object_descriptor.deinit(executable.allocator);

    const child_block_index = try executable.makeBlock();
    const child_block = executable.getBlock(child_block_index);

    try self.pushRegisterID();
    defer self.popRegisterID();

    const last_expression_location = try self.generateStatementList(executable, child_block, &object_descriptor, statements);
    try child_block.addInstruction(executable.allocator, .Return, .Nil, .{ .value_location = last_expression_location }, source_range);

    child_block.seal();
    return .{
        .block_index = child_block_index,
        .object_descriptor = object_descriptor,
    };
}

fn generateSlotList(self: *AstGen, executable: *Executable, block: *Block, object_descriptor: *const ObjectDescriptor, slots: []ast.SlotNode, source_range: Range) AstGenError!ObjectDescriptor {
    var slot_descriptors: std.ArrayList(ObjectDescriptor.SlotDescriptor) = .empty;
    defer {
        for (slot_descriptors.items) |*slot_descriptor| {
            slot_descriptor.deinit(executable.allocator);
        }
        slot_descriptors.deinit(executable.allocator);
    }

    var assignable_index: usize = 0;
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
                        break :blk try self.generateMethod(executable, block, object_descriptor, slot.name, value.Object);
                }

                break :blk try self.generateExpression(executable, block, object_descriptor, value);
            }

            break :blk RegisterLocation.Nil;
        };

        if (slot.is_argument) {
            std.debug.assert(slot.is_mutable);
            std.debug.assert(!slot.is_parent);

            const descriptor: ObjectDescriptor.SlotDescriptor = try .initArgument(executable.allocator, slot.name, assignable_index);
            errdefer descriptor.deinit(executable.allocator);
            try slot_descriptors.append(executable.allocator, descriptor);

            assignable_index += 1;
        } else if (slot.is_parent) {
            const descriptor: ObjectDescriptor.SlotDescriptor = try .initParent(executable.allocator, slot.name, if (slot.is_mutable) assignable_index else null);
            errdefer descriptor.deinit(executable.allocator);
            try slot_descriptors.append(executable.allocator, descriptor);

            if (slot.is_mutable) assignable_index += 1;

            try block.addInstruction(executable.allocator, .PushArg, .Nil, .{
                .argument_location = slot_value_location,
            }, source_range);
        } else {
            const descriptor: ObjectDescriptor.SlotDescriptor = try .initRegular(executable.allocator, slot.name, if (slot.is_mutable) assignable_index else null);
            errdefer descriptor.deinit(executable.allocator);
            try slot_descriptors.append(executable.allocator, descriptor);

            if (slot.is_mutable) assignable_index += 1;

            try block.addInstruction(executable.allocator, .PushArg, .Nil, .{
                .argument_location = slot_value_location,
            }, source_range);
        }
    }

    const descriptors_slice = try slot_descriptors.toOwnedSlice(executable.allocator);
    return .init(descriptors_slice);
}
