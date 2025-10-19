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

const ObjectDescriptorLink = struct {
    descriptor: *const ObjectDescriptor,
    previous: ?*const ObjectDescriptorLink,
    /// The offset at which this descriptor's locals start, relative to
    /// the base of the entire method+inline method+block chain.
    local_base: usize,
    /// The total number of locals that should be allocated for the entire
    /// method+inline method+block chain, used by
    locals_total: *usize,

    /// Initialize a new descriptor link with no previous link.
    pub fn init(descriptor: *const ObjectDescriptor, locals_total: *usize) ObjectDescriptorLink {
        locals_total.* += descriptor.assignable_slots;
        return .{
            .descriptor = descriptor,
            .previous = null,
            .local_base = 0,
            .locals_total = locals_total,
        };
    }

    /// Chain this descriptor link to a previous one.
    pub fn chain(self: *const ObjectDescriptorLink, descriptor: *const ObjectDescriptor) ObjectDescriptorLink {
        const local_base = self.locals_total.*;
        self.locals_total.* += descriptor.assignable_slots;
        return .{
            .descriptor = descriptor,
            .previous = self,
            .local_base = local_base,
            .locals_total = self.locals_total,
        };
    }

    /// Return the slot index for the given name, searching through
    /// the linked descriptors. If not found, returns null.
    pub fn localIndexByName(self: *const ObjectDescriptorLink, name: []const u8) ?usize {
        var link_it: ?*const ObjectDescriptorLink = self;
        var i: usize = 0;
        while (link_it) |link| : (link_it = link.previous) {
            for (link.descriptor.slots) |slot| {
                if (std.mem.eql(u8, slot.name, name)) {
                    if (slot.assignable_index) |index| {
                        return link.local_base + index;
                    } else {
                        return null;
                    }
                }
            }

            i += 1;
        }

        return null;
    }
};

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
    var locals_total: usize = 0;
    const object_descriptor = ObjectDescriptor.init(&.{});
    const object_descriptor_link = ObjectDescriptorLink.init(&object_descriptor, &locals_total);

    const last_expression_location = try self.generateStatementList(executable, script_block, &object_descriptor_link, script_node.statements.value.statements);
    try script_block.addInstruction(executable.allocator, .Return, self.allocateRegister(), .{ .value_location = last_expression_location }, script_node.range);

    script_block.seal();
}

fn generateStatementList(self: *AstGen, executable: *Executable, block: *Block, object_descriptor_link: *const ObjectDescriptorLink, statements: []ast.ExpressionNode) AstGenError!RegisterLocation {
    var last_expression_location = RegisterLocation.Nil;
    for (statements) |expression| {
        last_expression_location = try self.generateExpression(executable, block, object_descriptor_link, expression);
    }

    return last_expression_location;
}

fn generateExpression(self: *AstGen, executable: *Executable, block: *Block, object_descriptor_link: *const ObjectDescriptorLink, expression: ast.ExpressionNode) AstGenError!RegisterLocation {
    return switch (expression) {
        .Object => |object| self.generateObject(executable, block, object_descriptor_link, object),
        .Block => |block_node| self.generateBlock(executable, block, object_descriptor_link, block_node),
        .Message => |message| self.generateMessage(executable, block, object_descriptor_link, message),
        .Return => |return_node| self.generateReturn(executable, block, object_descriptor_link, return_node),
        .Identifier => |identifier| self.generateIdentifier(executable, block, object_descriptor_link, identifier),

        .String => |string| self.generateString(executable, block, string),
        .Number => |number| self.generateNumber(executable, block, number),
    };
}

fn generateObject(self: *AstGen, executable: *Executable, block: *Block, parent_object_descriptor_link: *const ObjectDescriptorLink, object: *ast.ObjectNode) AstGenError!RegisterLocation {
    // Verify that we are generating bytecode for a slots object and not a
    // method; methods are generated through generateMethod.
    if (object.statements.value.statements.len > 0) {
        @panic("!!! Attempted to generate a slots object from a non-slots object! Methods must codegen via generateMethod.");
    }

    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    const object_descriptor = try self.buildObjectDescriptor(object.slots);
    errdefer object_descriptor.deinit(executable.allocator);

    if (object_descriptor.assignable_slots > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return "Maximum assignable slot limit exceeded for slots object"
        return error.AstGenFailure;
    }

    // XXX: In order to avoid using "locals" from the current object (which
    //      isn't valid for slots objects), we reuse the
    //      parent_object_descriptor_link here.
    try self.generateSlotValues(executable, block, parent_object_descriptor_link, object.slots, object.range);

    const object_descriptor_index = try executable.addObjectDescriptor(object_descriptor);
    const object_location = self.allocateRegister();
    try block.addInstruction(executable.allocator, .CreateObject, object_location, .{ .descriptor_index = object_descriptor_index }, object.range);

    return object_location;
}

fn generateBlock(self: *AstGen, executable: *Executable, parent_block: *Block, parent_object_descriptor_link: *const ObjectDescriptorLink, block_node: *ast.BlockNode) AstGenError!RegisterLocation {
    const object_descriptor = try self.buildObjectDescriptor(block_node.slots);
    errdefer object_descriptor.deinit(executable.allocator);

    if (object_descriptor.argument_slots > MaximumArguments) {
        // FIXME: Return rich errors
        // return "Maximum argument slot limit exceeded for block object"
        return error.AstGenFailure;
    }

    if (object_descriptor.assignable_slots > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return "Maximum assignable slot limit exceeded for block object"
        return error.AstGenFailure;
    }

    const object_descriptor_link = parent_object_descriptor_link.chain(&object_descriptor);
    try self.generateSlotValues(executable, parent_block, &object_descriptor_link, block_node.slots, block_node.range);

    const child_block_index = try executable.makeBlock();
    const child_block = executable.getBlock(child_block_index);

    {
        try self.pushRegisterID();
        defer self.popRegisterID();

        const last_expression_location = try self.generateStatementList(executable, child_block, &object_descriptor_link, block_node.statements.value.statements);
        try child_block.addInstruction(executable.allocator, .Return, .Nil, .{ .value_location = last_expression_location }, block_node.range);
        child_block.seal();
    }

    const saved_method_execution_depth = self.method_execution_depth;
    self.method_execution_depth = 0;
    defer self.method_execution_depth = saved_method_execution_depth;

    const object_descriptor_index = try executable.addObjectDescriptor(object_descriptor);
    const block_location = self.allocateRegister();
    try parent_block.addInstruction(executable.allocator, .CreateBlock, block_location, .{
        .descriptor_index = object_descriptor_index,
        .block_index = child_block_index,
        .method_local_offset = @intCast(object_descriptor_link.local_base),
    }, block_node.range);

    return block_location;
}

fn generateMethod(self: *AstGen, executable: *Executable, parent_block: *Block, parent_object_descriptor_link: *const ObjectDescriptorLink, method_name: []const u8, method: *ast.ObjectNode) AstGenError!RegisterLocation {
    self.method_execution_depth += 1;
    defer self.method_execution_depth -= 1;

    const object_descriptor = try self.buildObjectDescriptor(method.slots);
    errdefer object_descriptor.deinit(executable.allocator);

    if (object_descriptor.argument_slots > MaximumArguments) {
        // FIXME: Return rich errors
        // return "Maximum argument slot limit exceeded for method object"
        return error.AstGenFailure;
    }

    if (object_descriptor.assignable_slots > MaximumAssignableSlots) {
        // FIXME: Return rich errors
        // return "Maximum assignable slot limit exceeded for method object"
        return error.AstGenFailure;
    }

    // Unused if this is an inline method.
    var locals_total: usize = 0;
    const object_descriptor_link = if (self.method_execution_depth > 1)
        parent_object_descriptor_link.chain(&object_descriptor)
    else
        ObjectDescriptorLink.init(&object_descriptor, &locals_total);

    try self.generateSlotValues(executable, parent_block, &object_descriptor_link, method.slots, method.range);

    const child_block_index = try executable.makeBlock();
    const child_block = executable.getBlock(child_block_index);

    {
        try self.pushRegisterID();
        defer self.popRegisterID();

        const last_expression_location = try self.generateStatementList(executable, child_block, &object_descriptor_link, method.statements.value.statements);
        try child_block.addInstruction(executable.allocator, .Return, .Nil, .{ .value_location = last_expression_location }, method.range);

        child_block.seal();
    }

    const method_name_location = self.allocateRegister();
    try parent_block.addInstruction(executable.allocator, .CreateByteArray, method_name_location, method_name, method.range);

    const object_descriptor_index = try executable.addObjectDescriptor(object_descriptor);
    const method_location = self.allocateRegister();
    try parent_block.addInstruction(executable.allocator, .CreateMethod, method_location, .{
        .method_name_location = method_name_location,
        .descriptor_index = object_descriptor_index,
        .block_index = child_block_index,
        .is_inline = self.method_execution_depth > 1,
        // This value is interpreted differently based on whether this is
        // an inline method or not:
        // - Inline method: The local offset within the parent method.
        // - Non-inline method: The total local depth for the entire method+block chain.
        .local_depth = if (self.method_execution_depth > 1)
            @intCast(object_descriptor_link.local_base)
        else
            @intCast(locals_total),
    }, method.range);

    return method_location;
}

fn generateArgumentList(self: *AstGen, executable: *Executable, block: *Block, object_descriptor_link: *const ObjectDescriptorLink, arguments: []ast.ExpressionNode, source_range: Range) !usize {
    const argument_sentinel = self.makeArgumentSentinel();
    if (std.debug.runtime_safety)
        try block.addInstruction(executable.allocator, .PushArgumentSentinel, .Nil, argument_sentinel, source_range);

    for (arguments) |argument| {
        const argument_location = try self.generateExpression(executable, block, object_descriptor_link, argument);
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

fn generateMessage(self: *AstGen, executable: *Executable, block: *Block, object_descriptor_link: *const ObjectDescriptorLink, message: *ast.MessageNode) AstGenError!RegisterLocation {
    var argument_sentinel: ?usize = null;
    const message_location = message_location: {
        if (message.receiver) |receiver| {
            const receiver_location = try self.generateExpression(executable, block, object_descriptor_link, receiver);
            argument_sentinel = try self.generateArgumentList(executable, block, object_descriptor_link, message.arguments, message.range);

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

            if (object_descriptor_link.localIndexByName(assignment_target)) |local_index| {
                // Okay, this is actually a local put. We need to generate
                // our argument first though.
                const value_location = try self.generateExpression(executable, block, object_descriptor_link, message.arguments[0]);
                const message_location = self.allocateRegister();
                try block.addInstruction(executable.allocator, .PutLocal, message_location, .{
                    // FIXME: Will we ever get more than 256 locals? We should have a check here.
                    .local_index = .initIndex(@intCast(local_index)),
                    .value_location = value_location,
                }, message.range);
                break :message_location message_location;
            }
        }

        argument_sentinel = try self.generateArgumentList(executable, block, object_descriptor_link, message.arguments, message.range);

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

fn generateReturn(self: *AstGen, executable: *Executable, block: *Block, object_descriptor_link: *const ObjectDescriptorLink, return_node: *ast.ReturnNode) AstGenError!RegisterLocation {
    const expr_location = try self.generateExpression(executable, block, object_descriptor_link, return_node.expression);
    try block.addInstruction(executable.allocator, .NonlocalReturn, .Nil, .{ .value_location = expr_location }, return_node.range);
    return RegisterLocation.Nil;
}

// TODO: Evaluate ownership of the string for generateIdentifier and
//       generateString. Right now since the lifetime of the script is tied to
//       the lifetime of the executable it will be alive, in the future we might
//       want to delete the AST after codegen.

fn generateIdentifier(self: *AstGen, executable: *Executable, block: *Block, object_descriptor_link: *const ObjectDescriptorLink, identifier: ast.IdentifierNode) AstGenError!RegisterLocation {
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
        if (object_descriptor_link.localIndexByName(identifier.value)) |local_index| {
            // FIXME: Will we ever get more than 256 locals? We should have a check here.
            try block.addInstruction(executable.allocator, .GetLocal, identifier_location, .{ .local_index = .initIndex(@intCast(local_index)) }, identifier.range);
            return identifier_location;
        }

        // TODO: Constants can be optimized even further by just pointing
        //       to the constant table of a given executable. For now we
        //       drop through to a normal self-send.

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

/// Generate an ObjectDescriptor from a list of slots.
/// Note that the actual values of the slots are NOT rendered in this function;
/// you must call generateSlotValues which will push the slot values onto
/// the argument stack.
fn buildObjectDescriptor(self: *AstGen, slots: []ast.SlotNode) AstGenError!ObjectDescriptor {
    var slot_descriptors: std.ArrayList(ObjectDescriptor.SlotDescriptor) = .empty;
    defer {
        for (slot_descriptors.items) |*slot_descriptor| {
            slot_descriptor.deinit(self.allocator);
        }
        slot_descriptors.deinit(self.allocator);
    }

    var assignable_index: usize = 0;
    for (slots) |slot| {
        if (slot.is_argument) {
            std.debug.assert(slot.is_mutable);
            std.debug.assert(!slot.is_parent);

            const descriptor: ObjectDescriptor.SlotDescriptor = try .initArgument(self.allocator, slot.name, assignable_index);
            errdefer descriptor.deinit(self.allocator);
            try slot_descriptors.append(self.allocator, descriptor);
        } else if (slot.is_parent) {
            const descriptor: ObjectDescriptor.SlotDescriptor = try .initParent(self.allocator, slot.name, if (slot.is_mutable) assignable_index else null);
            errdefer descriptor.deinit(self.allocator);
            try slot_descriptors.append(self.allocator, descriptor);
        } else {
            const descriptor: ObjectDescriptor.SlotDescriptor = try .initRegular(self.allocator, slot.name, if (slot.is_mutable) assignable_index else null);
            errdefer descriptor.deinit(self.allocator);
            try slot_descriptors.append(self.allocator, descriptor);
        }

        if (slot.is_mutable) assignable_index += 1;
    }

    const descriptors_slice = try slot_descriptors.toOwnedSlice(self.allocator);
    return .init(descriptors_slice);
}

/// Generate the values for the slots of an object and push them onto the
/// argument stack.
///
/// You must pass the object descriptor link that's connected to the slot list
/// that was just generated with generateSlotList.
fn generateSlotValues(self: *AstGen, executable: *Executable, block: *Block, object_descriptor_link: *const ObjectDescriptorLink, slots: []ast.SlotNode, source_range: Range) AstGenError!void {
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
                        break :blk try self.generateMethod(executable, block, object_descriptor_link, slot.name, value.Object);
                }

                break :blk try self.generateExpression(executable, block, object_descriptor_link, value);
            }

            break :blk RegisterLocation.Nil;
        };

        // Argument slots don't get values pushed for them.
        if (slot.is_argument) {
            std.debug.assert(slot_value_location == RegisterLocation.Nil);
            continue;
        }

        try block.addInstruction(executable.allocator, .PushArg, .Nil, .{
            .argument_location = slot_value_location,
        }, source_range);
    }
}
