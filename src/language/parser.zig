// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("./ast.zig");
const Diagnostics = @import("./diagnostics.zig");
const Lexer = @import("./lexer.zig");
const tokens = @import("./tokens.zig");

lexer: *Lexer,
diagnostics: Diagnostics,

pub const ParseError = errorSetOf(Lexer.nextToken);
const Self = @This();

pub fn createFromFile(allocator: Allocator, file_path: []const u8) !*Self {
    var lexer = try Lexer.createFromFile(allocator, file_path);
    errdefer lexer.destroy(allocator);
    var diagnostics = try Diagnostics.init(allocator);
    errdefer diagnostics.deinit();

    var self = try allocator.create(Self);
    self.init(lexer, diagnostics);
    return self;
}

pub fn createFromString(allocator: Allocator, contents: []const u8) !*Self {
    var lexer = try Lexer.createFromString(allocator, contents);
    errdefer lexer.destroy(allocator);
    var diagnostics = try Diagnostics.init(allocator);
    errdefer diagnostics.deinit();

    var self = try allocator.create(Self);
    self.init(lexer, diagnostics);
    return self;
}

fn init(self: *Self, lexer: *Lexer, diagnostics: Diagnostics) void {
    self.lexer = lexer;
    self.diagnostics = diagnostics;
}

fn deinit(self: *Self, allocator: Allocator) void {
    self.lexer.destroy(allocator);
    self.diagnostics.deinit();
}

pub fn destroy(self: *Self, allocator: Allocator) void {
    self.deinit(allocator);
    allocator.destroy(self);
}

pub fn parseScript(self: *Self, allocator: Allocator) ParseError!AST.ScriptNode {
    // Script = StatementList

    _ = try self.lexer.nextToken(allocator);
    const statements = try self.parseStatementList(allocator, false);
    errdefer statements.unrefWithAllocator(allocator);
    if (!try self.expect(allocator, .EOF, .DontConsume)) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.lexer.token_start,
            "Expected end-of-file after last statement, got {s}",
            .{self.lexer.current_token.toString()},
        );
    }

    return AST.ScriptNode{ .statements = statements };
}

fn parseStatementList(self: *Self, allocator: Allocator, allow_nonlocal_return: bool) ParseError!AST.StatementList.Ref {
    // StatementList = Expression ("." Expression)* "."? | empty

    var statements = std.ArrayList(AST.ExpressionNode).init(allocator);
    defer {
        for (statements.items) |*expression| {
            expression.deinit(allocator);
        }
        statements.deinit();
    }

    var did_parse_nonlocal_return = false;
    var should_wrap_expression_with_return_node = false;

    if (self.lexer.current_token != .EOF) first_expr_parsing: {
        {
            const start_of_expression = self.lexer.token_start;
            if (self.lexer.current_token == .Cap) parse_nonlocal_return: {
                if (!allow_nonlocal_return) {
                    try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Non-local returns are not allowed here");
                    break :parse_nonlocal_return;
                }

                should_wrap_expression_with_return_node = true;
                _ = try self.lexer.nextToken(allocator);
            }

            var first_expression = (try self.parseExpression(allocator, .Any)) orelse break :first_expr_parsing;
            errdefer first_expression.deinit(allocator);

            if (should_wrap_expression_with_return_node) {
                var return_node = try allocator.create(AST.ReturnNode);
                return_node.* = .{
                    .expression = first_expression,
                    .range = .{ .start = start_of_expression, .end = first_expression.range().end },
                };
                first_expression = AST.ExpressionNode{ .Return = return_node };
                did_parse_nonlocal_return = true;
                should_wrap_expression_with_return_node = false;
            }

            try statements.append(first_expression);
        }

        while (self.lexer.current_token == .Period) {
            _ = try self.lexer.nextToken(allocator);

            const start_of_expression = self.lexer.token_start;
            if (self.lexer.current_token == .Cap) parse_nonlocal_return: {
                if (!allow_nonlocal_return) {
                    try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Non-local returns are not allowed here");
                    break :parse_nonlocal_return;
                } else if (did_parse_nonlocal_return) {
                    try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Unexpected statement after non-local return");
                    break :parse_nonlocal_return;
                }

                should_wrap_expression_with_return_node = true;
                _ = try self.lexer.nextToken(allocator);
            }

            if (!self.canParseExpression()) {
                if (should_wrap_expression_with_return_node)
                    try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Expected expression after non-local return");
                break;
            }

            if (did_parse_nonlocal_return) {
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Unexpected statement after non-local return");
            }

            // TODO: Recover until next period or EOF
            var expression = (try self.parseExpression(allocator, .Any)) orelse continue;
            errdefer expression.deinit(allocator);

            if (should_wrap_expression_with_return_node) {
                var return_node = try allocator.create(AST.ReturnNode);
                return_node.* = .{
                    .expression = expression,
                    .range = .{ .start = start_of_expression, .end = expression.range().end },
                };
                expression = AST.ExpressionNode{ .Return = return_node };
                did_parse_nonlocal_return = true;
                should_wrap_expression_with_return_node = false;
            }

            try statements.append(expression);
        }
    }

    return AST.StatementList.create(allocator, statements.toOwnedSlice());
}

fn canParseExpression(self: *Self) bool {
    // binary to self
    if (self.lexer.current_token.isOperator())
        return true;

    return switch (self.lexer.current_token) {
        // zig fmt: off
        .FirstKeyword, // keyword to self
        .Identifier,   // unary to self
        .ParenOpen,    // object/subexpr
        .BracketOpen,  // block
        .String,
        .Integer,
        .FloatingPoint,
        => true,
        // zig fmt: on
        else => false,
    };
}

const MessagePrecedence = enum { Binary, Keyword, Any };
fn parseExpression(self: *Self, allocator: Allocator, precedence: MessagePrecedence) ParseError!?AST.ExpressionNode {
    // Expression = KeywordSend
    //            | BinarySend
    //            | Primary UnarySend* BinarySend? KeywordSend?

    if (self.lexer.current_token == .FirstKeyword)
        return try self.parseKeywordMessage(allocator, null);

    if (self.lexer.current_token.isOperator())
        return try self.parseBinaryMessage(allocator, null);

    var primary = (try self.parsePrimary(allocator)) orelse return null;
    errdefer primary.deinit(allocator);

    return try self.parseExpressionFromPrimary(allocator, primary, precedence, false);
}

// If require_message is true then the primary must receive at least one message.
fn parseExpressionFromPrimary(
    self: *Self,
    allocator: Allocator,
    primary: AST.ExpressionNode,
    precedence: MessagePrecedence,
    require_message: bool,
) ParseError!?AST.ExpressionNode {
    var expr = primary;
    var did_send_message = false;

    const emitDiagnosticIfNeeded = struct {
        fn f(s: *Self, require: bool, did_send: bool) ParseError!void {
            if (!did_send and require) {
                try s.diagnostics.reportDiagnosticFormatted(
                    .Error,
                    s.lexer.token_start,
                    "Expected unary, binary or keyword message, got {s}",
                    .{s.lexer.current_token.toString()},
                );
            }
        }
    }.f;

    // Collect unary messages
    while (self.lexer.current_token == .Identifier) {
        did_send_message = true;

        var message_node = try allocator.create(AST.MessageNode);
        errdefer allocator.destroy(message_node);

        // FIXME: The source code is always kept in memory, even after the file
        //        has been parsed. The script object always owns the AST, and
        //        shares the same lifetime. Avoid these allocations by simply
        //        pointing to a slice of source code.
        const identifier_slice = std.mem.sliceTo(&self.lexer.current_token.Identifier, 0);
        var identifier_copy = try allocator.dupe(u8, identifier_slice);
        // NOTE: message_node takes ownership of this identifier so we don't errdefer here.
        //       In case of error, expr.deinit cleans up the identifier.

        message_node.* = .{
            .receiver = expr,
            .message_name = identifier_copy,
            .arguments = &.{},
            .range = .{ .start = expr.range().start, .end = self.lexer.token_end },
        };
        expr = AST.ExpressionNode{ .Message = message_node };

        _ = try self.lexer.nextToken(allocator);
    }

    // If possible, parse a binary message.
    if (self.lexer.current_token.isOperator()) {
        did_send_message = true;
        expr = (try self.parseBinaryMessage(allocator, expr)) orelse return null;
    }

    if (precedence == .Binary) {
        try emitDiagnosticIfNeeded(self, require_message, did_send_message);
        return expr;
    }

    // Otherwise, try parsing a keyword message.
    if (self.lexer.current_token == .FirstKeyword) {
        did_send_message = true;
        expr = (try self.parseKeywordMessage(allocator, expr)) orelse return null;
    }

    if (precedence == .Keyword) {
        try emitDiagnosticIfNeeded(self, require_message, did_send_message);
        return expr;
    }

    // If the current token is a semicolon then we want to send more messages to it,
    // so try parsing an expression after it with the current expression as the
    // "primary".
    if (self.lexer.current_token == .Semicolon) {
        _ = try self.lexer.nextToken(allocator);
        expr = (try self.parseExpressionFromPrimary(allocator, expr, .Any, true)) orelse return null;
    }

    return expr;
}

fn parseBinaryMessage(self: *Self, allocator: Allocator, receiver: ?AST.ExpressionNode) ParseError!?AST.ExpressionNode {
    // BinarySend = binaryOp+ Expression
    std.debug.assert(self.lexer.current_token.isOperator());

    const start_of_message = self.lexer.token_start;
    var binary_message_name = std.BoundedArray(u8, tokens.MaximumIdentifierLength).init(0) catch unreachable;

    // FIXME: Disallow space between operator tokens.
    while (self.lexer.current_token.isOperator()) {
        binary_message_name.appendSlice(self.lexer.current_token.toString()) catch {
            try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Maximum binary operator length exceeded");
            // FIXME: This will fail if we exceeded the binary operator length with an implicit self!
            return receiver;
        };

        _ = try self.lexer.nextToken(allocator);
    }

    var term = (try self.parseExpression(allocator, .Binary)) orelse return null;
    errdefer term.deinit(allocator);
    var binary_message_copy = try allocator.dupe(u8, binary_message_name.constSlice());
    errdefer allocator.free(binary_message_copy);
    var arguments = try allocator.alloc(AST.ExpressionNode, 1);
    errdefer allocator.free(arguments);
    arguments[0] = term;

    var message_node = try allocator.create(AST.MessageNode);
    message_node.* = .{
        .receiver = receiver,
        .message_name = binary_message_copy,
        .arguments = arguments,
        .range = .{ .start = if (receiver) |r| r.range().start else start_of_message, .end = term.range().end },
    };

    return AST.ExpressionNode{ .Message = message_node };
}

fn parseKeywordMessage(self: *Self, allocator: Allocator, receiver: ?AST.ExpressionNode) ParseError!?AST.ExpressionNode {
    // KeywordSend = firstKeyword Expression (restKeyword Expression)*
    std.debug.assert(self.lexer.current_token == .FirstKeyword);

    const start_of_message = self.lexer.token_start;
    const first_keyword_slice = std.mem.sliceTo(&self.lexer.current_token.FirstKeyword, 0);

    var message_name = try std.ArrayList(u8).initCapacity(allocator, first_keyword_slice.len);
    defer message_name.deinit();
    var arguments = try std.ArrayList(AST.ExpressionNode).initCapacity(allocator, 1);
    defer {
        for (arguments.items) |*expression| {
            expression.deinit(allocator);
        }
        arguments.deinit();
    }

    message_name.appendSliceAssumeCapacity(first_keyword_slice);
    _ = try self.lexer.nextToken(allocator);

    const first_expression = (try self.parseExpression(allocator, .Keyword)) orelse return null;
    // NOTE: No errdefer as arguments now owns first_expression.
    arguments.appendAssumeCapacity(first_expression);

    while (self.lexer.current_token == .RestKeyword) {
        const rest_keyword_slice = std.mem.sliceTo(&self.lexer.current_token.RestKeyword, 0);
        try message_name.appendSlice(rest_keyword_slice);
        _ = try self.lexer.nextToken(allocator);

        var expression = (try self.parseExpression(allocator, .Keyword)) orelse return null;
        errdefer expression.deinit(allocator);
        try arguments.append(expression);
    }

    const end_of_last_argument = arguments.items[arguments.items.len - 1].range().end;

    var message_node = try allocator.create(AST.MessageNode);
    errdefer allocator.destroy(message_node);

    message_node.* = .{
        .receiver = receiver,
        .message_name = message_name.toOwnedSlice(),
        .arguments = arguments.toOwnedSlice(),
        .range = .{ .start = if (receiver) |r| r.range().start else start_of_message, .end = end_of_last_argument },
    };

    return AST.ExpressionNode{ .Message = message_node };
}

fn parsePrimary(self: *Self, allocator: Allocator) ParseError!?AST.ExpressionNode {
    // Primary = Integer | FloatingPoint | Object | Block | identifier | String
    return switch (self.lexer.current_token) {
        .Integer => AST.ExpressionNode{ .Number = try self.parseInteger(allocator) },
        .FloatingPoint => AST.ExpressionNode{ .Number = try self.parseFloatingPoint(allocator) },
        .ParenOpen => (try self.parseSlotsObjectOrSubexpr(allocator, true, null, null)) orelse return null,
        .BracketOpen => AST.ExpressionNode{ .Block = (try self.parseBlock(allocator)) orelse return null },
        .Identifier => AST.ExpressionNode{ .Identifier = try self.parseIdentifier(allocator) },
        .String => AST.ExpressionNode{ .String = try self.parseString(allocator) },
        else => blk: {
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.lexer.token_start,
                "Expected primary expression, got {s}",
                .{self.lexer.current_token.toString()},
            );
            break :blk null;
        },
    };
}

fn parseSlotsObjectOrSubexpr(self: *Self, allocator: Allocator, must_not_be_method: bool, did_extract_expr: ?*bool, arguments: ?[]const AST.SlotNode) ParseError!?AST.ExpressionNode {
    var did_use_slots = false;
    var object = (try self.parseObject(allocator, &did_use_slots, arguments)) orelse return null;
    errdefer object.destroy(allocator);

    const statement_count = object.statements.value.statements.len;
    const slot_count = object.slots.len;
    if (!did_use_slots) {
        if (statement_count == 0) {
            // Just an empty slots object.
            return AST.ExpressionNode{ .Object = object };
        } else if (statement_count == 1) {
            // A sub-expression. We need to do some fiddling here to avoid destroying
            // the expression when we destroy the expression node.
            const subexpr = object.statements.value.statements[0];
            allocator.free(object.statements.value.statements);
            object.statements.value.statements = &.{};

            object.destroy(allocator);
            if (did_extract_expr) |flag| flag.* = true;
            return subexpr;
        } else {
            if (must_not_be_method)
                try self.diagnostics.reportDiagnostic(
                    .Error,
                    object.range.start,
                    "Sub-expressions cannot have more than one expression",
                );

            return AST.ExpressionNode{ .Object = object };
        }
    }

    if (slot_count == 0) {
        if (statement_count == 0) {
            // Just an empty slots object.
            return AST.ExpressionNode{ .Object = object };
        } else if (statement_count == 1) {
            if (must_not_be_method)
                try self.diagnostics.reportDiagnostic(
                    .Error,
                    object.range.start,
                    "Sub-expressions must not use slot delimiters",
                );

            return AST.ExpressionNode{ .Object = object };
        } else {
            if (must_not_be_method)
                try self.diagnostics.reportDiagnostic(
                    .Error,
                    object.range.start,
                    "Slots object cannot contain expressions, use methods",
                );

            return AST.ExpressionNode{ .Object = object };
        }
    } else if (statement_count != 0) {
        if (must_not_be_method)
            try self.diagnostics.reportDiagnostic(
                .Error,
                object.range.start,
                "Slots object cannot contain expressions, use methods",
            );

        return AST.ExpressionNode{ .Object = object };
    }

    return AST.ExpressionNode{ .Object = object };
}

fn parseObject(self: *Self, allocator: Allocator, did_use_slots: ?*bool, argument_slots: ?[]const AST.SlotNode) ParseError!?*AST.ObjectNode {
    // Object = "(" SlotList<ObjectSlot>? ")"
    // Method = "(" SlotList<ObjectSlot>? StatementList ")"
    std.debug.assert(self.lexer.current_token == .ParenOpen);

    const start_of_object = self.lexer.token_start;
    _ = try self.lexer.nextToken(allocator);

    var slots = std.ArrayList(AST.SlotNode).init(allocator);
    defer {
        for (slots.items) |*slot| {
            slot.deinit(allocator);
        }
        slots.deinit();
    }

    var slot_list_order_offset: usize = 0;
    if (argument_slots) |slot_list| {
        slot_list_order_offset = slot_list.len;
        try slots.appendSlice(slot_list);
    }

    if (self.lexer.current_token == .Pipe) {
        if (did_use_slots) |flag| flag.* = true;
        const slot_list = (try self.parseSlotList(.Object, allocator, slot_list_order_offset)) orelse return null;
        defer allocator.free(slot_list);
        errdefer for (slot_list) |*slot| {
            slot.deinit(allocator);
        };

        try slots.appendSlice(slot_list);
    }

    var statements = blk: {
        if (self.lexer.current_token != .ParenClose)
            break :blk try self.parseStatementList(allocator, false);
        break :blk try AST.StatementList.create(allocator, &.{});
    };
    defer statements.unrefWithAllocator(allocator);

    const end_of_object = self.lexer.token_end;
    if (!try self.expect(allocator, .ParenClose, .Consume)) {
        return null;
    }

    statements.ref();
    var object_node = try allocator.create(AST.ObjectNode);
    object_node.* = .{
        .slots = slots.toOwnedSlice(),
        .statements = statements,
        .range = .{ .start = start_of_object, .end = end_of_object },
    };

    return object_node;
}

fn parseBlock(self: *Self, allocator: Allocator) ParseError!?*AST.BlockNode {
    // Block = "[" SlotList<BlockSlot>? StatementList "]"
    std.debug.assert(self.lexer.current_token == .BracketOpen);

    const start_of_block = self.lexer.token_start;
    _ = try self.lexer.nextToken(allocator);

    var slots = if (self.lexer.current_token == .Pipe)
        (try self.parseSlotList(.Block, allocator, 0)) orelse return null
    else
        &[_]AST.SlotNode{};
    defer {
        for (slots) |*slot| {
            slot.deinit(allocator);
        }
        allocator.free(slots);
    }

    var statements = blk: {
        if (self.lexer.current_token != .BracketClose)
            break :blk try self.parseStatementList(allocator, true);
        break :blk try AST.StatementList.create(allocator, &.{});
    };
    defer statements.unrefWithAllocator(allocator);

    const end_of_block = self.lexer.token_end;
    if (!try self.expect(allocator, .BracketClose, .Consume)) {
        return null;
    }

    var block_node = try allocator.create(AST.BlockNode);
    block_node.* = .{
        .slots = slots,
        .statements = statements,
        .range = .{ .start = start_of_block, .end = end_of_block },
    };
    // Replace slots with an empty slice so that we don't free the slots after
    // we exit.
    slots = &.{};
    statements.ref();

    return block_node;
}

const SlotListType = enum { Object, Block };
fn parseSlotList(self: *Self, comptime slot_list_type: SlotListType, allocator: Allocator, initial_order_offset: usize) ParseError!?[]AST.SlotNode {
    // SlotList<SlotType> = "|" "|" | "|" SlotType ("." SlotType)* "."? "|"
    std.debug.assert(self.lexer.current_token == .Pipe);
    _ = try self.lexer.nextToken(allocator);

    var slots = std.ArrayList(AST.SlotNode).init(allocator);
    defer {
        for (slots.items) |*slot| {
            slot.deinit(allocator);
        }
        slots.deinit();
    }

    const allow_argument = switch (slot_list_type) {
        .Object => false,
        .Block => true,
    };

    const allow_inherited = switch (slot_list_type) {
        .Object => true,
        .Block => false,
    };

    var order: usize = initial_order_offset;
    if (self.lexer.current_token != .Pipe) first_slot_parsing: {
        {
            var first_slot = (try self.parseSlot(allocator, order, allow_argument, allow_inherited)) orelse break :first_slot_parsing;
            errdefer first_slot.deinit(allocator);
            try slots.append(first_slot);
            order += 1;
        }

        while (self.lexer.current_token == .Period) {
            _ = try self.lexer.nextToken(allocator);
            if (!self.canParseSlot()) break;

            var slot = (try self.parseSlot(allocator, order, allow_argument, allow_inherited)) orelse continue;
            errdefer slot.deinit(allocator);
            try slots.append(slot);
            order += 1;
        }
    }

    if (!try self.expect(allocator, .Pipe, .Consume)) {
        while (self.lexer.current_token != .Pipe and self.lexer.current_token != .EOF)
            _ = try self.lexer.nextToken(allocator);
        if (self.lexer.current_token == .Pipe)
            _ = try self.lexer.nextToken(allocator);
    }

    // Sort inherited slots first.
    std.sort.sort(AST.SlotNode, slots.items, void{}, struct {
        fn lessThan(context: void, lhs: AST.SlotNode, rhs: AST.SlotNode) bool {
            _ = context;

            if (lhs.is_inherited != rhs.is_inherited)
                return lhs.is_inherited;
            return lhs.order < rhs.order;
        }
    }.lessThan);

    return slots.toOwnedSlice();
}

fn canParseSlot(self: *Self) bool {
    if (self.lexer.current_token.isOperator())
        return true;
    return switch (self.lexer.current_token) {
        .FirstKeyword, .Identifier, .Colon => true,
        else => false,
    };
}

const MethodMode = enum { Required, Optional, Forbidden };
fn parseSlot(self: *Self, allocator: Allocator, order: usize, allow_argument: bool, allow_inherited: bool) ParseError!?AST.SlotNode {
    //   CommonSlots = identifier "*"? ("=" | "<-") Expression -- slot
    //               | SlotName "=" Method                     -- method
    //               | identifier                              -- default init to nil
    //   ObjectSlot = CommonSlots
    //              | identifier "<" "=" Expression            -- inherited
    //   BlockSlot = CommonSlots
    //             | ":" identifier                            -- argument

    const start_of_slot = self.lexer.token_start;

    var is_argument = false;
    var is_mutable = false;
    var is_parent = false;
    var is_inherited = false;

    if (self.lexer.current_token == .Colon) {
        if (!allow_argument) {
            try self.diagnostics.reportDiagnostic(
                .Error,
                self.lexer.token_start,
                "Argument slots are not allowed in this slot list",
            );
            return null;
        }

        is_argument = true;
        _ = try self.lexer.nextToken(allocator);
    }

    var did_parse_successfully = false;
    var method_mode = MethodMode.Optional;
    var arguments = std.ArrayList(AST.SlotNode).init(allocator);
    defer arguments.deinit();

    var slot_name = blk: {
        errdefer for (arguments.items) |*slot| {
            slot.deinit(allocator);
        };

        break :blk (try self.parseSlotName(allocator, &arguments, &method_mode)) orelse return null;
    };
    defer if (!did_parse_successfully) allocator.free(slot_name);

    if (method_mode == .Required) {
        if (!try self.expect(allocator, .Equals, .Consume)) return null;
    } else if (self.lexer.current_token == .LessThan) {
        if (!allow_inherited) {
            try self.diagnostics.reportDiagnostic(
                .Error,
                self.lexer.token_start,
                "Inherited slots are not allowed in this slot list",
            );
            return null;
        }

        is_inherited = true;
        method_mode = .Forbidden;
        _ = try self.lexer.nextToken(allocator);

        if (!try self.expect(allocator, .Equals, .Consume)) return null;
    } else {
        if (self.lexer.current_token == .Asterisk) {
            is_parent = true;
            _ = try self.lexer.nextToken(allocator);
        }

        if (self.lexer.current_token == .Arrow) {
            is_mutable = true;
        } else if (self.lexer.current_token == .Period or self.lexer.current_token == .Pipe) {
            // This slot has no value. Just exit.
            did_parse_successfully = true;
            return AST.SlotNode{
                .is_mutable = true,
                .is_parent = is_parent,
                .is_argument = is_argument,
                .is_inherited = is_inherited,
                .order = order,
                .name = slot_name,
                .value = null,
                .range = .{ .start = start_of_slot, .end = self.lexer.token_start },
            };
        } else if (self.lexer.current_token != .Equals) {
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.lexer.token_start,
                "Expected arrow or equals after slot name, got {s}",
                .{self.lexer.current_token.toString()},
            );
            return null;
        }

        _ = try self.lexer.nextToken(allocator);
    }

    var value = switch (method_mode) {
        .Required => blk: {
            // NOTE: Just for fun, turn this into:
            //           break :blk AST.ExpressionNode{ ...
            //       and watch the Zig compiler crash and burn.
            const expression_node = AST.ExpressionNode{
                .Object = (try self.parseObject(allocator, null, arguments.items)) orelse return null,
            };
            break :blk expression_node;
        },
        .Optional => blk: {
            if (self.lexer.current_token == .ParenOpen) {
                // It's entirely possible for the user to do this:
                //     value = (1 + 2) * 3.
                // Or this:
                //     value = (| foo = ... |) foo.
                // We don't want to disallow these, so we look at the object and see
                // whether it is a slots object or sub-expr, and if it is we look
                // ahead to the next token to see whether it would be a valid message
                // token. If it is, we turn this into a message instead (through the
                // use of parseExpressionFromPrimary).

                var did_extract_expr = false;
                var expr = (try self.parseSlotsObjectOrSubexpr(allocator, false, &did_extract_expr, arguments.items)) orelse return null;
                errdefer expr.deinit(allocator);
                if (expr == .Object and expr.Object.statements.value.statements.len > 0) {
                    // This is a method, we cannot put expressions after it.
                    break :blk expr;
                }

                if (self.lexer.current_token == .Period or self.lexer.current_token == .Pipe) {
                    // This is a method, but it looks like a sub-expression. If it was extracted
                    // into an expression, then re-pack it, and then return it from this block.
                    if (did_extract_expr) {
                        var statement_slice = try allocator.alloc(AST.ExpressionNode, 1);
                        errdefer allocator.free(statement_slice);
                        statement_slice[0] = expr;

                        var statement_list = try AST.StatementList.create(allocator, statement_slice);
                        errdefer statement_list.unrefWithAllocator(allocator);

                        var object_node = try allocator.create(AST.ObjectNode);
                        object_node.* = .{
                            .slots = &.{},
                            .statements = statement_list,
                            .range = expr.range(),
                        };
                        expr = AST.ExpressionNode{ .Object = object_node };
                    }

                    break :blk expr;
                }

                break :blk (try self.parseExpressionFromPrimary(allocator, expr, .Any, false)) orelse return null;
            }

            break :blk (try self.parseExpression(allocator, .Any)) orelse return null;
        },
        .Forbidden => (try self.parseExpression(allocator, .Any)) orelse return null,
    };

    did_parse_successfully = true;
    return AST.SlotNode{
        .is_mutable = is_mutable,
        .is_parent = is_parent,
        .is_argument = is_argument,
        .is_inherited = is_inherited,
        .order = order,
        .name = slot_name,
        .value = value,
        .range = .{ .start = start_of_slot, .end = value.range().end },
    };
}

/// Creates an argument slot node from the current identifier.
fn createSlotNodeFromArgument(self: *Self, allocator: Allocator, order: usize) ParseError!AST.SlotNode {
    std.debug.assert(self.lexer.current_token == .Identifier);

    const identifier_slice = std.mem.sliceTo(&self.lexer.current_token.Identifier, 0);
    var identifier_copy = try allocator.dupe(u8, identifier_slice);
    errdefer allocator.free(identifier_copy);

    const slot_node = AST.SlotNode{
        .is_mutable = true,
        .is_parent = false,
        .is_argument = true,
        .is_inherited = false,
        .order = order,
        .name = identifier_copy,
        .value = null,
        .range = .{ .start = self.lexer.token_start, .end = self.lexer.token_end },
    };
    _ = try self.lexer.nextToken(allocator);
    return slot_node;
}

fn parseSlotName(self: *Self, allocator: Allocator, arguments: *std.ArrayList(AST.SlotNode), method_mode: *MethodMode) ParseError!?[]const u8 {
    var order: usize = 0;

    if (self.lexer.current_token == .Identifier) {
        // Identifier means this is a unary message, no need to do anymore work.
        method_mode.* = .Optional;

        const identifier_slice = std.mem.sliceTo(&self.lexer.current_token.Identifier, 0);
        var identifier_copy = try allocator.dupe(u8, identifier_slice);
        errdefer allocator.free(identifier_copy);

        _ = try self.lexer.nextToken(allocator);
        return identifier_copy;
    }

    if (self.lexer.current_token == .FirstKeyword) {
        method_mode.* = .Required;

        const first_keyword_slice = std.mem.sliceTo(&self.lexer.current_token.FirstKeyword, 0);
        var slot_name = try std.ArrayList(u8).initCapacity(allocator, first_keyword_slice.len);
        defer slot_name.deinit();
        slot_name.appendSliceAssumeCapacity(first_keyword_slice);

        _ = try self.lexer.nextToken(allocator);

        if (!try self.expect(allocator, .Identifier, .DontConsume))
            return null;

        try arguments.append(try self.createSlotNodeFromArgument(allocator, order));
        order += 1;

        while (self.lexer.current_token == .RestKeyword) {
            const keyword_slice = std.mem.sliceTo(&self.lexer.current_token.RestKeyword, 0);
            try slot_name.appendSlice(keyword_slice);
            _ = try self.lexer.nextToken(allocator);

            if (!try self.expect(allocator, .Identifier, .DontConsume))
                return null;

            try arguments.append(try self.createSlotNodeFromArgument(allocator, order));
            order += 1;
        }

        return slot_name.toOwnedSlice();
    }

    if (!self.lexer.current_token.isOperator()) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.lexer.token_start,
            "Expected slot name, got {s}",
            .{self.lexer.current_token.toString()},
        );
        return null;
    }

    method_mode.* = .Required;
    var binary_message_name = std.BoundedArray(u8, tokens.MaximumIdentifierLength).init(0) catch unreachable;

    // FIXME: Disallow space between operator tokens.
    while (self.lexer.current_token.isOperator()) {
        binary_message_name.appendSlice(self.lexer.current_token.toString()) catch {
            try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Maximum binary operator length exceeded");
            return null;
        };

        _ = try self.lexer.nextToken(allocator);
    }

    if (!try self.expect(allocator, .Identifier, .DontConsume))
        return null;

    try arguments.append(try self.createSlotNodeFromArgument(allocator, order));
    return try allocator.dupe(u8, binary_message_name.constSlice());
}

fn parseString(self: *Self, allocator: Allocator) ParseError!AST.StringNode {
    std.debug.assert(self.lexer.current_token == .String);

    var string_copy = try allocator.dupe(u8, self.lexer.current_token.String);
    errdefer allocator.free(string_copy);

    const node = AST.StringNode{
        .value = string_copy,
        .range = .{ .start = self.lexer.token_start, .end = self.lexer.token_end },
    };
    _ = try self.lexer.nextToken(allocator);
    return node;
}

fn parseIdentifier(self: *Self, allocator: Allocator) ParseError!AST.IdentifierNode {
    std.debug.assert(self.lexer.current_token == .Identifier);

    const identifier_slice = std.mem.sliceTo(&self.lexer.current_token.Identifier, 0);
    var identifier_copy = try allocator.dupe(u8, identifier_slice);
    errdefer allocator.free(identifier_copy);

    const node = AST.IdentifierNode{
        .value = identifier_copy,
        .range = .{ .start = self.lexer.token_start, .end = self.lexer.token_end },
    };
    _ = try self.lexer.nextToken(allocator);
    return node;
}

fn parseInteger(self: *Self, allocator: Allocator) ParseError!AST.NumberNode {
    std.debug.assert(self.lexer.current_token == .Integer);

    const node = AST.NumberNode{
        .value = .{ .Integer = self.lexer.current_token.Integer },
        .range = .{ .start = self.lexer.token_start, .end = self.lexer.token_end },
    };
    _ = try self.lexer.nextToken(allocator);
    return node;
}

fn parseFloatingPoint(self: *Self, allocator: Allocator) ParseError!AST.NumberNode {
    std.debug.assert(self.lexer.current_token == .FloatingPoint);

    const node = AST.NumberNode{
        .value = .{ .FloatingPoint = self.lexer.current_token.FloatingPoint },
        .range = .{ .start = self.lexer.token_start, .end = self.lexer.token_end },
    };
    _ = try self.lexer.nextToken(allocator);
    return node;
}

fn errorSetOf(comptime Fn: anytype) type {
    return @typeInfo(@typeInfo(@TypeOf(Fn)).Fn.return_type.?).ErrorUnion.error_set;
}

const ExpectTokenAction = enum { Consume, DontConsume };
fn expect(self: *Self, allocator: Allocator, token_type: std.meta.Tag(tokens.Token), action: ExpectTokenAction) ParseError!bool {
    if (self.lexer.current_token != token_type) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.lexer.token_start,
            "Expected '{s}', got '{s}'",
            .{ tokens.tokenTypeToString(token_type), self.lexer.current_token.toString() },
        );
        return false;
    }

    switch (action) {
        .Consume => _ = try self.lexer.nextToken(allocator),
        .DontConsume => {},
    }

    return true;
}
