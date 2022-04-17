// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const AST = @import("./ast.zig");
const Diagnostics = @import("./diagnostics.zig");
const Tokenizer = @import("./Tokenizer.zig");
const Token = @import("./Token.zig");
const Location = @import("./location.zig");

allocator: Allocator,
buffer: [:0]const u8,
diagnostics: Diagnostics,

tokens: std.MultiArrayList(struct {
    tag: Token.Tag,
    start: usize,
}),
token_tags: []Token.Tag = undefined,
token_starts: []usize = undefined,
token_index: usize,
line_offsets: std.ArrayListUnmanaged(usize),

pub const ParseError = Allocator.Error;
const Self = @This();

pub fn createFromFile(allocator: Allocator, file_path: []const u8) !*Self {
    const cwd = std.fs.cwd();
    const file = try cwd.openFile(file_path, .{});
    defer file.close();

    var file_contents = try file.readToEndAllocOptions(allocator, std.math.maxInt(usize), null, @alignOf(u8), 0);
    errdefer allocator.free(file_contents);

    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    try self.init(allocator, file_contents);
    return self;
}

pub fn createFromString(allocator: Allocator, contents: []const u8) !*Self {
    var contents_copy = try allocator.dupeZ(u8, contents);
    errdefer allocator.free(contents_copy);

    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);
    try self.init(allocator, contents_copy);
    return self;
}

fn init(self: *Self, allocator: Allocator, buffer: [:0]const u8) !void {
    self.* = .{
        .allocator = allocator,
        .buffer = buffer,
        .token_index = 0,
        .tokens = .{},
        .line_offsets = .{},
        .diagnostics = try Diagnostics.init(allocator),
    };
    errdefer self.diagnostics.deinit();
    errdefer self.tokens.deinit(allocator);
    errdefer self.line_offsets.deinit(allocator);

    var tokenizer = Tokenizer.init(self.buffer);
    while (true) {
        const token = tokenizer.next();
        try self.tokens.append(allocator, .{
            .tag = token.tag,
            .start = token.location.start,
        });

        if (token.tag == .EOF) break;
    }

    self.token_tags = self.tokens.items(.tag);
    self.token_starts = self.tokens.items(.start);

    {
        try self.line_offsets.append(allocator, 0);
        var offset: usize = 0;
        var will_insert_newline = false;

        while (offset < self.buffer.len) : (offset += 1) {
            if (will_insert_newline) {
                try self.line_offsets.append(allocator, offset);
                will_insert_newline = false;
            }

            if (self.buffer[offset] == '\n') {
                will_insert_newline = true;
            }
        }
    }
}

fn deinit(self: *Self) void {
    self.line_offsets.deinit(self.allocator);
    self.tokens.deinit(self.allocator);
    self.allocator.free(self.buffer);
    self.diagnostics.deinit();
}

pub fn destroy(self: *Self) void {
    self.deinit();
    self.allocator.destroy(self);
}

pub fn parseScript(self: *Self) ParseError!AST.ScriptNode {
    // Script = StatementList
    const statements = try self.parseStatementList(false);
    errdefer statements.unrefWithAllocator(self.allocator);

    if (self.token_tags[self.token_index] != .EOF) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.offsetToLocation(self.token_starts[self.token_index]),
            "Expected end-of-file after last statement, got {s}",
            .{self.token_tags[self.token_index].symbol()},
        );
    }

    return AST.ScriptNode{ .statements = statements };
}

fn parseStatementList(self: *Self, allow_nonlocal_return: bool) ParseError!AST.StatementList.Ref {
    // StatementList = Statement ("." Statement)* "."? | empty

    var statements = std.ArrayList(AST.ExpressionNode).init(self.allocator);
    defer {
        for (statements.items) |*expression| {
            expression.deinit(self.allocator);
        }
        statements.deinit();
    }

    var nonlocal_return_state: NonlocalReturnState = if (allow_nonlocal_return) .Allowed else .Disallowed;

    if (self.token_tags[self.token_index] != .EOF) first_expr_parsing: {
        const first_statement = (try self.parseStatement(&nonlocal_return_state)) orelse break :first_expr_parsing;
        try statements.append(first_statement);

        while (self.consumeToken(.Period)) |_| {
            const statement = (try self.parseStatement(&nonlocal_return_state)) orelse break;
            try statements.append(statement);
        }
    }

    return AST.StatementList.create(self.allocator, statements.toOwnedSlice());
}

const NonlocalReturnState = enum { Disallowed, Allowed, WrappingThisExpr, Parsed };
fn parseStatement(self: *Self, nonlocal_return_state: *NonlocalReturnState) ParseError!?AST.ExpressionNode {
    // Statement = "^"? Expression
    var start_of_expression = self.token_starts[self.token_index];
    if (self.consumeToken(.Cap)) |nonlocal_return_token| {
        const cap_offset = self.token_starts[nonlocal_return_token];
        start_of_expression = cap_offset;

        switch (nonlocal_return_state.*) {
            .Disallowed => {
                try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(cap_offset), "Non-local returns are not allowed here");
            },
            .Allowed => {
                nonlocal_return_state.* = .WrappingThisExpr;
            },
            .Parsed => {
                try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(cap_offset), "Unexpected statement after non-local return");
            },
            else => unreachable,
        }
    }

    if (!self.canParseExpression()) {
        if (nonlocal_return_state.* == .WrappingThisExpr)
            try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(self.token_starts[self.token_index]), "Expected expression after non-local return");
        return null;
    }

    var expression = (try self.parseExpression(.Any)) orelse return null;
    errdefer expression.deinit(self.allocator);

    switch (nonlocal_return_state.*) {
        .WrappingThisExpr => {
            var return_node = try self.allocator.create(AST.ReturnNode);
            return_node.* = .{
                .expression = expression,
                .range = .{ .start = self.offsetToLocation(start_of_expression), .end = expression.range().end },
            };
            expression = AST.ExpressionNode{ .Return = return_node };

            nonlocal_return_state.* = .Parsed;
        },
        .Parsed => {
            try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(self.token_starts[self.token_index]), "Unexpected statement after non-local return");
        },
        .Allowed, .Disallowed => {},
    }

    return expression;
}

fn canParseExpression(self: *Self) bool {
    const current_tag = self.token_tags[self.token_index];
    // binary to self
    if (current_tag.isOperator())
        return true;

    return switch (current_tag) {
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
fn parseExpression(self: *Self, precedence: MessagePrecedence) ParseError!?AST.ExpressionNode {
    // Expression = KeywordSend
    //            | BinarySend
    //            | Primary UnarySend* BinarySend? KeywordSend?

    var primary: AST.ExpressionNode = undefined;

    const current_tag = self.token_tags[self.token_index];
    if (current_tag == .FirstKeyword)
        primary = (try self.parseKeywordMessage(null)) orelse return null
    else if (current_tag.isOperator())
        primary = (try self.parseBinaryMessage(null)) orelse return null
    else
        primary = (try self.parsePrimary()) orelse return null;
    errdefer primary.deinit(self.allocator);

    return try self.parseExpressionFromPrimary(primary, precedence, false);
}

// If require_message is true then the primary must receive at least one message.
fn parseExpressionFromPrimary(
    self: *Self,
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
                    s.offsetToLocation(s.token_starts[s.token_index]),
                    "Expected unary, binary or keyword message, got {s}",
                    .{s.token_tags[s.token_index].symbol()},
                );
            }
        }
    }.f;

    // Collect unary messages
    while (self.consumeToken(.Identifier)) |identifier_token| {
        did_send_message = true;

        var message_node = try self.allocator.create(AST.MessageNode);
        errdefer self.allocator.destroy(message_node);

        const identifier_copy = try self.getIdentifierCopy(identifier_token);
        const end_of_identifier = self.token_starts[identifier_token] + identifier_copy.len;

        message_node.* = .{
            .receiver = expr,
            .message_name = identifier_copy,
            .arguments = &.{},
            .range = .{ .start = expr.range().start, .end = self.offsetToLocation(end_of_identifier) },
        };
        expr = AST.ExpressionNode{ .Message = message_node };
    }

    // If possible, parse a binary message.
    if (self.token_tags[self.token_index].isOperator()) {
        did_send_message = true;
        expr = (try self.parseBinaryMessage(expr)) orelse return null;
    }

    if (precedence == .Binary) {
        try emitDiagnosticIfNeeded(self, require_message, did_send_message);
        return expr;
    }

    // Otherwise, try parsing a keyword message.
    if (self.token_tags[self.token_index] == .FirstKeyword) {
        did_send_message = true;
        expr = (try self.parseKeywordMessage(expr)) orelse return null;
    }

    if (precedence == .Keyword) {
        try emitDiagnosticIfNeeded(self, require_message, did_send_message);
        return expr;
    }

    // If the current token is a semicolon then we want to send more messages to it,
    // so try parsing an expression after it with the current expression as the
    // "primary".
    if (self.token_tags[self.token_index] == .Semicolon) {
        _ = self.nextToken();
        expr = (try self.parseExpressionFromPrimary(expr, .Any, true)) orelse return null;
    }

    return expr;
}

fn parseBinaryMessage(self: *Self, receiver: ?AST.ExpressionNode) ParseError!?AST.ExpressionNode {
    // BinarySend = binaryOp+ Expression
    std.debug.assert(self.token_tags[self.token_index].isOperator());

    const start_of_message = self.token_starts[self.token_index];
    // FIXME: De-magic this number
    var binary_message_name = std.BoundedArray(u8, 128).init(0) catch unreachable;

    // FIXME: Disallow space between operator tokens.
    while (self.token_tags[self.token_index].isOperator()) {
        binary_message_name.appendSlice(self.token_tags[self.token_index].symbol()) catch {
            try self.diagnostics.reportDiagnostic(
                .Error,
                self.offsetToLocation(self.token_starts[self.token_index]),
                "Maximum binary operator length exceeded",
            );
            // FIXME: This will fail if we exceeded the binary operator length with an implicit self!
            return receiver;
        };

        _ = self.nextToken();
    }

    var term = (try self.parseExpression(.Binary)) orelse return null;
    errdefer term.deinit(self.allocator);
    var binary_message_copy = try self.allocator.dupe(u8, binary_message_name.constSlice());
    errdefer self.allocator.free(binary_message_copy);
    var arguments = try self.allocator.alloc(AST.ExpressionNode, 1);
    errdefer self.allocator.free(arguments);
    arguments[0] = term;

    var message_node = try self.allocator.create(AST.MessageNode);
    message_node.* = .{
        .receiver = receiver,
        .message_name = binary_message_copy,
        .arguments = arguments,
        .range = .{ .start = if (receiver) |r| r.range().start else self.offsetToLocation(start_of_message), .end = term.range().end },
    };

    return AST.ExpressionNode{ .Message = message_node };
}

fn parseKeywordMessage(self: *Self, receiver: ?AST.ExpressionNode) ParseError!?AST.ExpressionNode {
    // KeywordSend = firstKeyword Expression (restKeyword Expression)*
    const first_keyword_token = self.assertToken(.FirstKeyword);
    const start_of_message = self.token_starts[first_keyword_token];
    const first_keyword_slice = self.getKeywordSlice(first_keyword_token);

    var message_name = try std.ArrayList(u8).initCapacity(self.allocator, first_keyword_slice.len);
    defer message_name.deinit();
    var arguments = try std.ArrayList(AST.ExpressionNode).initCapacity(self.allocator, 1);
    defer {
        for (arguments.items) |*expression| {
            expression.deinit(self.allocator);
        }
        arguments.deinit();
    }

    message_name.appendSliceAssumeCapacity(first_keyword_slice);

    const first_expression = (try self.parseExpression(.Keyword)) orelse return null;
    // NOTE: No errdefer as arguments now owns first_expression.
    arguments.appendAssumeCapacity(first_expression);

    while (self.consumeToken(.RestKeyword)) |rest_keyword_token| {
        const rest_keyword_slice = self.getKeywordSlice(rest_keyword_token);
        try message_name.appendSlice(rest_keyword_slice);

        var expression = (try self.parseExpression(.Keyword)) orelse return null;
        errdefer expression.deinit(self.allocator);
        try arguments.append(expression);
    }

    const end_of_last_argument = arguments.items[arguments.items.len - 1].range().end;

    var message_node = try self.allocator.create(AST.MessageNode);
    errdefer self.allocator.destroy(message_node);

    message_node.* = .{
        .receiver = receiver,
        .message_name = message_name.toOwnedSlice(),
        .arguments = arguments.toOwnedSlice(),
        .range = .{ .start = if (receiver) |r| r.range().start else self.offsetToLocation(start_of_message), .end = end_of_last_argument },
    };

    return AST.ExpressionNode{ .Message = message_node };
}

fn parsePrimary(self: *Self) ParseError!?AST.ExpressionNode {
    // Primary = Integer | FloatingPoint | Object | Block | identifier | String
    return switch (self.token_tags[self.token_index]) {
        .Integer => AST.ExpressionNode{ .Number = try self.parseInteger() },
        .FloatingPoint => AST.ExpressionNode{ .Number = try self.parseFloatingPoint() },
        .ParenOpen => (try self.parseSlotsObjectOrSubexpr(true, null, null)) orelse return null,
        .BracketOpen => AST.ExpressionNode{ .Block = (try self.parseBlock()) orelse return null },
        .Identifier => AST.ExpressionNode{ .Identifier = try self.parseIdentifier() },
        .String => AST.ExpressionNode{ .String = (try self.parseString()) orelse return null },
        else => blk: {
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.offsetToLocation(self.token_starts[self.token_index]),
                "Expected primary expression, got {s}",
                .{self.token_tags[self.token_index].symbol()},
            );
            break :blk null;
        },
    };
}

fn parseSlotsObjectOrSubexpr(self: *Self, must_not_be_method: bool, did_extract_expr: ?*bool, arguments: ?[]const AST.SlotNode) ParseError!?AST.ExpressionNode {
    var did_use_slots = false;
    var object = (try self.parseObject(&did_use_slots, arguments)) orelse return null;
    errdefer object.destroy(self.allocator);

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
            self.allocator.free(object.statements.value.statements);
            object.statements.value.statements = &.{};

            object.destroy(self.allocator);
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

fn parseObject(self: *Self, did_use_slots: ?*bool, argument_slots: ?[]const AST.SlotNode) ParseError!?*AST.ObjectNode {
    // Object = "(" SlotList<ObjectSlot>? ")"
    // Method = "(" SlotList<ObjectSlot>? StatementList ")"
    const paren_open_token = self.assertToken(.ParenOpen);
    const start_of_object = self.token_starts[paren_open_token];

    var slots = std.ArrayList(AST.SlotNode).init(self.allocator);
    defer {
        for (slots.items) |*slot| {
            slot.deinit(self.allocator);
        }
        slots.deinit();
    }

    var slot_list_order_offset: usize = 0;
    if (argument_slots) |slot_list| {
        slot_list_order_offset = slot_list.len;
        try slots.appendSlice(slot_list);
    }

    if (self.token_tags[self.token_index] == .Pipe) {
        if (did_use_slots) |flag| flag.* = true;
        const slot_list = (try self.parseSlotList(.Object, slot_list_order_offset)) orelse return null;
        defer self.allocator.free(slot_list);
        errdefer for (slot_list) |*slot| {
            slot.deinit(self.allocator);
        };

        try slots.appendSlice(slot_list);
    }

    var statements = blk: {
        if (self.token_tags[self.token_index] != .ParenClose)
            break :blk try self.parseStatementList(false);
        break :blk try AST.StatementList.create(self.allocator, &.{});
    };
    defer statements.unrefWithAllocator(self.allocator);

    if (self.consumeToken(.ParenClose) == null) {
        return null;
    }
    const end_of_object = self.token_starts[self.token_index];

    statements.ref();
    var object_node = try self.allocator.create(AST.ObjectNode);
    object_node.* = .{
        .slots = slots.toOwnedSlice(),
        .statements = statements,
        .range = .{ .start = self.offsetToLocation(start_of_object), .end = self.offsetToLocation(end_of_object) },
    };

    return object_node;
}

fn parseBlock(self: *Self) ParseError!?*AST.BlockNode {
    // Block = "[" SlotList<BlockSlot>? StatementList "]"
    const bracket_open_token = self.assertToken(.BracketOpen);
    const start_of_block = self.token_starts[bracket_open_token];

    var slots = if (self.token_tags[self.token_index] == .Pipe)
        (try self.parseSlotList(.Block, 0)) orelse return null
    else
        &[_]AST.SlotNode{};
    defer {
        for (slots) |*slot| {
            slot.deinit(self.allocator);
        }
        self.allocator.free(slots);
    }

    var statements = blk: {
        if (self.token_tags[self.token_index] != .BracketClose)
            break :blk try self.parseStatementList(true);
        break :blk try AST.StatementList.create(self.allocator, &.{});
    };
    defer statements.unrefWithAllocator(self.allocator);

    if (self.consumeToken(.BracketClose) == null) {
        return null;
    }

    const end_of_block = self.token_starts[self.token_index] + 1;

    var block_node = try self.allocator.create(AST.BlockNode);
    block_node.* = .{
        .slots = slots,
        .statements = statements,
        .range = .{ .start = self.offsetToLocation(start_of_block), .end = self.offsetToLocation(end_of_block) },
    };
    // Replace slots with an empty slice so that we don't free the slots after
    // we exit.
    slots = &.{};
    statements.ref();

    return block_node;
}

const SlotListType = enum { Object, Block };
fn parseSlotList(self: *Self, comptime slot_list_type: SlotListType, initial_order_offset: usize) ParseError!?[]AST.SlotNode {
    // SlotList<SlotType> = "|" "|" | "|" SlotType ("." SlotType)* "."? "|"
    _ = self.assertToken(.Pipe);

    var slots = std.ArrayList(AST.SlotNode).init(self.allocator);
    defer {
        for (slots.items) |*slot| {
            slot.deinit(self.allocator);
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
    if (self.token_tags[self.token_index] != .Pipe) first_slot_parsing: {
        {
            var first_slot = (try self.parseSlot(order, allow_argument, allow_inherited)) orelse break :first_slot_parsing;
            errdefer first_slot.deinit(self.allocator);
            try slots.append(first_slot);
            order += 1;
        }

        while (self.consumeToken(.Period)) |_| {
            if (!self.canParseSlot()) break;

            var slot = (try self.parseSlot(order, allow_argument, allow_inherited)) orelse continue;
            errdefer slot.deinit(self.allocator);
            try slots.append(slot);
            order += 1;
        }
    }

    if (!try self.expectToken(.Pipe)) {
        while (self.token_tags[self.token_index] != .Pipe and self.token_tags[self.token_index] != .EOF)
            _ = self.nextToken();
        if (self.token_tags[self.token_index] == .Pipe)
            _ = self.nextToken();
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
    if (self.token_tags[self.token_index].isOperator())
        return true;
    return switch (self.token_tags[self.token_index]) {
        .FirstKeyword, .Identifier, .Colon => true,
        else => false,
    };
}

const MethodMode = enum { Required, Optional, Forbidden };
fn parseSlot(self: *Self, order: usize, allow_argument: bool, allow_inherited: bool) ParseError!?AST.SlotNode {
    //   CommonSlots = identifier "*"? ("=" | "<-") Expression -- slot
    //               | SlotName "=" Method                     -- method
    //               | identifier                              -- default init to nil
    //   ObjectSlot = CommonSlots
    //              | identifier "<" "=" Expression            -- inherited
    //   BlockSlot = CommonSlots
    //             | ":" identifier                            -- argument

    const start_of_slot = self.token_starts[self.token_index];

    var is_argument = false;
    var is_mutable = false;
    var is_parent = false;
    var is_inherited = false;

    if (self.token_tags[self.token_index] == .Colon) {
        if (!allow_argument) {
            try self.diagnostics.reportDiagnostic(
                .Error,
                self.offsetToLocation(start_of_slot),
                "Argument slots are not allowed in this slot list",
            );
            return null;
        }

        is_argument = true;
        _ = self.nextToken();
    }

    var did_parse_successfully = false;
    var method_mode = MethodMode.Optional;
    var arguments = std.ArrayList(AST.SlotNode).init(self.allocator);
    defer arguments.deinit();

    var slot_name = blk: {
        errdefer for (arguments.items) |*slot| {
            slot.deinit(self.allocator);
        };

        break :blk (try self.parseSlotName(&arguments, &method_mode)) orelse return null;
    };
    defer if (!did_parse_successfully) self.allocator.free(slot_name);

    if (method_mode == .Required) {
        if (!try self.expectToken(.Equals)) return null;
    } else if (self.consumeToken(.LessThan)) |less_than_token| {
        if (!allow_inherited) {
            try self.diagnostics.reportDiagnostic(
                .Error,
                self.offsetToLocation(self.token_starts[less_than_token]),
                "Inherited slots are not allowed in this slot list",
            );
            return null;
        }

        is_inherited = true;
        method_mode = .Forbidden;

        if (!try self.expectToken(.Equals))
            return null;
    } else {
        if (self.consumeToken(.Asterisk)) |_| {
            is_parent = true;
        }

        if (self.consumeToken(.Arrow)) |_| {
            is_mutable = true;
        } else if (self.token_tags[self.token_index] == .Period or self.token_tags[self.token_index] == .Pipe) {
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
                .range = .{
                    .start = self.offsetToLocation(start_of_slot),
                    .end = self.offsetToLocation(self.token_starts[self.token_index]),
                },
            };
        } else if (self.consumeToken(.Equals)) |_| {} else {
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.offsetToLocation(self.token_starts[self.token_index]),
                "Expected arrow or equals after slot name, got {s}",
                .{self.token_tags[self.token_index].symbol()},
            );
            return null;
        }
    }

    var value = switch (method_mode) {
        .Required => blk: {
            // NOTE: Just for fun, turn this into:
            //           break :blk AST.ExpressionNode{ ...
            //       and watch the Zig compiler crash and burn.
            const expression_node = AST.ExpressionNode{
                .Object = (try self.parseObject(null, arguments.items)) orelse return null,
            };
            break :blk expression_node;
        },
        .Optional => blk: {
            if (self.token_tags[self.token_index] == .ParenOpen) {
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
                var expr = (try self.parseSlotsObjectOrSubexpr(false, &did_extract_expr, arguments.items)) orelse return null;
                errdefer expr.deinit(self.allocator);
                if (expr == .Object and expr.Object.statements.value.statements.len > 0) {
                    // This is a method, we cannot put expressions after it.
                    break :blk expr;
                }

                if (self.token_tags[self.token_index] == .Period or self.token_tags[self.token_index] == .Pipe) {
                    // This is a method, but it looks like a sub-expression. If it was extracted
                    // into an expression, then re-pack it, and then return it from this block.
                    // FIXME: This is very ugly.
                    if (did_extract_expr) {
                        var statement_slice = try self.allocator.alloc(AST.ExpressionNode, 1);
                        errdefer self.allocator.free(statement_slice);
                        statement_slice[0] = expr;

                        var statement_list = try AST.StatementList.create(self.allocator, statement_slice);
                        errdefer statement_list.unrefWithAllocator(self.allocator);

                        var object_node = try self.allocator.create(AST.ObjectNode);
                        object_node.* = .{
                            .slots = &.{},
                            .statements = statement_list,
                            .range = expr.range(),
                        };
                        expr = AST.ExpressionNode{ .Object = object_node };
                    }

                    break :blk expr;
                }

                break :blk (try self.parseExpressionFromPrimary(expr, .Any, false)) orelse return null;
            }

            break :blk (try self.parseExpression(.Any)) orelse return null;
        },
        .Forbidden => (try self.parseExpression(.Any)) orelse return null,
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
        .range = .{ .start = self.offsetToLocation(start_of_slot), .end = value.range().end },
    };
}

/// Creates an argument slot node from the current identifier.
fn createSlotNodeFromArgument(self: *Self, order: usize) ParseError!AST.SlotNode {
    const identifier_token = self.assertToken(.Identifier);
    var identifier_copy = try self.getIdentifierCopy(identifier_token);
    errdefer self.allocator.free(identifier_copy);

    const start_of_identifier = self.token_starts[self.token_index];
    const end_of_identifier = start_of_identifier + identifier_copy.len;

    const slot_node = AST.SlotNode{
        .is_mutable = true,
        .is_parent = false,
        .is_argument = true,
        .is_inherited = false,
        .order = order,
        .name = identifier_copy,
        .value = null,
        .range = .{
            .start = self.offsetToLocation(start_of_identifier),
            .end = self.offsetToLocation(end_of_identifier),
        },
    };
    return slot_node;
}

fn parseSlotName(self: *Self, arguments: *std.ArrayList(AST.SlotNode), method_mode: *MethodMode) ParseError!?[]const u8 {
    var order: usize = 0;

    if (self.consumeToken(.Identifier)) |identifier_token| {
        // Identifier means this is a unary message, no need to do anymore work.
        method_mode.* = .Optional;
        return try self.getIdentifierCopy(identifier_token);
    }

    if (self.consumeToken(.FirstKeyword)) |first_keyword_token| {
        method_mode.* = .Required;

        const first_keyword_slice = self.getKeywordSlice(first_keyword_token);
        var slot_name = try std.ArrayList(u8).initCapacity(self.allocator, first_keyword_slice.len);
        defer slot_name.deinit();
        slot_name.appendSliceAssumeCapacity(first_keyword_slice);

        if (self.token_tags[self.token_index] != .Identifier) {
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.offsetToLocation(self.token_starts[self.token_index]),
                "Expected identifier after keyword in slot name, got {s}",
                .{self.token_tags[self.token_index].symbol()},
            );
            return null;
        }

        try arguments.append(try self.createSlotNodeFromArgument(order));
        order += 1;

        while (self.consumeToken(.RestKeyword)) |rest_keyword_token| {
            const keyword_slice = self.getKeywordSlice(rest_keyword_token);
            try slot_name.appendSlice(keyword_slice);

            if (self.token_tags[self.token_index] != .Identifier) {
                try self.diagnostics.reportDiagnosticFormatted(
                    .Error,
                    self.offsetToLocation(self.token_starts[self.token_index]),
                    "Expected identifier after keyword in slot name, got {s}",
                    .{self.token_tags[self.token_index].symbol()},
                );
                return null;
            }

            try arguments.append(try self.createSlotNodeFromArgument(order));
            order += 1;
        }

        return slot_name.toOwnedSlice();
    }

    if (!self.token_tags[self.token_index].isOperator()) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.offsetToLocation(self.token_starts[self.token_index]),
            "Expected slot name, got {s}",
            .{self.token_tags[self.token_index].symbol()},
        );
        return null;
    }

    method_mode.* = .Required;
    // FIXME: De-magic this number
    var binary_message_name = std.BoundedArray(u8, 128).init(0) catch unreachable;

    // FIXME: Disallow space between operator tokens.
    while (self.token_tags[self.token_index].isOperator()) {
        binary_message_name.appendSlice(self.token_tags[self.token_index].symbol()) catch {
            try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(self.token_starts[self.token_index]), "Maximum binary operator length exceeded");
            return null;
        };

        _ = self.nextToken();
    }

    if (self.token_tags[self.token_index] != .Identifier) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.offsetToLocation(self.token_starts[self.token_index]),
            "Expected identifier after keyword in slot name, got {s}",
            .{self.token_tags[self.token_index].symbol()},
        );
        return null;
    }

    try arguments.append(try self.createSlotNodeFromArgument(order));
    return try self.allocator.dupe(u8, binary_message_name.constSlice());
}

const StringParseState = enum { Start, Literal, Backslash };
fn parseString(self: *Self) ParseError!?AST.StringNode {
    const string_token = self.assertToken(.String);
    const start_of_string = self.token_starts[string_token];

    var offset = self.token_starts[string_token];
    var state = StringParseState.Start;
    var string_buffer = std.ArrayList(u8).init(self.allocator);
    defer string_buffer.deinit();

    while (offset < self.buffer.len) : (offset += 1) {
        var c = self.buffer[offset];
        switch (state) {
            .Start => switch (c) {
                '\'' => {
                    state = .Literal;
                },
                else => unreachable,
            },
            .Literal => switch (c) {
                '\'' => {
                    return AST.StringNode{
                        .value = string_buffer.toOwnedSlice(),
                        .range = .{
                            .start = self.offsetToLocation(start_of_string),
                            .end = self.offsetToLocation(offset + 1),
                        },
                    };
                },
                '\n' => {
                    try self.diagnostics.reportDiagnostic(
                        .Error,
                        self.offsetToLocation(start_of_string),
                        "Unexpected newline in string literal",
                    );
                    return null;
                },
                '\\' => {
                    state = .Backslash;
                },
                else => {
                    try string_buffer.append(c);
                },
            },
            .Backslash => switch (c) {
                'n' => {
                    try string_buffer.append('\n');
                    state = .Literal;
                },
                'r' => {
                    try string_buffer.append('\r');
                    state = .Literal;
                },
                't' => {
                    try string_buffer.append('\t');
                    state = .Literal;
                },
                '\\' => {
                    try string_buffer.append('\\');
                    state = .Literal;
                },
                '\'' => {
                    try string_buffer.append('\'');
                    state = .Literal;
                },
                'x' => {
                    // At this point the tokenizer has validated that this
                    // character is followed by two valid hex digits.
                    var char: u8 = 0;

                    offset += 1;
                    c = self.buffer[offset];
                    if (std.ascii.isDigit(c))
                        char |= (c - '0') << 4
                    else
                        char |= 10 + (if (c >= 'a') c - 'a' else c - 'A') << 4;

                    offset += 1;
                    c = self.buffer[offset];
                    if (std.ascii.isDigit(c))
                        char |= (c - '0')
                    else
                        char |= 10 + (if (c >= 'a') c - 'a' else c - 'A');

                    try string_buffer.append(char);
                    state = .Literal;
                },
                else => unreachable,
            },
        }
    }

    try self.diagnostics.reportDiagnostic(
        .Error,
        self.offsetToLocation(start_of_string),
        "Unterminated string literal",
    );
    return null;
}

fn parseIdentifier(self: *Self) ParseError!AST.IdentifierNode {
    const identifier_token = self.assertToken(.Identifier);
    var identifier_copy = try self.getIdentifierCopy(identifier_token);
    errdefer self.allocator.free(identifier_copy);

    const start_of_identifier = self.token_starts[identifier_token];
    const end_of_identifier = start_of_identifier + identifier_copy.len;

    const node = AST.IdentifierNode{
        .value = identifier_copy,
        .range = .{ .start = self.offsetToLocation(start_of_identifier), .end = self.offsetToLocation(end_of_identifier) },
    };
    return node;
}

const IntegerParseState = enum { Start, Zero, Decimal, Hexadecimal, Octal };
fn parseInteger(self: *Self) ParseError!AST.NumberNode {
    const number_token = self.assertToken(.Integer);
    const start_of_number = self.token_starts[number_token];

    var integer: i62 = 0;
    var state = IntegerParseState.Start;
    var offset = start_of_number;
    while (offset < self.buffer.len) : (offset += 1) {
        const c = self.buffer[offset];
        switch (state) {
            .Start => switch (c) {
                '0' => {
                    state = .Zero;
                },
                '1'...'9' => {
                    integer = c - '0';
                    state = .Decimal;
                },
                else => unreachable,
            },
            .Zero => switch (c) {
                'x', 'X' => {
                    state = .Hexadecimal;
                },
                'o', 'O' => {
                    state = .Octal;
                },
                '0'...'9' => unreachable,
                else => break,
            },
            .Decimal => switch (c) {
                '0'...'9' => {
                    const digit = c - '0';

                    if (@mulWithOverflow(i62, integer, 10, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }

                    if (@addWithOverflow(i62, integer, digit, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }
                },
                else => break,
            },
            .Hexadecimal => switch (c) {
                '0'...'9' => {
                    const digit = c - '0';

                    if (@mulWithOverflow(i62, integer, 10, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }

                    if (@addWithOverflow(i62, integer, digit, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }
                },
                'A'...'F', 'a'...'f' => {
                    const digit = 10 + (if (c >= 'a') c - 'a' else c - 'A');

                    if (@mulWithOverflow(i62, integer, 16, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }

                    if (@addWithOverflow(i62, integer, digit, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }
                },
                else => break,
            },
            .Octal => switch (c) {
                '0'...'7' => {
                    const digit = c - '0';

                    if (@mulWithOverflow(i62, integer, 8, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }

                    if (@addWithOverflow(i62, integer, digit, &integer)) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Value does not fit in 62-bit integer");
                        break;
                    }
                },
                '8', '9' => unreachable,
                else => break,
            },
        }
    }

    const node = AST.NumberNode{
        .value = .{ .Integer = integer },
        .range = .{ .start = self.offsetToLocation(start_of_number), .end = self.offsetToLocation(offset) },
    };
    return node;
}

const FloatingPointParseState = enum { Integer, Fraction };
fn parseFloatingPoint(self: *Self) ParseError!AST.NumberNode {
    const number_token = self.assertToken(.FloatingPoint);
    const start_of_number = self.token_starts[number_token];

    var result: f64 = 0.0;
    var fraction: i64 = 0;
    var fraction_counter: usize = 0;
    var state = FloatingPointParseState.Integer;
    var offset = start_of_number;
    while (offset < self.buffer.len) : (offset += 1) {
        const c = self.buffer[offset];
        switch (state) {
            .Integer => switch (c) {
                '0'...'9' => {
                    result = (result * 10) + @intToFloat(f64, c - '0');
                },
                '.' => {
                    state = .Fraction;
                },
                else => unreachable,
            },
            .Fraction => switch (c) {
                '0'...'9' => {
                    if (fraction_counter == 9) {
                        try self.diagnostics.reportDiagnostic(.Error, self.offsetToLocation(start_of_number), "Floating point fraction too large");
                        break;
                    }
                    const digit = c - '0';

                    fraction = (fraction * 10) + digit;
                    fraction_counter += 1;
                },
                else => break,
            },
        }
    }

    var divisor = std.math.pow(f64, 10.0, @intToFloat(f64, fraction_counter));
    result += @intToFloat(f64, fraction) / divisor;

    const node = AST.NumberNode{
        .value = .{ .FloatingPoint = result },
        .range = .{ .start = self.offsetToLocation(start_of_number), .end = self.offsetToLocation(offset) },
    };
    return node;
}

fn nextToken(self: *Self) usize {
    const result = self.token_index;
    self.token_index += 1;
    return result;
}

fn consumeToken(self: *Self, tag: Token.Tag) ?usize {
    if (self.token_tags[self.token_index] == tag)
        return self.nextToken();
    return null;
}

fn assertToken(self: *Self, tag: Token.Tag) usize {
    const result = self.consumeToken(tag);
    if (result) |r| return r;
    unreachable;
}

fn expectToken(self: *Self, tag: Token.Tag) ParseError!bool {
    const current_tag = self.token_tags[self.token_index];
    const current_start = self.token_starts[self.token_index];

    if (current_tag != tag) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.offsetToLocation(current_start),
            "Expected '{s}', got '{s}'",
            .{ tag.symbol(), current_tag.symbol() },
        );
        return false;
    }

    _ = self.nextToken();
    return true;
}

/// Turn an offset into a line-and-column location.
fn offsetToLocation(self: Self, offset: usize) Location {
    var line: usize = 1;
    var line_start: usize = undefined;
    var line_end: usize = undefined;
    var did_find_line = false;

    std.debug.assert(offset <= self.buffer.len);

    while (line < self.line_offsets.items.len) : (line += 1) {
        const line_offset = self.line_offsets.items[line];
        if (offset < line_offset) {
            line_start = self.line_offsets.items[line - 1];
            line_end = line_offset - 1; // Get rid of newline
            did_find_line = true;
            break;
        }
    }

    if (!did_find_line) {
        // We are on the very last line.
        line_start = self.line_offsets.items[line - 1];
        line_end = self.buffer.len;
    }

    return Location{
        .line = line,
        .column = offset - line_start + 1,
        .line_start = line_start,
        .line_end = line_end,
    };
}

fn getKeywordSlice(self: Self, index: usize) []const u8 {
    const start = self.token_starts[index];
    const next_start = self.token_starts[index + 1];
    var end = next_start;

    while (self.buffer[end] != ':') : (end -= 1) {}
    return self.buffer[start .. end + 1];
}

fn getIdentifierSlice(self: Self, index: usize) []const u8 {
    const start = self.token_starts[index];
    const next_start = self.token_starts[index + 1];
    var end = next_start - 1;

    while (true) {
        switch (self.buffer[end]) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => break,
            else => end -= 1,
        }
    }
    return self.buffer[start .. end + 1];
}

fn getIdentifierCopy(self: Self, index: usize) Allocator.Error![]const u8 {
    const slice = self.getIdentifierSlice(index);
    return try self.allocator.dupe(u8, slice);
}
