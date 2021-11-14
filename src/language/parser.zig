// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Location = @import("./location.zig");
const Lexer = @import("./lexer.zig");
const AST = @import("./ast.zig");
const tokens = @import("./tokens.zig");
const Diagnostics = @import("./diagnostics.zig");

const Self = @This();

const SlotName = struct {
    name: []u8,
    arguments: [][]u8,

    pub fn deinit(self: *SlotName, allocator: *std.mem.Allocator) void {
        allocator.free(self.name);

        for (self.arguments) |argument| {
            allocator.free(argument);
        }
        allocator.free(self.arguments);
    }
};

fn errorSetOf(comptime Fn: anytype) type {
    return @typeInfo(@typeInfo(@TypeOf(Fn)).Fn.return_type.?).ErrorUnion.error_set;
}

// TODO: When the Zig compiler stops entering an infinite loop because of error
//       sets in recursive functions, remove this.
const ParserFunctionErrorSet = (errorSetOf(Lexer.nextToken));

// TODO: When Zig's RLS actually starts working properly, make this a static
//       function.
pub fn initInPlaceFromFilePath(self: *Self, file_path: [*:0]const u8, allocator: *std.mem.Allocator) !void {
    if (self.initialized)
        @panic("Attempting to initialize already-initialized parser");

    self.lexer = Lexer{};
    try self.lexer.initInPlaceFromFilePath(file_path, allocator);

    self.diagnostics = try Diagnostics.init(allocator);

    self.allocator = allocator;
    self.initialized = true;
}

pub fn deinit(self: *Self) void {
    if (!self.initialized)
        @panic("Attempting to deinitialize uninitialized parser");

    self.lexer.deinit();
    self.diagnostics.deinit();
}

pub fn parse(self: *Self) !AST.ScriptNode {
    if (!self.initialized)
        @panic("Attempting to call Parser.parse on uninitialized parser");

    _ = try self.lexer.nextToken();

    var statements = std.ArrayList(AST.StatementNode).init(self.allocator);
    defer {
        for (statements.items) |*statement| {
            statement.deinit(self.allocator);
        }
        statements.deinit();
    }

    while (self.lexer.current_token != .EOF) {
        if (try self.parseStatement(.EOF)) |*statement| {
            errdefer statement.deinit(self.allocator);
            try statements.append(statement.*);
        }
    }

    return AST.ScriptNode{ .statements = statements.toOwnedSlice() };
}

const ExpectTokenAction = enum { Consume, DontConsume };

fn expectToken(self: *Self, token_type: std.meta.Tag(tokens.Token), action: ExpectTokenAction) !bool {
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
        .Consume => _ = try self.lexer.nextToken(),
        .DontConsume => {},
    }

    return true;
}

fn parseStatement(self: *Self, alternative_terminator: std.meta.Tag(tokens.Token)) ParserFunctionErrorSet!?AST.StatementNode {
    if (try self.parseExpression()) |*expression| {
        if (self.lexer.current_token == .Period) {
            _ = try self.lexer.nextToken();
        } else if (self.lexer.current_token != alternative_terminator) {
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.lexer.token_start,
                "Expected period or '{s}' after expression, got '{s}'",
                .{ tokens.tokenTypeToString(alternative_terminator), self.lexer.current_token.toString() },
            );

            // Attempt to recover by consuming up to the next statement or end of scope
            while (self.lexer.current_token != .Period and self.lexer.current_token != alternative_terminator and self.lexer.current_token != .EOF) {
                _ = try self.lexer.nextToken();
            }

            if (self.lexer.current_token == .Period) {
                _ = try self.lexer.nextToken();
            }

            expression.deinit(self.allocator);
            return null;
        }

        return AST.StatementNode{ .expression = expression.* };
    } else {
        // Attempt to recover by consuming up to the next statement or end of scope
        while (self.lexer.current_token != .Period and self.lexer.current_token != alternative_terminator and self.lexer.current_token != .EOF) {
            _ = try self.lexer.nextToken();
        }

        if (self.lexer.current_token == .Period) {
            _ = try self.lexer.nextToken();
        }

        return null;
    }
}

fn parseExpression(self: *Self) ParserFunctionErrorSet!?AST.ExpressionNode {
    if (try self.parsePrimary()) |*primary| {
        errdefer primary.deinit(self.allocator);
        var receiver = primary.*;

        if (primary.* == .Identifier and self.lexer.current_token == .Colon) {
            return try self.parseKeywordMessageToSelf(primary.Identifier);
        } else if (self.lexer.current_token == .Identifier) {
            if (try self.parseMessageToReceiver(primary.*)) |message_expr| {
                receiver = message_expr;
            } else {
                return null;
            }
        }

        if (self.lexer.current_token.isOperator()) {
            if (try self.parseBinaryMessage(receiver)) |message_expr| {
                receiver = message_expr;
            } else {
                return null;
            }
        }

        return receiver;
    } else {
        return null;
    }
}

fn parsePrimary(self: *Self) ParserFunctionErrorSet!?AST.ExpressionNode {
    switch (self.lexer.current_token) {
        .ParenOpen => return try self.parseSlotsObjectOrSubExpression(),
        .BracketOpen => if (try self.parseBlock()) |block| return AST.ExpressionNode{ .Block = block },
        .Identifier => if (try self.parseIdentifier()) |identifier| return AST.ExpressionNode{ .Identifier = identifier },
        .Integer, .FloatingPoint => if (try self.parseNumber()) |number| return AST.ExpressionNode{ .Number = number },
        .String => if (try self.parseString()) |string| return AST.ExpressionNode{ .String = string },
        else => {
            // TODO: Recovery
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.lexer.token_start,
                "Got unknown token '{s}' while parsing primary expression",
                .{self.lexer.current_token.toString()},
            );
        },
    }

    return null;
}

fn parseSlotsObjectOrSubExpression(self: *Self) ParserFunctionErrorSet!?AST.ExpressionNode {
    if (try self.parseObject()) |object| {
        errdefer object.destroy(self.allocator);

        if (object.slots.len > 0) {
            if (object.statements.len == 0) {
                return AST.ExpressionNode{ .Object = object };
            } else {
                // TODO: Recovery
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Slot list cannot be present in a sub-expression");

                object.destroy(self.allocator);
                return null;
            }
        } else {
            if (object.statements.len == 1) {
                const statement = object.statements[0];

                // This is done so that the slice is freed but the expression isn't.
                object.statements = try self.allocator.resize(object.statements, 0);
                object.destroy(self.allocator);

                return statement.expression;
            } else {
                // TODO: Recovery
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Only one expression must be present in a sub-expression");

                object.destroy(self.allocator);
                return null;
            }
        }
    }

    return null;
}

fn parseObject(self: *Self) ParserFunctionErrorSet!?*AST.ObjectNode {
    if (!try self.expectToken(.ParenOpen, .Consume))
        return null;

    var slots = std.ArrayList(AST.SlotNode).init(self.allocator);
    defer {
        for (slots.items) |*slot| {
            slot.deinit(self.allocator);
        }
        slots.deinit();
    }

    var statements = std.ArrayList(AST.StatementNode).init(self.allocator);
    defer {
        for (statements.items) |*statement| {
            statement.deinit(self.allocator);
        }
        statements.deinit();
    }

    if (self.lexer.current_token == .Pipe) {
        _ = try self.lexer.nextToken();

        while (self.lexer.current_token != .Pipe) {
            if (try self.parseSlot(.Object)) |*slot| {
                errdefer slot.deinit(self.allocator);
                try slots.append(slot.*);

                if (self.lexer.current_token == .Period) {
                    _ = try self.lexer.nextToken();
                } else if (self.lexer.current_token != .Pipe) {
                    try self.diagnostics.reportDiagnosticFormatted(
                        .Error,
                        self.lexer.token_start,
                        "Expected '.' or '|' after slot, got '{s}'",
                        .{self.lexer.current_token.toString()},
                    );

                    // Attempt to recover by eating up to either the pipe or period
                    while (self.lexer.current_token != .Period and self.lexer.current_token != .Pipe) {
                        _ = try self.lexer.nextToken();
                    }

                    if (self.lexer.current_token == .Period) {
                        _ = try self.lexer.nextToken();
                    }
                }
            } else {
                // Attempt to recover by eating up to either the pipe or period
                while (self.lexer.current_token != .Period and self.lexer.current_token != .Pipe) {
                    _ = try self.lexer.nextToken();
                }
            }
        }

        _ = try self.lexer.nextToken();
    }

    while (self.lexer.current_token != .ParenClose) {
        // NOTE: parseStatement will have handled the "consuming until end of
        //       statement" part here, so we don't need to do it ourselves.
        if (try self.parseStatement(.ParenClose)) |*statement| {
            errdefer statement.deinit(self.allocator);
            try statements.append(statement.*);
        }
    }

    _ = try self.lexer.nextToken();

    const object_node = try self.allocator.create(AST.ObjectNode);
    object_node.slots = slots.toOwnedSlice();
    object_node.statements = statements.toOwnedSlice();

    return object_node;
}

fn parseBlock(self: *Self) ParserFunctionErrorSet!?*AST.BlockNode {
    if (!try self.expectToken(.BracketOpen, .Consume))
        return null;

    var slots = std.ArrayList(AST.SlotNode).init(self.allocator);
    defer {
        for (slots.items) |*slot| {
            slot.deinit(self.allocator);
        }
        slots.deinit();
    }

    var statements = std.ArrayList(AST.StatementNode).init(self.allocator);
    defer {
        for (statements.items) |*statement| {
            statement.deinit(self.allocator);
        }
        statements.deinit();
    }

    if (self.lexer.current_token == .Pipe) {
        _ = try self.lexer.nextToken();

        while (self.lexer.current_token != .Pipe) {
            if (try self.parseSlot(.Block)) |*slot| {
                errdefer slot.deinit(self.allocator);
                try slots.append(slot.*);

                if (self.lexer.current_token == .Period) {
                    _ = try self.lexer.nextToken();
                } else if (self.lexer.current_token != .Pipe) {
                    try self.diagnostics.reportDiagnosticFormatted(
                        .Error,
                        self.lexer.token_start,
                        "Expected '.' or '|' after slot, got '{s}'",
                        .{self.lexer.current_token.toString()},
                    );

                    // Attempt to recover by eating up to either the pipe or period
                    while (self.lexer.current_token != .Period and self.lexer.current_token != .Pipe) {
                        _ = try self.lexer.nextToken();
                    }

                    if (self.lexer.current_token == .Period) {
                        _ = try self.lexer.nextToken();
                    }
                }
            } else {
                // Attempt to recover by eating up to either the pipe or period
                while (self.lexer.current_token != .Period and self.lexer.current_token != .Pipe) {
                    _ = try self.lexer.nextToken();
                }
            }
        }

        _ = try self.lexer.nextToken();
    }

    var did_use_return = false;
    var return_use_location: Location = .{};
    while (self.lexer.current_token != .BracketClose) {
        if (did_use_return) {
            try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Unexpected expression after return expression in block");
            try self.diagnostics.reportDiagnostic(.Note, return_use_location, "Return expression used here");

            // Try to recover by eating up to bracket
            while (self.lexer.current_token != .BracketClose) {
                _ = try self.lexer.nextToken();
            }

            break;
        }

        if (self.lexer.current_token == .Cap) {
            did_use_return = true;
            return_use_location = self.lexer.token_start;

            _ = try self.lexer.nextToken();
        }

        // NOTE: parseStatement will have handled the "consuming until end of
        //       statement" part here, so we don't need to do it ourselves.
        if (try self.parseStatement(.BracketClose)) |*statement| {
            errdefer statement.deinit(self.allocator);

            if (did_use_return) {
                const return_node = try self.allocator.create(AST.ReturnNode);
                return_node.expression = statement.expression;
                errdefer return_node.destroy(self.allocator);

                try statements.append(AST.StatementNode{ .expression = AST.ExpressionNode{ .Return = return_node } });
            } else {
                try statements.append(statement.*);
            }
        }
    }

    _ = try self.lexer.nextToken();

    const block_node = try self.allocator.create(AST.BlockNode);
    block_node.slots = slots.toOwnedSlice();
    block_node.statements = statements.toOwnedSlice();

    return block_node;
}

const SlotParsingMode = enum { Object, Block };
fn parseSlot(self: *Self, parsing_mode: SlotParsingMode) ParserFunctionErrorSet!?AST.SlotNode {
    var is_mutable = true;
    var is_parent = false;
    var is_argument = false;

    if (self.lexer.current_token == .Colon) {
        if (parsing_mode == .Block) {
            is_argument = true;
            _ = try self.lexer.nextToken();
        } else {
            try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Object slots may not be arguments");
            return null;
        }
    }

    if (try self.parseSlotName(parsing_mode)) |*slot_name| {
        errdefer slot_name.deinit(self.allocator);

        if (self.lexer.current_token == .Asterisk) {
            if (parsing_mode == .Object) {
                is_parent = true;
                _ = try self.lexer.nextToken();
            } else {
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Block slots may not be parents");

                slot_name.deinit(self.allocator);
                return null;
            }
        }

        var value = value_parsing: {
            if (slot_name.arguments.len > 0) {
                // If we get here then we *MUST* be in Object slot parsing mode,
                // because only Objects can have slots with keywords.
                std.debug.assert(parsing_mode == .Object);

                if (!try self.expectToken(.Equals, .Consume)) {
                    slot_name.deinit(self.allocator);
                    return null;
                }
                is_mutable = false;

                if (self.lexer.current_token != .ParenOpen) {
                    try self.diagnostics.reportDiagnosticFormatted(
                        .Error,
                        self.lexer.token_start,
                        "Expected object after slot with keywords, got '{s}'",
                        .{self.lexer.current_token.toString()},
                    );

                    slot_name.deinit(self.allocator);
                    return null;
                }

                if (try self.parseObject()) |object| {
                    break :value_parsing AST.ExpressionNode{ .Object = object };
                } else {
                    slot_name.deinit(self.allocator);
                    return null;
                }
            } else {
                if (is_argument) {
                    // If this is an argument slot, we don't allow the assignment of
                    // any sort of value.
                    if (!(self.lexer.current_token == .Pipe or self.lexer.current_token == .Period)) {
                        try self.diagnostics.reportDiagnosticFormatted(
                            .Error,
                            self.lexer.token_start,
                            "Expected '|' or '.' after argument slot, got '{s}'",
                            .{self.lexer.current_token.toString()},
                        );

                        slot_name.deinit(self.allocator);
                        return null;
                    }
                } else {
                    if (self.lexer.current_token == .Arrow or self.lexer.current_token == .Equals) {
                        is_mutable = self.lexer.current_token == .Arrow;
                        _ = try self.lexer.nextToken();

                        // NOTE: We need to override ParenOpen here because
                        //       parseExpression only parses sub-expressions or
                        //       slots objects.
                        switch (self.lexer.current_token) {
                            .ParenOpen => if (try self.parseObject()) |object| break :value_parsing AST.ExpressionNode{ .Object = object },
                            else => if (try self.parseExpression()) |expression| break :value_parsing expression,
                        }

                        // If we got here, then parsing either of them failed.
                        slot_name.deinit(self.allocator);
                        return null;
                    } else if (!(self.lexer.current_token == .Period or self.lexer.current_token == .Pipe)) {
                        try self.diagnostics.reportDiagnosticFormatted(
                            .Error,
                            self.lexer.token_start,
                            "Expected '.', '|', '<-' or '=' after slot name, got '{s}'",
                            .{self.lexer.current_token.toString()},
                        );

                        slot_name.deinit(self.allocator);
                        return null;
                    }
                }

                // TODO: Intern these
                const nil_identifier = try self.allocator.dupe(u8, "nil");
                break :value_parsing AST.ExpressionNode{ .Identifier = AST.IdentifierNode{ .value = nil_identifier } };
            }
        };

        return AST.SlotNode{
            .is_mutable = is_mutable,
            .is_parent = is_parent,
            .is_argument = is_argument,
            .name = slot_name.name,
            .arguments = slot_name.arguments,
            .value = value,
        };
    }

    return null;
}

fn parseSlotName(self: *Self, parsing_mode: SlotParsingMode) ParserFunctionErrorSet!?SlotName {
    var name = std.ArrayList(u8).init(self.allocator);
    defer name.deinit();

    var arguments = std.ArrayList([]u8).init(self.allocator);
    defer {
        for (arguments.items) |argument| {
            self.allocator.free(argument);
        }
        arguments.deinit();
    }

    if (self.lexer.current_token == .Identifier) {
        var is_parsing_keyword_message = false;
        while (self.lexer.current_token == .Identifier) {
            const keyword_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];

            // Keyword messages have their casing as: foo: param Bar: param Baz: param
            if (is_parsing_keyword_message and !std.ascii.isUpper(keyword_slice[0])) {
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Keyword argument must have uppercase prefix");
                return null;
            }

            try name.appendSlice(keyword_slice);

            if ((try self.lexer.nextToken()).* == .Colon) {
                if (parsing_mode == .Block) {
                    try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Blocks cannot have keyword slots");
                    return null;
                } else {
                    is_parsing_keyword_message = true;
                }
            } else if (is_parsing_keyword_message) {
                // If we're parsing a keyword message and there's no colon after a keyword,
                // that's an error.
                try self.diagnostics.reportDiagnosticFormatted(
                    .Error,
                    self.lexer.token_start,
                    "Expected ':' after slot keyword, got '{s}'",
                    .{self.lexer.current_token.toString()},
                );
                return null;
            } else {
                // If we weren't parsing a keyword message, then this is just a unary message.
                break;
            }

            try name.append(':');

            if ((try self.lexer.nextToken()).* != .Identifier) {
                try self.diagnostics.reportDiagnosticFormatted(
                    .Error,
                    self.lexer.token_start,
                    "Expected parameter name after slot keyword, got '{s}'",
                    .{self.lexer.current_token.toString()},
                );
                return null;
            }

            const argument_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];
            const argument_copy = try self.allocator.dupe(u8, argument_slice);

            {
                errdefer self.allocator.free(argument_copy);
                try arguments.append(argument_copy);
            }

            _ = try self.lexer.nextToken();
        }
    } else if (self.lexer.current_token.isOperator()) {
        if (parsing_mode != .Object) {
            try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Binary slots are only allowed on objects");
            return null;
        }

        var tokens_consumed: usize = 0;
        while (self.lexer.current_token.isOperator()) {
            if (tokens_consumed > 0 and self.lexer.consumed_whitespace > 0) {
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Unexpected whitespace between binary message tokens");
                return null;
            }

            try name.appendSlice(self.lexer.current_token.toString());
            _ = try self.lexer.nextToken();
            tokens_consumed += 1;
        }

        if (!try self.expectToken(.Identifier, .DontConsume))
            return null;

        const argument_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];
        const argument_copy = try self.allocator.dupe(u8, argument_slice);

        {
            errdefer self.allocator.free(argument_copy);
            try arguments.append(argument_copy);
        }

        _ = try self.lexer.nextToken();
    } else {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.lexer.token_start,
            "Expected operator character or identifier for slot name, got '{s}'",
            .{self.lexer.current_token.toString()},
        );
        return null;
    }

    return SlotName{ .name = name.toOwnedSlice(), .arguments = arguments.toOwnedSlice() };
}

fn parseKeywordMessageToSelf(self: *Self, identifier: AST.IdentifierNode) ParserFunctionErrorSet!?AST.ExpressionNode {
    // Arguments aren't mutable in Zig.
    var identifier_mut = identifier;

    if (!try self.expectToken(.Colon, .DontConsume)) {
        identifier_mut.deinit(self.allocator);
        return null;
    }

    // TODO: Intern these
    const self_identifier = try self.allocator.dupe(u8, "self");
    // NOTE: We transfer the ownership to parseMessageCommon at call time, so it
    //       will clean up the receiver and identifier on failure.

    const receiver = AST.ExpressionNode{ .Identifier = AST.IdentifierNode{ .value = self_identifier } };
    return try self.parseMessageCommon(receiver, identifier);
}

fn parseMessageToReceiver(self: *Self, receiver: AST.ExpressionNode) ParserFunctionErrorSet!?AST.ExpressionNode {
    // Arguments aren't mutable in Zig.
    var receiver_mut = receiver;

    if (!try self.expectToken(.Identifier, .DontConsume)) {
        receiver_mut.deinit(self.allocator);
        return null;
    }

    const identifier_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];

    // Determine whether this identifier can be a message. Normally
    // parseMessageCommon would do this but we get rid of the identifier below
    // by doing a nextToken, so we won't be able to recover the parser state at
    // that point.
    if (std.ascii.isUpper(identifier_slice[0])) {
        std.debug.print("parseMessageToReceiver: This message is part of an outer expression, giving up\n", .{});
        // Nope, can't use it.
        return receiver;
    }

    const identifier_copy = try self.allocator.dupe(u8, identifier_slice);
    // NOTE: We transfer the ownership to parseMessageCommon at call time, so it
    //       will clean up the receiver and identifier on failure.

    {
        errdefer self.allocator.free(identifier_copy);
        _ = try self.lexer.nextToken();
    }

    const identifier = AST.IdentifierNode{ .value = identifier_copy };
    return try self.parseMessageCommon(receiver, identifier);
}

const MessageParsingState = enum { Unary, Keyword };
fn parseMessageCommon(
    self: *Self,
    receiver: AST.ExpressionNode,
    first_identifier: AST.IdentifierNode,
) ParserFunctionErrorSet!?AST.ExpressionNode {
    // At this part of message parsing, we're going to be at one of these two
    // points:
    // 1) It was a keyword message. We are at a Colon token, and will start
    //    parsing a keyword message from the get-go.
    // 2) It was a unary message, possibly followed up by more unary messages,
    //    maybe ending in a keyword message. We are at an Identifier token, and
    //    will reduce it to a new expression.

    // Arguments aren't mutable in Zig.
    var first_identifier_mut = first_identifier;

    var parsing_state = MessageParsingState.Unary;

    var current_receiver = receiver;
    errdefer current_receiver.deinit(self.allocator);

    var current_message_name = std.ArrayList(u8).init(self.allocator);
    defer current_message_name.deinit();

    var parameters = std.ArrayList(AST.ExpressionNode).init(self.allocator);
    defer {
        for (parameters.items) |*parameter| {
            parameter.deinit(self.allocator);
        }
        parameters.deinit();
    }

    // Used when passing an identifier string from unary to keyword parsing state.
    var unary_to_keyword_identifier: []const u8 = undefined;
    // Used to tell whether the Keyword state should use unary-to-keyword
    // passover or the first identifier as the initial keyword, because we can
    // have two ways of entering the Keyword state.
    var should_use_passover_as_first_identifier = false;

    // If the next token is not a Colon, then we want to immediately reduce
    // the first identifier into the receiver; this handles the case of
    // chained unary messages.
    if (self.lexer.current_token != .Colon) {
        errdefer first_identifier_mut.deinit(self.allocator);

        var message_node = try self.allocator.create(AST.MessageNode);
        message_node.receiver = current_receiver;
        // NOTE: We now take ownership of the first identifier's string
        message_node.message_name = first_identifier.value;
        message_node.arguments = try self.allocator.alloc(AST.ExpressionNode, 0);
        current_receiver = AST.ExpressionNode{ .Message = message_node };

        should_use_passover_as_first_identifier = true;
    } else {
        // If it *is* a colon, we want to switch to Keyword mode immediately.
        parsing_state = .Keyword;
    }

    state_machine: while (true) {
        switch (parsing_state) {
            .Unary => {
                if (self.lexer.current_token == .Identifier) {
                    const identifier_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];
                    if (std.ascii.isUpper(identifier_slice[0])) {
                        // We are parsing a unary message and the message name
                        // starts with an uppercase?! No, this is part of an
                        // outer expression, probably a keyword message this
                        // unary message expression is an argument to. Give up
                        // parsing.
                        break :state_machine;
                    }

                    const identifier_copy = try self.allocator.dupe(u8, identifier_slice);
                    errdefer self.allocator.free(identifier_copy);

                    unary_to_keyword_identifier = identifier_copy;
                    // Look ahead once to see whether we should be collapsing
                    // this into a new message or we should be letting keyword
                    // state handle this from now on.
                    if ((try self.lexer.nextToken()).* == .Colon) {
                        // Yep, it's a keyword. Switch over.
                        parsing_state = .Keyword;
                        continue :state_machine;
                    }

                    // No, it's not. Collapse this identifier into an unary
                    // message and let the state machine handle the rest.
                    var message_node = try self.allocator.create(AST.MessageNode);
                    message_node.receiver = current_receiver;
                    message_node.message_name = identifier_copy;
                    message_node.arguments = try self.allocator.alloc(AST.ExpressionNode, 0);
                    current_receiver = AST.ExpressionNode{ .Message = message_node };
                } else {
                    // We let the statement parser throw any errors related
                    // to unexpected tokens at the end of an expression.
                    break :state_machine;
                }
            },
            .Keyword => {
                if (parameters.items.len == 0) {
                    // We have not parsed anything keyword-related yet, so let's
                    // figure out what the first keyword is now.
                    if (should_use_passover_as_first_identifier) {
                        // We were parsing unary messages before we got here,
                        // so use the passover value.
                        defer self.allocator.free(unary_to_keyword_identifier);

                        // Sanity check
                        if (std.ascii.isUpper(unary_to_keyword_identifier[0]))
                            @panic("Got an uppercase keyword as first keyword argument from unary state?!");

                        if (!try self.expectToken(.Colon, .Consume)) {
                            current_receiver.deinit(self.allocator);
                            return null;
                        }

                        try current_message_name.appendSlice(unary_to_keyword_identifier);
                    } else {
                        // Unary never triggered so we should be using the first
                        // identifier.
                        defer first_identifier_mut.deinit(self.allocator);

                        // The same sanity check can't be repeated here because
                        // first_identifier is given by the expression parser
                        // who has no idea about keyword rules.
                        if (std.ascii.isUpper(first_identifier.value[0])) {
                            break :state_machine;
                        }

                        if (!try self.expectToken(.Colon, .Consume)) {
                            current_receiver.deinit(self.allocator);
                            return null;
                        }

                        try current_message_name.appendSlice(first_identifier.value);
                    }
                    try current_message_name.append(':');

                    if (try self.parseExpression()) |*expression| {
                        errdefer expression.deinit(self.allocator);
                        try parameters.append(expression.*);
                    } else {
                        current_receiver.deinit(self.allocator);
                        return null;
                    }
                }

                // Okay, NOW we can resume regular parsing proper.

                if (self.lexer.current_token == .Identifier) {
                    const identifier_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];

                    // We are at the start of a new keyword. Verify that it is
                    // part of this message.
                    if (!std.ascii.isUpper(identifier_slice[0])) {
                        break :state_machine;
                    }

                    try current_message_name.appendSlice(identifier_slice);
                    _ = try self.lexer.nextToken();

                    if (!try self.expectToken(.Colon, .Consume)) {
                        current_receiver.deinit(self.allocator);
                        return null;
                    }

                    try current_message_name.append(':');

                    if (try self.parseExpression()) |*expression| {
                        errdefer expression.deinit(self.allocator);
                        try parameters.append(expression.*);
                    } else {
                        current_receiver.deinit(self.allocator);
                        return null;
                    }
                } else {
                    break :state_machine;
                }
            },
        }
    }

    // If we're in keyword mode, let's compile the collected slot name and
    // variables into a message expression.
    switch (parsing_state) {
        .Unary => return current_receiver,
        .Keyword => {
            var message_node = try self.allocator.create(AST.MessageNode);
            message_node.receiver = current_receiver;
            // NOTE: We now take ownership of the first identifier's string
            message_node.message_name = current_message_name.toOwnedSlice();
            message_node.arguments = parameters.toOwnedSlice();

            return AST.ExpressionNode{ .Message = message_node };
        },
    }
}

fn parseBinaryMessage(self: *Self, receiver: AST.ExpressionNode) ParserFunctionErrorSet!?AST.ExpressionNode {
    // Arguments aren't mutable in Zig.
    var receiver_mut = receiver;
    errdefer receiver_mut.deinit(self.allocator);

    var message_name = std.ArrayList(u8).init(self.allocator);
    defer message_name.deinit();

    if (!self.lexer.current_token.isOperator()) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.lexer.token_start,
            "Expected an operator token for binary message, got '{s}'",
            .{self.lexer.current_token.toString()},
        );

        receiver_mut.deinit(self.allocator);
        return null;
    }

    var tokens_consumed: usize = 0;
    while (self.lexer.current_token.isOperator()) {
        if (tokens_consumed > 0 and self.lexer.consumed_whitespace > 0) {
            try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Unexpected whitespace between binary message tokens");
            return null;
        }

        try message_name.appendSlice(self.lexer.current_token.toString());
        _ = try self.lexer.nextToken();
        tokens_consumed += 1;
    }

    if (try self.parseExpression()) |*expression| {
        errdefer expression.deinit(self.allocator);
        // TODO: Disallow mixing different binary messages without parenthesis,
        //       as it looks ambiguous. Self doesn't define any precedence for
        //       operators, so they would always be processed right-to-left.
        //
        //       Not having any precedence doesn't really sound that good either,
        //       though. Could we do better here? Maybe objects that are the same
        //       "type" (i.e. a binary message resolves to identical method
        //       activations on both objects) can have a type precedence within?
        //       Though that wouldn't work for custom types. I wouldn't like
        //       it to be world-dependent either, that would easily break across
        //       worlds. Need to put more thought here.

        const message_node = try self.allocator.create(AST.MessageNode);
        message_node.message_name = message_name.toOwnedSlice();
        message_node.arguments = try self.allocator.alloc(AST.ExpressionNode, 1);
        message_node.arguments[0] = expression.*;
        message_node.receiver = receiver;
        return AST.ExpressionNode{ .Message = message_node };
    } else {
        receiver_mut.deinit(self.allocator);
        return null;
    }
}

fn parseIdentifier(self: *Self) ParserFunctionErrorSet!?AST.IdentifierNode {
    if (!try self.expectToken(.Identifier, .DontConsume))
        return null;

    const identifier_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];
    const identifier_copy = try self.allocator.dupe(u8, identifier_slice);
    errdefer self.allocator.free(identifier_copy);

    _ = try self.lexer.nextToken();
    return AST.IdentifierNode{ .value = identifier_copy };
}

fn parseNumber(self: *Self) ParserFunctionErrorSet!?AST.NumberNode {
    const value = switch (self.lexer.current_token) {
        .Integer => AST.NumberNode{ .Integer = self.lexer.current_token.Integer },
        .FloatingPoint => AST.NumberNode{ .FloatingPoint = self.lexer.current_token.FloatingPoint },
        else => {
            try self.diagnostics.reportDiagnosticFormatted(
                .Error,
                self.lexer.token_start,
                "Expected number value, got '{s}'",
                .{self.lexer.current_token.toString()},
            );
            return null;
        },
    };

    _ = try self.lexer.nextToken();
    return value;
}

fn parseString(self: *Self) ParserFunctionErrorSet!?AST.StringNode {
    if (!try self.expectToken(.String, .DontConsume))
        return null;

    const value = AST.StringNode{ .value = self.lexer.current_token.String };

    _ = try self.lexer.nextToken();
    return value;
}

initialized: bool = false,
allocator: *std.mem.Allocator = undefined,
lexer: Lexer = undefined,
diagnostics: Diagnostics = undefined,
