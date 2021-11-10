const std = @import("std");
const Lexer = @import("./lexer.zig");
const AST = @import("./ast.zig");
const tokens = @import("./tokens.zig");
const Diagnostics = @import("./diagnostics.zig");

const Self = @This();

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
    defer statements.deinit();

    while (self.lexer.current_token != .EOF) {
        if (try self.parseStatement(.EOF)) |statement| {
            try statements.append(statement);
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
    var expression = try self.parseExpression();
    if (expression == null) {
        // Attempt to recover by consuming up to the next statement or end of scope
        while (self.lexer.current_token != .Period and self.lexer.current_token != alternative_terminator and self.lexer.current_token != .EOF) {
            _ = try self.lexer.nextToken();
        }

        if (self.lexer.current_token == .Period) {
            _ = try self.lexer.nextToken();
        }

        return null;
    }

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

        expression.?.deinit(self.allocator);
        return null;
    }

    return AST.StatementNode{ .expression = expression.? };
}

fn parseExpression(self: *Self) ParserFunctionErrorSet!?AST.ExpressionNode {
    if (try self.parsePrimary()) |primary| {
        return primary;
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
        errdefer object.deinit(self.allocator);

        if (object.slots.len > 0) {
            if (object.statements.len == 0) {
                return AST.ExpressionNode{ .Object = object };
            } else {
                // TODO: Recovery
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Slot list cannot be present in a sub-expression");

                object.deinit(self.allocator);
                return null;
            }
        } else {
            if (object.statements.len == 1) {
                const statement = object.statements[0];

                // This is done so that the slice is freed but the expression isn't.
                object.statements = object.statements[0..0];
                object.deinit(self.allocator);

                return statement.expression;
            } else {
                // TODO: Recovery
                try self.diagnostics.reportDiagnostic(.Error, self.lexer.token_start, "Only one expression must be present in a sub-expression");

                object.deinit(self.allocator);
                return null;
            }
        }
    }

    return null;
}

// FIXME: Refactor this code into multiple functions.
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
            const did_parse_slot = slot_parsing: {
                if (try self.parseSlotName()) |*slot_name| {
                    {
                        errdefer slot_name.deinit(self.allocator);

                        var is_mutable = true;
                        var is_parent = false;

                        if (self.lexer.current_token == .Asterisk) {
                            is_parent = true;
                            _ = try self.lexer.nextToken();
                        }

                        const value = value_parsing: {
                            if (slot_name.arguments.len > 0) {
                                if (!try self.expectToken(.Equals, .Consume)) {
                                    slot_name.deinit(self.allocator);
                                    break :slot_parsing false;
                                }

                                if (self.lexer.current_token != .ParenOpen) {
                                    try self.diagnostics.reportDiagnosticFormatted(
                                        .Error,
                                        self.lexer.token_start,
                                        "Expected object after slot with keywords, got '{s}'",
                                        .{self.lexer.current_token.toString()},
                                    );

                                    slot_name.deinit(self.allocator);
                                    break :slot_parsing false;
                                }

                                if (try self.parseObject()) |object| {
                                    break :value_parsing AST.ExpressionNode{ .Object = object };
                                } else {
                                    break :slot_parsing false;
                                }
                            } else {
                                if (self.lexer.current_token == .Arrow or self.lexer.current_token == .Equals) {
                                    is_mutable = self.lexer.current_token == .Arrow;
                                    _ = try self.lexer.nextToken();

                                    // NOTE: We need to override ParenOpen here because
                                    //       parsePrimary only parses sub-expressions or
                                    //       slots objects.
                                    switch (self.lexer.current_token) {
                                        .ParenOpen => if (try self.parseObject()) |object| break :value_parsing AST.ExpressionNode{ .Object = object },
                                        else => if (try self.parsePrimary()) |primary| break :value_parsing primary,
                                    }

                                    // If we got here, then parsing either of them failed.
                                    slot_name.deinit(self.allocator);
                                    break :slot_parsing false;
                                } else if (!(self.lexer.current_token == .Period or self.lexer.current_token == .Pipe)) {
                                    try self.diagnostics.reportDiagnosticFormatted(
                                        .Error,
                                        self.lexer.token_start,
                                        "Expected '.', '|', '<-' or '=' after slot name, got '{s}'",
                                        .{self.lexer.current_token.toString()},
                                    );

                                    slot_name.deinit(self.allocator);
                                    break :slot_parsing false;
                                }

                                // TODO: Intern these
                                const nil_identifier = try self.allocator.dupe(u8, "nil");
                                break :value_parsing AST.ExpressionNode{ .Identifier = AST.IdentifierNode{ .value = nil_identifier } };
                            }
                        };

                        try slots.append(AST.SlotNode{
                            .is_mutable = is_mutable,
                            .is_parent = is_parent,
                            .is_argument = false,
                            .name = slot_name.name,
                            .arguments = slot_name.arguments,
                            .value = value,
                        });
                    }

                    _ = try self.lexer.nextToken();
                    break :slot_parsing true;
                }

                break :slot_parsing false;
            };

            if (!did_parse_slot) {
                // Attempt to recover by eating up to either the pipe or period
                while (self.lexer.current_token != .Period and self.lexer.current_token != .Pipe) {
                    _ = try self.lexer.nextToken();
                }
            }

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
        }
    }

    _ = try self.lexer.nextToken();

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

fn parseSlotName(self: *Self) ParserFunctionErrorSet!?SlotName {
    if (!try self.expectToken(.Identifier, .DontConsume))
        return null;

    var name = std.ArrayList(u8).init(self.allocator);
    defer name.deinit();
    var arguments = std.ArrayList([]u8).init(self.allocator);
    defer {
        for (arguments.items) |argument| {
            self.allocator.free(argument);
        }
        arguments.deinit();
    }

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
            is_parsing_keyword_message = true;
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

    return SlotName{ .name = name.toOwnedSlice(), .arguments = arguments.toOwnedSlice() };
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
            const did_parse_slot = slot_parsing: {
                var is_mutable = true;
                var is_argument = false;

                if (self.lexer.current_token == .Colon) {
                    is_argument = true;
                    _ = try self.lexer.nextToken();
                }

                if (!try self.expectToken(.Identifier, .DontConsume)) {
                    break :slot_parsing false;
                }

                const identifier_slice = self.lexer.current_token.Identifier[0..self.lexer.current_token.Identifier.len];

                {
                    const identifier_copy = try self.allocator.dupe(u8, identifier_slice);
                    errdefer self.allocator.free(identifier_copy);

                    const value = value_parsing: {
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

                                self.allocator.free(identifier_copy);
                                break :slot_parsing false;
                            }
                        } else {
                            if (self.lexer.current_token == .Equals or self.lexer.current_token == .Arrow) {
                                is_mutable = self.lexer.current_token == .Arrow;
                                _ = try self.lexer.nextToken();

                                // NOTE: We need to override ParenOpen here because
                                //       parsePrimary only parses sub-expressions or
                                //       slots objects.
                                switch (self.lexer.current_token) {
                                    .ParenOpen => if (try self.parseObject()) |object| break :value_parsing AST.ExpressionNode{ .Object = object },
                                    else => if (try self.parsePrimary()) |primary| break :value_parsing primary,
                                }

                                // If we got here, then parsing either of them failed.
                                self.allocator.free(identifier_copy);
                                break :slot_parsing false;
                            } else if (!(self.lexer.current_token == .Pipe or self.lexer.current_token == .Period)) {
                                try self.diagnostics.reportDiagnosticFormatted(
                                    .Error,
                                    self.lexer.token_start,
                                    "Expected '.', '|', '<-' or '=' after slot name, got '{s}'",
                                    .{self.lexer.current_token.toString()},
                                );

                                self.allocator.free(identifier_copy);
                                break :slot_parsing false;
                            }
                        }

                        // TODO: Intern these
                        const nil_identifier = try self.allocator.dupe(u8, "nil");
                        break :value_parsing AST.ExpressionNode{ .Identifier = AST.IdentifierNode{ .value = nil_identifier } };
                    };

                    try slots.append(AST.SlotNode{
                        .is_mutable = is_mutable,
                        .is_parent = false,
                        .is_argument = is_argument,
                        .name = identifier_copy,
                        .arguments = try self.allocator.alloc([]u8, 0),
                        .value = value,
                    });

                    break :slot_parsing true;
                }

                _ = try self.lexer.nextToken();
                break :slot_parsing true;
            };

            if (!did_parse_slot) {
                // Attempt to recover by eating up to either the pipe or period
                while (self.lexer.current_token != .Period and self.lexer.current_token != .Pipe) {
                    _ = try self.lexer.nextToken();
                }
            }

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
        }
    }

    _ = try self.lexer.nextToken();

    while (self.lexer.current_token != .BracketClose) {
        // NOTE: parseStatement will have handled the "consuming until end of
        //       statement" part here, so we don't need to do it ourselves.
        if (try self.parseStatement(.BracketClose)) |*statement| {
            errdefer statement.deinit(self.allocator);
            try statements.append(statement.*);
        }
    }

    _ = try self.lexer.nextToken();

    const block_node = try self.allocator.create(AST.BlockNode);
    block_node.slots = slots.toOwnedSlice();
    block_node.statements = statements.toOwnedSlice();

    return block_node;
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
