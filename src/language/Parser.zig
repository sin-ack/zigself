// Copyright (c) 2022-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const tracy = @import("tracy");
const Allocator = std.mem.Allocator;

const AST = @import("./ast.zig");
const Diagnostics = @import("./Diagnostics.zig");
const Tokenizer = @import("./Tokenizer.zig");
const Token = @import("./Token.zig");
const Location = @import("./Location.zig");

allocator: Allocator,
buffer: []const u8,
diagnostics: Diagnostics,

tokens: std.MultiArrayList(struct {
    tag: Token.Tag,
    start: usize,
}),
token_tags: []Token.Tag = undefined,
token_starts: []usize = undefined,
token_index: TokenIndex,
line_offsets: std.ArrayList(usize),

pub const ParseError = Allocator.Error;
const Parser = @This();
const TokenIndex = enum(usize) { _ };

/// A list of slots that are currently being built. Ensures that all slots have
/// unique names (which significantly simplifies the implementation of the later
/// stages of the VM).
const SlotList = struct {
    slots: std.ArrayList(AST.SlotNode) = .empty,

    pub const empty: SlotList = .{};
    pub const Error = error{SlotExistsWithSameName};

    pub fn deinit(self: *SlotList, allocator: Allocator) void {
        for (self.slots.items) |*slot| {
            slot.deinit(allocator);
        }
        self.slots.deinit(allocator);
    }

    /// Append a new slot to the list. Returns an error if the slot name
    /// collides with an existing slot name.
    pub fn append(
        self: *SlotList,
        allocator: Allocator,
        slot: AST.SlotNode,
    ) !void {
        // Ensure that the slot name is unique.
        for (self.slots.items) |existing_slot| {
            if (std.mem.eql(u8, existing_slot.name, slot.name)) {
                return Error.SlotExistsWithSameName;
            }
        }

        // Append the slot.
        try self.slots.append(allocator, slot);
    }

    /// Extract the slots into a slice. From this point on, the slots should
    /// be treated as immutable. Caller will take ownership of the slice.
    pub fn toOwnedSlice(self: *SlotList, allocator: Allocator) ![]AST.SlotNode {
        // Sort argument slots last. This ensures a consistent ordering, and is
        // relied upon in object layout.
        std.mem.sort(AST.SlotNode, self.slots.items, {}, struct {
            pub fn lessThan(context: void, lhs: AST.SlotNode, rhs: AST.SlotNode) bool {
                _ = context;
                return !lhs.is_argument and rhs.is_argument;
            }
        }.lessThan);

        return try self.slots.toOwnedSlice(allocator);
    }
};

pub fn createFromFile(allocator: Allocator, file_path: []const u8) !*Parser {
    const cwd = std.fs.cwd();
    const file_contents = try cwd.readFileAlloc(file_path, allocator, .unlimited);
    errdefer allocator.free(file_contents);

    var self = try allocator.create(Parser);
    errdefer allocator.destroy(self);
    try self.init(allocator, file_contents, true);
    return self;
}

pub fn createFromString(allocator: Allocator, contents: []const u8) !*Parser {
    const contents_copy = try allocator.dupeZ(u8, contents);
    errdefer allocator.free(contents_copy);

    var self = try allocator.create(Parser);
    errdefer allocator.destroy(self);
    try self.init(allocator, contents_copy, false);
    return self;
}

fn init(self: *Parser, allocator: Allocator, buffer: []const u8, from_file: bool) !void {
    self.* = .{
        .allocator = allocator,
        .buffer = buffer,
        .token_index = @enumFromInt(0),
        .tokens = .empty,
        .line_offsets = .empty,
        .diagnostics = try Diagnostics.init(allocator),
    };
    errdefer self.diagnostics.deinit();
    errdefer self.tokens.deinit(allocator);
    errdefer self.line_offsets.deinit(allocator);

    var tokenizer = Tokenizer.init(self.buffer);
    if (from_file) {
        tokenizer.skipShebangFromStart();
    }

    while (true) {
        const token = tokenizer.next();
        try self.tokens.append(allocator, .{
            .tag = token.tag,
            .start = token.location.start,
        });

        if (token.tag == .EOF or token.tag == .Invalid)
            break;
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

fn deinit(self: *Parser) void {
    self.line_offsets.deinit(self.allocator);
    self.tokens.deinit(self.allocator);
    self.allocator.free(self.buffer);
    self.diagnostics.deinit();
}

pub fn destroy(self: *Parser) void {
    self.deinit();
    self.allocator.destroy(self);
}

/// Peek at the current token.
fn peek(self: *Parser) Token.Tag {
    const token_index = @intFromEnum(self.token_index);
    if (token_index >= self.token_tags.len) {
        return .EOF;
    }
    return self.token_tags[token_index];
}

/// Move the token index forward and return the previous token index.
/// If the last token had already been consumed, returns the index of the last token.
fn consume(self: *Parser) TokenIndex {
    const result = @intFromEnum(self.token_index);
    if (result < self.token_tags.len) {
        self.token_index = @enumFromInt(result + 1);
    }
    return @enumFromInt(result);
}

/// Try to consume the given token, or return null.
fn tryConsume(self: *Parser, tag: Token.Tag) ?TokenIndex {
    if (self.tokenAt(self.token_index) == tag)
        return self.consume();
    return null;
}

/// Expect the given token and try to consume it, or produce a diagnostic and return null.
fn expectToken(self: *Parser, expected: Token.Tag) !?TokenIndex {
    return self.expectTokenContext(expected, null);
}

/// Expect the given token and try to consume it, or produce a diagnostic and return null.
/// If `context` is non-null, it will be included in the diagnostic message after the expected token,
/// i.e. "Expected 'foo' <context>, found 'bar'".
fn expectTokenContext(self: *Parser, expected: Token.Tag, context: ?[]const u8) !?TokenIndex {
    const actual = self.peek();
    if (actual != expected) {
        try self.diagnostics.reportDiagnosticFormatted(
            .Error,
            self.offsetToLocation(self.tokenStartAt(self.token_index)),
            "Expected '{s}'{s}{s}, found '{s}'",
            .{ expected.symbol(), if (context != null) " " else "", context orelse "", actual.symbol() },
        );
        return null;
    }
    return self.consume();
}

/// Assert that the current token is the given tag.
/// When it isn't, panics when runtime safety is enabled and causes UB otherwise.
/// Only used for checking invariants in debug builds.
fn assertToken(self: *Parser, tag: Token.Tag) void {
    std.debug.assert(self.peek() == tag);
}

/// Turn an offset into a line-and-column location.
pub fn offsetToLocation(self: Parser, offset: usize) Location {
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
        if (line_end > 0 and self.buffer[line_end - 1] == '\n')
            line_end -= 1;
    }

    return Location{
        .line = line,
        .column = offset - line_start + 1,
        .line_start = line_start,
        .line_end = line_end,
    };
}

/// Get a non-owning slice of the identifier at the given token index.
/// The token at the given index must be an identifier.
/// The returned slice will be at least one byte long.
fn getIdentifierSlice(self: Parser, index: TokenIndex) []const u8 {
    std.debug.assert(self.tokenAt(index) == .Identifier);

    const start = self.tokenStartAt(index);
    const next_start = self.tokenStartAt(@enumFromInt(@intFromEnum(index) + 1));
    var end = next_start - 1;

    while (true) {
        switch (self.buffer[end]) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => break,
            else => end -= 1,
        }
    }

    // The end needs to be exclusive.
    end += 1;

    // Can't have 0-length identifiers.
    std.debug.assert(end > start);
    return self.buffer[start..end];
}

/// Copy the identifier at the given token index.
fn getIdentifierCopy(self: Parser, index: TokenIndex) Allocator.Error![]const u8 {
    const slice = self.getIdentifierSlice(index);
    return try self.allocator.dupe(u8, slice);
}

/// Get a non-owning slice of the keyword at the given token index.
fn getKeywordSlice(self: Parser, index: TokenIndex) []const u8 {
    std.debug.assert(self.tokenAt(index) == .FirstKeyword or self.tokenAt(index) == .RestKeyword);

    const start = self.tokenStartAt(index);
    const next_start = self.tokenStartAt(@enumFromInt(@intFromEnum(index) + 1));
    var end = next_start;

    while (self.buffer[end] != ':') : (end -= 1) {}
    // The end needs to be exclusive.
    end += 1;

    // A keyword must be at least one character long, plus the colon.
    std.debug.assert(end - start >= 2);
    return self.buffer[start..end];
}

/// Ensure that the current identifier token isn't primitive (doesn't start with `_`).
/// Returns true if the identifier is non-primitive, false otherwise.
fn expectNonPrimitiveIdentifier(self: *Parser) !bool {
    self.assertToken(.Identifier);

    const token_start = self.tokenStartAt(self.token_index);
    if (self.buffer[token_start] == '_') {
        try self.diagnostics.reportDiagnostic(
            .Error,
            self.offsetToLocation(self.tokenStartAt(self.token_index)),
            "Expected non-primitive identifier",
        );
        return false;
    }

    return true;
}

/// Get the token at the given index.
fn tokenAt(self: Parser, index: TokenIndex) Token.Tag {
    return self.token_tags[@intFromEnum(index)];
}

/// Get the token start offset at the given index.
fn tokenStartAt(self: Parser, index: TokenIndex) usize {
    return self.token_starts[@intFromEnum(index)];
}

// <Script> ::= <MethodSlotList>? <StatementList>? <Whitespace>
pub fn parseScript(self: *Parser) ParseError!?AST.ScriptNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var slots = try self.parseSlotList(.Method);
    defer slots.deinit(self.allocator);

    const statements = try self.parseStatementList(.DisallowNonlocalReturns, .EOF);
    errdefer statements.unrefWithAllocator(self.allocator);

    const slots_slice = try slots.toOwnedSlice(self.allocator);
    return AST.ScriptNode{
        .range = .{
            .start = 0,
            .end = self.buffer.len,
        },
        .slots = slots_slice,
        .statements = statements,
    };
}

// std.io.getStdErr().writer().writeByteNTimes(' ', self.depth * 2) catch unreachable;
// std.debug.print(@src().fn_name ++ ": self.peek() = {s}, self.token_index = {}\n", .{ @tagName(self.peek()), @intFromEnum(self.token_index) });
// self.depth += 1;
// defer self.depth -= 1;

// <StatementList> ::= <Expression> ("." <Expression>)* "."?
// <BlockStatementList> ::= (<Expression> ("." <Expression>)*)? "^"? <Expression> "."?
const StatementListNonlocalReturns = enum { AllowNonlocalReturns, DisallowNonlocalReturns };
fn parseStatementList(self: *Parser, nonlocal_returns: StatementListNonlocalReturns, closing_token: Token.Tag) ParseError!AST.StatementList.Ref {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var statements: std.ArrayList(AST.ExpressionNode) = .empty;
    defer {
        for (statements.items) |*expression| {
            expression.deinit(self.allocator);
        }
        statements.deinit(self.allocator);
    }

    var nonlocal_return_start: ?usize = null;
    if (self.tryConsume(closing_token) == null) parse_first_statement: {
        {
            if (self.peek() == .Cap and nonlocal_returns == .AllowNonlocalReturns) {
                nonlocal_return_start = self.tokenStartAt(self.consume());
            }

            var expression = (try self.parseExpression(.WithParenExpr)) orelse break :parse_first_statement;
            errdefer expression.deinit(self.allocator);

            try statements.append(self.allocator, expression);
        }

        while (self.tryConsume(.Period)) |_| {
            if (self.peek() == closing_token) {
                break;
            }

            if (nonlocal_return_start) |start| {
                try self.diagnostics.reportDiagnostic(
                    .Error,
                    self.offsetToLocation(start),
                    "Non-local return must be the last statement in blocks",
                );
            }

            if (self.peek() == .Cap and nonlocal_returns == .AllowNonlocalReturns) {
                nonlocal_return_start = self.tokenStartAt(self.consume());
            }

            var expression = (try self.parseExpression(.WithParenExpr)) orelse break;
            errdefer expression.deinit(self.allocator);

            try statements.append(self.allocator, expression);
        }

        _ = try self.expectTokenContext(closing_token, "after statement list");
    }

    if (nonlocal_return_start) |start| {
        // Turn the last expression into a non-local return.
        var last_expression = statements.items[statements.items.len - 1];

        const return_node = try self.allocator.create(AST.ReturnNode);
        // NOTE: No errdefer here as owned_statements will handle the error condition.
        return_node.* = .{
            .expression = last_expression,
            .range = .{ .start = start, .end = last_expression.range().end },
        };

        statements.items[statements.items.len - 1] = AST.ExpressionNode{ .Return = return_node };
    }

    const owned_statements = try statements.toOwnedSlice(self.allocator);
    errdefer {
        for (statements.items) |*expression| {
            expression.deinit(self.allocator);
        }
        self.allocator.free(owned_statements);
    }

    return AST.StatementList.create(self.allocator, owned_statements);
}

// <SlotsSlotList> ::= "|" "|" | "|" <SlotsSlot> ("." <SlotsSlot>)* "."? "|"
// <MethodSlotList> ::= "|" "|" | "|" <MethodSlot> ("." <MethodSlot>)* "."? "|"
// <BlockSlotList> ::= "|" "|" | "|" <BlockSlot> ("." <BlockSlot>)* (".")? "|"
const SlotType = enum { Slots, Method, Block };
fn parseSlotList(self: *Parser, slot_type: SlotType) ParseError!SlotList {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    _ = self.tryConsume(.Pipe) orelse return .empty;
    // Empty slot list.
    if (self.tryConsume(.Pipe)) |_| return .empty;

    var slots: SlotList = .empty;
    errdefer slots.deinit(self.allocator);

    {
        var slot = (try self.parseSlot(slot_type)) orelse return slots;
        errdefer slot.deinit(self.allocator);

        slots.append(self.allocator, slot) catch |err| switch (err) {
            SlotList.Error.SlotExistsWithSameName => {
                try self.diagnostics.reportDiagnosticFormatted(
                    .Error,
                    self.offsetToLocation(slot.range.start),
                    "Slot with name '{s}' already defined",
                    .{slot.name},
                );
                return slots;
            },
            else => |e| return e,
        };
    }

    while (self.tryConsume(.Period)) |_| {
        if (self.peek() == .Pipe) {
            break;
        }

        var slot = (try self.parseSlot(slot_type)) orelse return slots;
        errdefer slot.deinit(self.allocator);

        slots.append(self.allocator, slot) catch |err| switch (err) {
            SlotList.Error.SlotExistsWithSameName => {
                try self.diagnostics.reportDiagnosticFormatted(
                    .Error,
                    self.offsetToLocation(slot.range.start),
                    "Slot with name '{s}' already defined",
                    .{slot.name},
                );
                return slots;
            },
            else => |e| return e,
        };
    }

    _ = try self.expectTokenContext(.Pipe, "after slot list");
    return slots;
}

// <SlotsSlot> ::= <CommonSlot> | <NonPrimitiveIdentifier> "*" ("=" | "<-") <Expression>
// <BlockSlot> ::= <CommonSlot> | ":" <NonPrimitiveIdentifier>
// <MethodSlot> ::= <CommonSlot>
fn parseSlot(self: *Parser, slot_type: SlotType) ParseError!?AST.SlotNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const start = self.tokenStartAt(self.token_index);

    return switch (slot_type) {
        .Slots, .Method => blk: {
            const current = self.peek();
            if (current == .Identifier) {
                if (!try self.expectNonPrimitiveIdentifier()) return null;
                const identifier_token = self.consume();

                // Parent slot
                if (self.tryConsume(.Asterisk)) |_| {
                    if (slot_type == .Method) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start),
                            "Method slots can't have parent slots",
                        );
                        return null;
                    }

                    const is_mutable = if (self.peek() == .Equals)
                        false
                    else if (self.peek() == .Arrow)
                        true
                    else {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(self.tokenStartAt(self.token_index)),
                            "Expected '=' or '<-' after '*' in slot",
                        );
                        return null;
                    };
                    _ = self.consume();

                    var expression = (try self.parseExpression(.WithParenExpr)) orelse return null;
                    errdefer expression.deinit(self.allocator);

                    const end = self.tokenStartAt(self.token_index);
                    break :blk AST.SlotNode{
                        .range = .{
                            .start = start,
                            .end = end,
                        },
                        .name = try self.getIdentifierCopy(identifier_token),
                        .value = expression,

                        .is_parent = true,
                        .is_mutable = is_mutable,
                        .is_argument = false,
                    };
                }

                break :blk self.parseSlotCommonIdentifier(identifier_token);
            }

            break :blk self.parseSlotCommon();
        },
        .Block => blk: {
            // Argument slot
            if (self.tryConsume(.Colon)) |_| {
                if (self.peek() != .Identifier) {
                    try self.diagnostics.reportDiagnostic(
                        .Error,
                        self.offsetToLocation(self.tokenStartAt(self.token_index)),
                        "Expected identifier after ':' in argument slot",
                    );
                    return null;
                }
                if (!try self.expectNonPrimitiveIdentifier()) return null;

                const identifier_token = self.consume();

                const end = self.tokenStartAt(self.token_index);
                break :blk AST.SlotNode{
                    .range = .{
                        .start = start,
                        .end = end,
                    },
                    .name = try self.getIdentifierCopy(identifier_token),
                    .value = null,

                    .is_parent = false,
                    .is_mutable = true,
                    .is_argument = true,
                };
            }

            break :blk self.parseSlotCommon();
        },
    };
}

// <CommonSlot> ::= <NonPrimitiveIdentifier> "=" <MethodOrExpression>
//                | <NonPrimitiveIdentifier> "<-" <Expression>
//                | <NonPrimitiveIdentifier>
//                | <MethodSlotName> "=" <Method>
fn parseSlotCommon(self: *Parser) ParseError!?AST.SlotNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const start = self.tokenStartAt(self.token_index);

    const current = self.peek();
    if (current == .Identifier) {
        if (!try self.expectNonPrimitiveIdentifier()) return null;
        const identifier_index = self.consume();
        return self.parseSlotCommonIdentifier(identifier_index);
    }

    // Method slot. The name is either a binary or keyword message.
    if (!(current == .FirstKeyword or current.isOperator())) {
        try self.diagnostics.reportDiagnostic(
            .Error,
            self.offsetToLocation(start),
            "Expected identifier, keyword or operator for slot name",
        );
        return null;
    }

    var argument_names: std.ArrayList([]const u8) = .empty;
    var did_transfer_argument_names = false;
    defer {
        if (!did_transfer_argument_names) {
            for (argument_names.items) |argument_name| {
                self.allocator.free(argument_name);
            }
        }
        argument_names.deinit(self.allocator);
    }

    const slot_name = (try self.parseMethodSlotName(&argument_names)) orelse return null;
    errdefer self.allocator.free(slot_name);
    _ = try self.expectTokenContext(.Equals, "after method slot name");

    const method = (try self.parseMethod(argument_names.items, .DisallowParentSlots)) orelse return null;
    did_transfer_argument_names = true;

    const end = self.tokenStartAt(self.token_index);
    return AST.SlotNode{
        .range = .{
            .start = start,
            .end = end,
        },
        .name = slot_name,
        .value = .{ .Object = method },

        .is_parent = false,
        .is_mutable = false,
        .is_argument = false,
    };
}

// <MethodSlotName> ::= <FirstKeywordName> <NonPrimitiveIdentifier> (<RestKeywordName> <NonPrimitiveIdentifier>)*
//                    | <BinaryOp>+ <NonPrimitiveIdentifier>
fn parseMethodSlotName(self: *Parser, argument_names: *std.ArrayList([]const u8)) ParseError!?[]const u8 {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var slot_name: std.ArrayList(u8) = .empty;
    defer slot_name.deinit(self.allocator);

    if (self.peek() == .FirstKeyword) {
        try slot_name.appendSlice(self.allocator, self.getKeywordSlice(self.consume()));

        var identifier_index = (try self.expectToken(.Identifier)) orelse return null;
        var identifier = try self.getIdentifierCopy(identifier_index);
        errdefer self.allocator.free(identifier);
        try argument_names.append(self.allocator, identifier);

        while (self.peek() == .RestKeyword) {
            try slot_name.appendSlice(self.allocator, self.getKeywordSlice(self.consume()));

            identifier_index = (try self.expectToken(.Identifier)) orelse return null;
            identifier = try self.getIdentifierCopy(identifier_index);
            errdefer self.allocator.free(identifier);
            try argument_names.append(self.allocator, identifier);
        }

        return try slot_name.toOwnedSlice(self.allocator);
    }

    std.debug.assert(self.peek().isOperator());

    while (self.peek().isOperator()) {
        try slot_name.appendSlice(self.allocator, self.peek().symbol());
        _ = self.consume();
    }

    const identifier_index = (try self.expectToken(.Identifier)) orelse return null;
    const identifier = try self.getIdentifierCopy(identifier_index);
    errdefer self.allocator.free(identifier);
    try argument_names.append(self.allocator, identifier);

    return try slot_name.toOwnedSlice(self.allocator);
}

fn parseSlotCommonIdentifier(self: *Parser, identifier_index: TokenIndex) ParseError!?AST.SlotNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const start = self.tokenStartAt(identifier_index);

    // Constant slot. Can be either a method or an expression.
    if (self.tryConsume(.Equals)) |_| {
        const expression = try self.parseMethodOrExpression() orelse return null;
        const end = self.tokenStartAt(self.token_index);

        return AST.SlotNode{
            .range = .{
                .start = start,
                .end = end,
            },
            .name = try self.getIdentifierCopy(identifier_index),
            .value = expression,

            .is_parent = false,
            .is_mutable = false,
            .is_argument = false,
        };
    }

    // Assignable slot. Can't hold a method (at least syntactically).
    if (self.tryConsume(.Arrow)) |_| {
        const expression = try self.parseExpression(.WithParenExpr) orelse return null;
        const end = self.tokenStartAt(self.token_index);

        return AST.SlotNode{
            .range = .{
                .start = start,
                .end = end,
            },
            .name = try self.getIdentifierCopy(identifier_index),
            .value = expression,

            .is_parent = false,
            .is_mutable = true,
            .is_argument = false,
        };
    }

    // Assignable slot with no value.
    const end = self.tokenStartAt(self.token_index);
    return AST.SlotNode{
        .range = .{
            .start = start,
            .end = end,
        },
        .name = try self.getIdentifierCopy(identifier_index),
        .value = null,

        .is_parent = false,
        .is_mutable = true,
        .is_argument = false,
    };
}

// <MethodOrExpression> ::= <NonParenthesizedExpression> | <SendToParenthesizedExpression> | <Method>
// <SendToParenthesizedExpression> ::= "(" <Expression> ")" <MessageSend>
fn parseMethodOrExpression(self: *Parser) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (self.peek() != .ParenOpen) {
        // Simple case: just a (non-parenthesized) expression.
        return self.parseExpression(.WithoutParenExpr);
    }

    const start = self.token_index;
    // The ugliest part of Self syntax... Since method objects just look like
    // slots objects with statements added, we have no idea whether we need to
    // parse what's ahead of us as a slots object or as a method object until we
    // see what's after the closing parenthesis. We parse a "method" first while
    // allowing parent slots, and then check what comes after; if it looks like
    // a message send, we reset the parser state and parse a slots object.
    // Otherwise, we reset the parser state and parse a method object, this time
    // disallowing parent slots.
    //
    // Even worse, parenthesized expressions are really just single-statement
    // method objects, so we need to check for that as well. In that case we simply
    // extract the expression from the method object.
    {
        const method = (try self.parseMethod(&.{}, .AllowParentSlots)) orelse return null;
        defer method.destroy(self.allocator);

        // If what's in front of us looks like a message send...
        if (self.peek() == .FirstKeyword or self.peek().isOperator() or self.peek() == .Identifier) {
            // ...then this "method" must either be a "single-statement method object"
            // (i.e. parenthesized expression) or a slots object with no statements.
            var expression = if (method.slots.len == 0 and method.statements.value.statements.len == 1) blk: {
                const expression = method.statements.value.statements[0];
                // Prevent the method from freeing the expression.
                method.statements.value.statements.len = 0;
                break :blk expression;
            } else if (method.statements.value.statements.len == 0) blk: {
                self.token_index = start;
                const slots = (try self.parseSlotsObject()) orelse return null;
                break :blk AST.ExpressionNode{ .Object = slots };
            } else {
                try self.diagnostics.reportDiagnostic(
                    .Error,
                    self.offsetToLocation(method.range.start),
                    "Messages can only be sent to expressions, found method",
                );
                method.destroy(self.allocator);
                return null;
            };

            errdefer expression.deinit(self.allocator);
            return (try self.maybeParseMessageSend(expression)) orelse null;
        }

        // Alternatively, if the "method" has no statements, then assume it's a
        // slots object.
        if (method.statements.value.statements.len == 0) {
            self.token_index = start;
            const slots = (try self.parseSlotsObject()) orelse return null;
            return AST.ExpressionNode{ .Object = slots };
        }
    }

    // Otherwise, it's just a method object. Parse it properly this time.
    self.token_index = start;
    const method = (try self.parseMethod(&.{}, .DisallowParentSlots)) orelse return null;
    return AST.ExpressionNode{ .Object = method };
}

/// Parse a method. The strings in `argument_names` will be owned by the returned method object.
// <Method> ::= "(" <MethodSlotList>? <StatementList>? ")"
const MethodParentSlotPermissiveness = enum { AllowParentSlots, DisallowParentSlots };
fn parseMethod(self: *Parser, argument_names: []const []const u8, permissiveness: MethodParentSlotPermissiveness) ParseError!?*AST.ObjectNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const start = self.tokenStartAt(self.consume());

    var slots = try self.parseSlotList(if (permissiveness == .AllowParentSlots) .Slots else .Method);
    defer slots.deinit(self.allocator);

    for (argument_names) |argument_name| {
        const argument_slot = AST.SlotNode{
            // FIXME: This should point to the AST node that caused the argument
            //        slot to be defined (:foo in blocks or `keyword: [foo]` in
            //        methods).
            .range = .{
                .start = 0,
                .end = 1,
            },
            .name = argument_name,
            .value = null,

            .is_parent = false,
            .is_mutable = true,
            .is_argument = true,
        };

        slots.append(self.allocator, argument_slot) catch |err| switch (err) {
            SlotList.Error.SlotExistsWithSameName => {
                try self.diagnostics.reportDiagnosticFormatted(
                    .Error,
                    self.offsetToLocation(start),
                    "Slot with name '{s}' already defined",
                    .{argument_name},
                );
                return null;
            },
            else => return @errorCast(err),
        };
    }

    const statements = try self.parseStatementList(.DisallowNonlocalReturns, .ParenClose);
    errdefer statements.unrefWithAllocator(self.allocator);

    const end = self.tokenStartAt(self.token_index);
    const method_node = try self.allocator.create(AST.ObjectNode);
    errdefer method_node.destroy(self.allocator);

    const slots_slice = try slots.toOwnedSlice(self.allocator);
    method_node.* = .{
        .range = .{
            .start = start,
            .end = end,
        },
        .slots = slots_slice,
        .statements = statements,
    };

    return method_node;
}

// <Expression> ::= <KeywordExpression> (";" <MessageSend>)*
// <NonParenthesizedExpression> ::= <NonParenthesizedKeywordExpression> (";" <MessageSend>)*
fn parseExpression(self: *Parser, mode: PrimaryMode) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var expression = (try self.parseKeywordExpression(mode)) orelse return null;
    // NOTE: `maybeParseMessageSend` will free `expression` if it fails.

    while (self.tryConsume(.Semicolon)) |_| {
        expression = (try self.maybeParseMessageSend(expression)) orelse return null;
    }
    return expression;
}

// <KeywordExpression> ::= (<KeywordSend> | <BinarySend> <KeywordSend>? | <Primary> <MessageSend>?)
// <NonParenthesizedKeywordExpression> ::= (<KeywordSend> | <BinarySend> <KeywordSend>? | <PrimaryNoParenExpr> <MessageSend>?)
fn parseKeywordExpression(self: *Parser, mode: PrimaryMode) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const current_tag = self.peek();
    return if (current_tag == .FirstKeyword)
        (try self.parseKeywordSend(null)) orelse return null
    else if (current_tag.isOperator()) blk: {
        var primary = (try self.parseBinarySend(null, null)) orelse return null;
        errdefer primary.deinit(self.allocator);

        if (self.peek() == .FirstKeyword) {
            primary = (try self.parseKeywordSend(primary)) orelse {
                primary.deinit(self.allocator);
                return null;
            };
        }
        break :blk primary;
    } else blk: {
        var primary = (try self.parsePrimary(mode)) orelse return null;
        errdefer primary.deinit(self.allocator);

        // NOTE: `maybeParseMessageSend` will free `primary` if it fails.
        break :blk (try self.maybeParseMessageSend(primary)) orelse return null;
    };
}

/// Try to parse a message send with `receiver` as its receiver.
/// If the current token doesn't look like a message send, return `receiver`
/// as-is; otherwise, return the wrapping message send.
/// On parse error, return null.
/// Takes ownership of `receiver` and will free it on parse error.
// <MessageSend> ::= <KeywordSend> | <BinarySend> <KeywordSend>? | <UnarySend>+ <BinarySend>? <KeywordSend>?
fn maybeParseMessageSend(self: *Parser, receiver: AST.ExpressionNode) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const current_tag = self.peek();
    if (current_tag == .FirstKeyword) {
        return (try self.parseKeywordSend(receiver)) orelse null;
    }

    if (current_tag.isOperator()) {
        var binary_send = (try self.parseBinarySend(receiver, null)) orelse return null;
        errdefer binary_send.deinit(self.allocator);

        if (self.peek() == .FirstKeyword) {
            binary_send = (try self.parseKeywordSend(binary_send)) orelse {
                binary_send.deinit(self.allocator);
                return null;
            };
        }

        return binary_send;
    }

    var message = receiver;
    while (self.peek() == .Identifier) {
        message = (try self.parseUnarySend(message)) orelse {
            message.deinit(self.allocator);
            return null;
        };
    }

    if (self.peek().isOperator()) {
        message = (try self.parseBinarySend(message, null)) orelse {
            message.deinit(self.allocator);
            return null;
        };
    }

    if (self.peek() == .FirstKeyword) {
        message = (try self.parseKeywordSend(message)) orelse {
            message.deinit(self.allocator);
            return null;
        };
    }

    return message;
}

// <KeywordSend> ::= <FirstKeywordName> <KeywordExpression> (<RestKeywordName> <KeywordExpression>)*
fn parseKeywordSend(self: *Parser, receiver: ?AST.ExpressionNode) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    self.assertToken(.FirstKeyword);
    const first_keyword = self.consume();
    const start = self.tokenStartAt(first_keyword);

    var message: std.ArrayList(u8) = .empty;
    defer message.deinit(self.allocator);

    var arguments: std.ArrayList(AST.ExpressionNode) = .empty;
    defer {
        for (arguments.items) |*argument| {
            argument.deinit(self.allocator);
        }
        arguments.deinit(self.allocator);
    }

    try message.appendSlice(self.allocator, self.getKeywordSlice(first_keyword));
    {
        var argument = (try self.parseKeywordExpression(.WithParenExpr)) orelse return null;
        errdefer argument.deinit(self.allocator);

        try arguments.append(self.allocator, argument);
    }

    while (self.tryConsume(.RestKeyword)) |rest_keyword| {
        try message.appendSlice(self.allocator, self.getKeywordSlice(rest_keyword));

        var argument = (try self.parseKeywordExpression(.WithParenExpr)) orelse return null;
        errdefer argument.deinit(self.allocator);

        try arguments.append(self.allocator, argument);
    }

    const message_slice = try message.toOwnedSlice(self.allocator);
    errdefer self.allocator.free(message_slice);
    const arguments_slice = try arguments.toOwnedSlice(self.allocator);
    errdefer self.allocator.free(arguments_slice);

    const end = self.tokenStartAt(self.token_index);
    const message_node = try self.allocator.create(AST.MessageNode);
    message_node.* = .{
        .range = .{
            .start = start,
            .end = end,
        },
        .receiver = receiver,
        .message_name = message_slice,
        .arguments = arguments_slice,
    };

    return AST.ExpressionNode{ .Message = message_node };
}

// <BinarySend> ::= <BinaryOp>+ <BinaryExpression>
// FIXME: Binary sends are currently right-associative, but they should be left-associative,
//        because it's more intuitive. i.e. `a + b + c` should be parsed as `(a + b) + c`.
fn parseBinarySend(self: *Parser, receiver: ?AST.ExpressionNode, previous_operator: ?[]const u8) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const start = self.tokenStartAt(self.token_index);
    std.debug.assert(self.peek().isOperator());

    var operator: std.ArrayList(u8) = .empty;
    defer operator.deinit(self.allocator);

    // FIXME: Disallow whitespace between characters in binary operators
    while (self.peek().isOperator()) {
        try operator.appendSlice(self.allocator, self.peek().symbol());
        _ = self.consume();
    }

    std.debug.assert(operator.items.len > 0);

    if (previous_operator) |previous| {
        if (!std.mem.eql(u8, operator.items, previous)) {
            try self.diagnostics.reportDiagnostic(
                .Error,
                self.offsetToLocation(start),
                "Different binary messages cannot be chained",
            );
            return null;
        }
    }

    const operator_slice = try operator.toOwnedSlice(self.allocator);
    errdefer self.allocator.free(operator_slice);

    var argument = (try self.parseBinaryExpression(operator_slice)) orelse return null;
    errdefer argument.deinit(self.allocator);

    const arguments_slice = try self.allocator.alloc(AST.ExpressionNode, 1);
    errdefer self.allocator.free(arguments_slice);
    arguments_slice[0] = argument;

    const end = self.tokenStartAt(self.token_index);
    const message_node = try self.allocator.create(AST.MessageNode);
    errdefer self.allocator.destroy(message_node);
    message_node.* = .{
        .range = .{
            .start = start,
            .end = end,
        },
        .receiver = receiver,
        .message_name = operator_slice,
        .arguments = arguments_slice,
    };

    return AST.ExpressionNode{ .Message = message_node };
}

// <BinaryExpression> ::= <Primary> <UnaryMessage>* <BinaryMessage>?
fn parseBinaryExpression(self: *Parser, previous_operator: []const u8) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var primary = (try self.parsePrimary(.WithParenExpr)) orelse return null;
    errdefer primary.deinit(self.allocator);

    while (self.peek() == .Identifier) {
        const unary_message = (try self.parseUnarySend(primary)) orelse return null;
        primary = AST.ExpressionNode{ .Message = unary_message.Message };
    }

    if (self.peek().isOperator()) {
        primary = (try self.parseBinarySend(primary, previous_operator)) orelse return null;
    }

    return primary;
}

// <UnarySend> ::= <Identifier>
fn parseUnarySend(self: *Parser, receiver: ?AST.ExpressionNode) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const start = self.tokenStartAt(self.token_index);
    self.assertToken(.Identifier);

    const identifier_copy = try self.getIdentifierCopy(self.consume());
    errdefer self.allocator.free(identifier_copy);

    const end = self.tokenStartAt(self.token_index);
    const message_node = try self.allocator.create(AST.MessageNode);
    errdefer self.allocator.destroy(message_node);
    message_node.* = .{
        .range = .{
            .start = start,
            .end = end,
        },
        .receiver = receiver,
        .message_name = identifier_copy,
        .arguments = &.{},
    };

    return AST.ExpressionNode{ .Message = message_node };
}

// <PrimaryNoParenExpr> ::= <Integer> | <FloatingPoint> | <SlotsObject> | <Block> | <Identifier> | <String>
// <Primary> ::= <PrimaryNoParenExpr> | "(" <Expression> ")"
const PrimaryMode = enum { WithParenExpr, WithoutParenExpr };
fn parsePrimary(self: *Parser, mode: PrimaryMode) ParseError!?AST.ExpressionNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const current_tag = self.peek();
    const start = self.tokenStartAt(self.token_index);

    return switch (current_tag) {
        .Integer => AST.ExpressionNode{ .Number = try self.parseInteger() },
        .FloatingPoint => AST.ExpressionNode{ .Number = try self.parseFloatingPoint() },
        .ParenOpen => blk: {
            if (mode == .WithoutParenExpr) {
                const slots_object = (try self.parseSlotsObject()) orelse return null;
                return AST.ExpressionNode{ .Object = slots_object };
            }

            const paren_open = self.consume();
            if (self.peek() == .Pipe or self.peek() == .ParenClose) {
                self.token_index = paren_open;
                const slots_object = (try self.parseSlotsObject()) orelse return null;
                return AST.ExpressionNode{ .Object = slots_object };
            }

            var expression = (try self.parseExpression(.WithParenExpr)) orelse return null;
            errdefer expression.deinit(self.allocator);

            _ = try self.expectTokenContext(.ParenClose, "after expression");
            break :blk expression;
        },
        .BracketOpen => blk: {
            const block = (try self.parseBlock()) orelse return null;
            break :blk AST.ExpressionNode{ .Block = block };
        },
        .Identifier => AST.ExpressionNode{ .Identifier = try self.parseIdentifier() },
        .String => blk: {
            const string = (try self.parseString()) orelse return null;
            break :blk AST.ExpressionNode{ .String = string };
        },
        else => {
            try self.diagnostics.reportDiagnostic(
                .Error,
                self.offsetToLocation(start),
                "Expected primary expression",
            );
            return null;
        },
    };
}

// <Integer> ::= <BinaryInteger> | <OctalInteger> | <HexInteger> | <DecimalInteger>
// <BinaryInteger> ::= "0" "b" [0-1] ("_"? [0-1])*
// <OctalInteger> ::= "0" "o" [0-7] ("_"? [0-7])*
// <HexInteger> ::= "0" "x" <HexDigit> ("_"? <HexDigit>)*
// <DecimalInteger> ::= [0-9] ("_"? [0-9])*
// <HexDigit> ::= [0-9] | [a-f] | [A-F]
const IntegerParseState = enum { Start, Zero, Decimal, Hexadecimal, Octal, Binary };
fn parseInteger(self: *Parser) ParseError!AST.NumberNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    self.assertToken(.Integer);
    const start_of_number = self.tokenStartAt(self.consume());

    var integer: i63 = 0;
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
                'b', 'B' => {
                    state = .Binary;
                },
                '0' => {},
                '_' => {},
                '1'...'9' => {
                    integer = c - '0';
                    state = .Decimal;
                },
                else => break,
            },
            .Decimal => switch (c) {
                '0'...'9' => {
                    const digit = c - '0';

                    const mul_result = @mulWithOverflow(integer, 10);
                    if (mul_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = mul_result[0];

                    const add_result = @addWithOverflow(integer, digit);
                    if (add_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = add_result[0];
                },
                '_' => {},
                else => break,
            },
            .Hexadecimal => switch (c) {
                '0'...'9' => {
                    const digit = c - '0';

                    const mul_result = @mulWithOverflow(integer, 16);
                    if (mul_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = mul_result[0];

                    const add_result = @addWithOverflow(integer, digit);
                    if (add_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = add_result[0];
                },
                'A'...'F', 'a'...'f' => {
                    const digit = 10 + (if (c >= 'a') c - 'a' else c - 'A');

                    const mul_result = @mulWithOverflow(integer, 16);
                    if (mul_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = mul_result[0];

                    const add_result = @addWithOverflow(integer, digit);
                    if (add_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = add_result[0];
                },
                '_' => {},
                else => break,
            },
            .Octal => switch (c) {
                '0'...'7' => {
                    const digit = c - '0';

                    const mul_result = @mulWithOverflow(integer, 8);
                    if (mul_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = mul_result[0];

                    const add_result = @addWithOverflow(integer, digit);
                    if (add_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = add_result[0];
                },
                '8', '9' => unreachable,
                '_' => {},
                else => break,
            },
            .Binary => switch (c) {
                '0', '1' => {
                    const digit = c - '0';

                    const mul_result = @mulWithOverflow(integer, 2);
                    if (mul_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = mul_result[0];

                    const add_result = @addWithOverflow(integer, digit);
                    if (add_result[1] != 0) {
                        try self.diagnostics.reportDiagnostic(
                            .Error,
                            self.offsetToLocation(start_of_number),
                            "Value does not fit in 63-bit integer",
                        );
                        break;
                    }
                    integer = add_result[0];
                },
                '2'...'9' => unreachable,
                '_' => {},
                else => break,
            },
        }
    }

    const node = AST.NumberNode{
        .value = .{ .Integer = integer },
        .range = .{ .start = start_of_number, .end = offset },
    };
    return node;
}

// <FloatingPoint> ::= <DecimalInteger> "." <DecimalInteger>
const FloatingPointParseState = enum { Integer, Fraction };
fn parseFloatingPoint(self: *Parser) ParseError!AST.NumberNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    self.assertToken(.FloatingPoint);
    const start_of_number = self.tokenStartAt(self.consume());

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
                    result = (result * 10) + @as(f64, @floatFromInt(c - '0'));
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

    const divisor = std.math.pow(f64, 10.0, @as(f64, @floatFromInt(fraction_counter)));
    result += @as(f64, @floatFromInt(fraction)) / divisor;

    const node = AST.NumberNode{
        .value = .{ .FloatingPoint = result },
        .range = .{ .start = start_of_number, .end = offset },
    };
    return node;
}

// <SlotsObject> ::= "(" <Whitespace> <SlotsSlotList>? <Whitespace> ")"
fn parseSlotsObject(self: *Parser) ParseError!?*AST.ObjectNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    self.assertToken(.ParenOpen);
    const start = self.tokenStartAt(self.consume());

    var slots = try self.parseSlotList(.Slots);
    defer slots.deinit(self.allocator);

    _ = try self.expectTokenContext(.ParenClose, "after slots object");
    const end = self.tokenStartAt(self.token_index);

    var statements = try AST.StatementList.create(self.allocator, &.{});
    errdefer statements.unrefWithAllocator(self.allocator);

    const object_node = try self.allocator.create(AST.ObjectNode);
    errdefer object_node.destroy(self.allocator);

    const slots_slice = try slots.toOwnedSlice(self.allocator);
    object_node.* = .{
        .range = .{
            .start = start,
            .end = end,
        },
        .slots = slots_slice,
        .statements = statements,
    };

    return object_node;
}

// <Block> ::= "[" <Whitespace> <BlockSlotList>? <Whitespace> <BlockStatementList>? <Whitespace> "]"
fn parseBlock(self: *Parser) ParseError!?*AST.BlockNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    self.assertToken(.BracketOpen);
    const start = self.tokenStartAt(self.consume());

    var slots = try self.parseSlotList(.Block);
    errdefer slots.deinit(self.allocator);

    const statements = try self.parseStatementList(.AllowNonlocalReturns, .BracketClose);
    errdefer statements.unrefWithAllocator(self.allocator);

    const end = self.tokenStartAt(self.token_index);
    const block_node = try self.allocator.create(AST.BlockNode);
    errdefer block_node.destroy(self.allocator);

    const slots_slice = try slots.toOwnedSlice(self.allocator);
    block_node.* = .{
        .range = .{
            .start = start,
            .end = end,
        },
        .slots = slots_slice,
        .statements = statements,
    };

    return block_node;
}

// <NonPrimitiveIdentifier> ::= [a-z] ([a-z] | [A-Z] | [0-9])*
// <Identifier> ::= <NonPrimitiveIdentifier> | ("_" ([a-z] | [A-Z] | [0-9])+)
fn parseIdentifier(self: *Parser) ParseError!AST.IdentifierNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    self.assertToken(.Identifier);
    const start = self.tokenStartAt(self.token_index);

    const identifier = try self.getIdentifierCopy(self.consume());
    const end = self.tokenStartAt(self.token_index);

    return AST.IdentifierNode{
        .value = identifier,
        .range = .{ .start = start, .end = end },
    };
}

const StringParseState = enum { Start, Literal, Backslash };
fn parseString(self: *Parser) ParseError!?AST.StringNode {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    self.assertToken(.String);
    const start_of_string = self.tokenStartAt(self.token_index);
    _ = self.consume();

    var offset = start_of_string;
    var state = StringParseState.Start;
    var string_buffer: std.ArrayList(u8) = .empty;
    defer string_buffer.deinit(self.allocator);

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
                        .value = try string_buffer.toOwnedSlice(self.allocator),
                        .range = .{ .start = start_of_string, .end = offset + 1 },
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
                    try string_buffer.append(self.allocator, c);
                },
            },
            .Backslash => switch (c) {
                'n' => {
                    try string_buffer.append(self.allocator, '\n');
                    state = .Literal;
                },
                'r' => {
                    try string_buffer.append(self.allocator, '\r');
                    state = .Literal;
                },
                't' => {
                    try string_buffer.append(self.allocator, '\t');
                    state = .Literal;
                },
                '\\' => {
                    try string_buffer.append(self.allocator, '\\');
                    state = .Literal;
                },
                '\'' => {
                    try string_buffer.append(self.allocator, '\'');
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

                    try string_buffer.append(self.allocator, char);
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
