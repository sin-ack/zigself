// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const tokens = @import("./tokens.zig");
const Location = @import("./location.zig");

const MaximumLookaheadLength = 4096;
const LEXER_DEBUG = false;

const Self = @This();
const BufferType = std.io.FixedBufferStream([]const u8);
const PeekStreamType = std.io.PeekStream(.{ .Static = MaximumLookaheadLength }, BufferType.Reader);

// TODO: When Zig's RLS actually starts working properly, make this a static
//       function.
pub fn initInPlaceFromFilePath(self: *Self, file_path: []const u8, allocator: *std.mem.Allocator) !void {
    if (self.initialized)
        @panic("Attempting to initialize already-initialized lexer");

    const current_working_directory = std.fs.cwd();

    const file = try current_working_directory.openFile(file_path, .{});
    defer file.close();

    var file_contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    errdefer allocator.free(file_contents);

    var buffer = std.io.fixedBufferStream(@as([]const u8, file_contents));

    self.allocator = allocator;
    self.file_contents = file_contents;
    self.buffer = buffer;
    self.stream = std.io.peekStream(MaximumLookaheadLength, self.buffer.reader());
    self.reader = self.stream.reader();
    self.initialized = true;
}

pub fn deinit(self: *Self) void {
    if (!self.initialized)
        @panic("Attempting to deinit uninitialized lexer");

    self.allocator.free(self.file_contents);
}

/// Return the next token from the file stream.
pub fn nextToken(self: *Self) !*tokens.Token {
    if (!self.initialized)
        @panic("Attempting to call Lexer.nextToken on uninitialized lexer");

    self.consumed_whitespace = 0;

    while (true) {
        try self.skipWhitespace();
        if (self.eof()) {
            if (LEXER_DEBUG) std.debug.print("lexer: Reached EOF!\n", .{});
            self.current_token = tokens.Token{ .EOF = .{} };
            return &self.current_token;
        }

        self.token_start = self.token_end;

        if (try self.lexComment()) |token| {
            _ = token;
            if (LEXER_DEBUG) std.debug.print("lexer: Parsed a comment, continuing\n", .{});
        } else {
            break;
        }
    }

    if (try self.lexString()) |token| {
        if (LEXER_DEBUG) std.debug.print("lexer: Current token is now a string: \"{s}\"\n", .{token.String});
        self.current_token = token;
        return &self.current_token;
    }
    if (try self.lexNumber()) |token| {
        if (LEXER_DEBUG) std.debug.print("lexer: Current token is now a number\n", .{});
        self.current_token = token;
        return &self.current_token;
    }
    if (try self.lexIdentifier()) |token| {
        if (LEXER_DEBUG) std.debug.print("lexer: Current token is now an identifier: \"{s}\"\n", .{token.Identifier});
        self.current_token = token;
        return &self.current_token;
    }
    if (try self.lexSymbol()) |token| {
        if (LEXER_DEBUG) std.debug.print("lexer: Current token is now a symbol: '{s}'\n", .{token.toString()});
        self.current_token = token;
        return &self.current_token;
    }

    @panic("Shouldn't reach here!");
}

/// Return the line from the file contents corresponding to the given location.
pub fn getLineForLocation(self: Self, location: Location) ![]const u8 {
    var start_offset = location.offset - (location.column - 1);
    var end_offset = start_offset + 1;

    while (end_offset < self.file_contents.len and self.file_contents[end_offset] != '\n') : (end_offset += 1) {}

    return self.file_contents[start_offset..end_offset];
}

/// Return whether the lexer has reached the end of the file.
fn eof(self: *Self) bool {
    // Note that we only consider it truly EOF if:
    // 1) We are at the end of the FixedSizeBuffer;
    // 2) We have exhausted the pushed back bytes.
    return (self.buffer.getPos() catch return true) == (self.buffer.getEndPos() catch return true) and self.stream.fifo.count == 0;
}

/// Read a byte. If EOF is reached return null.
fn readByte(self: *Self) !?u8 {
    const byte = self.reader.readByte() catch |err| switch (err) {
        error.EndOfStream => return @as(?u8, null),
        else => return err,
    };
    return byte;
}

/// Skip any immediately available whitespace.
fn skipWhitespace(self: *Self) !void {
    var last_byte: ?u8 = null;

    while (try self.readByte()) |c| {
        if (!(c == '\n' or c == '\t' or c == ' ' or c == '\r')) {
            last_byte = c;
            break;
        }

        self.consumed_whitespace += 1;
        self.token_end.advanceForCharacter(c);
    }
    // We will have read one extra byte at this point.
    if (last_byte) |c| {
        try self.stream.putBackByte(c);
    }
}

/// Lex a comment, if possible.
fn lexComment(self: *Self) !?tokens.Token {
    var first_byte: u8 = (try self.readByte()) orelse return null;
    if (first_byte != '"') {
        try self.stream.putBackByte(first_byte);
        return null;
    }
    self.token_end.advanceForCharacter(first_byte);

    var did_escape = false;
    var did_finish_comment = false;
    while (try self.readByte()) |c| {
        self.token_end.advanceForCharacter(c);

        if (c == '\"') {
            if (!did_escape) {
                did_finish_comment = true;
                break;
            }
            did_escape = false;
        } else if (c == '\\') {
            did_escape = !did_escape;
        } else {
            did_escape = false;
        }
    }

    if (!did_finish_comment)
        return error.UnterminatedComment;

    return tokens.Token{ .Comment = .{} };
}

/// Lex a string, if possible.
/// The allocated string will be owned by the caller.
fn lexString(self: *Self) !?tokens.Token {
    var first_byte: u8 = (try self.readByte()) orelse return null;
    if (first_byte != '\'') {
        try self.stream.putBackByte(first_byte);
        return null;
    }
    self.token_end.advanceForCharacter(first_byte);

    var string_builder = std.ArrayList(u8).init(self.allocator);
    defer string_builder.deinit();

    var did_escape = false;
    var did_finish_string = false;
    outer: while (try self.readByte()) |c| {
        if (c == '\n' or c == '\r') {
            return error.NewlineInStringLiteral;
        }

        self.token_end.advanceForCharacter(c);

        // TODO: Handle more complicated escapes, like \xNN and \uNNNN.
        if (did_escape) {
            switch (c) {
                'n' => try string_builder.append('\n'),
                'r' => try string_builder.append('\r'),
                't' => try string_builder.append('\t'),
                '\'' => try string_builder.append('\''),
                '\\' => try string_builder.append('\\'),
                else => return error.UnknownEscape,
            }
            did_escape = false;
        } else {
            switch (c) {
                '\\' => did_escape = true,
                '\'' => {
                    did_finish_string = true;
                    break :outer;
                },
                else => try string_builder.append(c),
            }
        }
    }

    if (!did_finish_string)
        return error.UnterminatedStringLiteral;

    return tokens.Token{ .String = string_builder.toOwnedSlice() };
}

// NOTE: Only Decimal can have a fraction.
const NumberParserState = enum { Initial, MaybeRepresentation, Binary, Hexadecimal, Decimal, Fraction };

/// Lex a number (integer or floating-point), if possible.
fn lexNumber(self: *Self) !?tokens.Token {
    var first_byte: u8 = (try self.readByte()) orelse return null;
    try self.stream.putBackByte(first_byte);
    if (!std.ascii.isDigit(first_byte)) {
        return null;
    }

    var parser_state: NumberParserState = .Initial;
    var integer: i64 = 0;
    var floating_point: f64 = 0.0;
    var fraction_counter: usize = 0;
    var did_finish_number = false;
    var last_byte: u8 = undefined;
    outer: while (try self.readByte()) |c| {
        last_byte = c;

        switch (parser_state) {
            .Initial => {
                if (c == '0') {
                    // If we are starting with 0, then we either follow it up
                    // with a representation type or a non-digit character.
                    // (No octals in Self).
                    parser_state = .MaybeRepresentation;
                } else {
                    // This is just a regular decimal number which we can switch
                    // to floating point later if we want to.
                    parser_state = .Decimal;
                    integer += c - '0';
                }
            },
            .MaybeRepresentation => {
                if (c == 'x' or c == 'X') {
                    parser_state = .Hexadecimal;
                } else if (c == 'b' or c == 'B') {
                    parser_state = .Binary;
                } else if (std.ascii.isDigit(c)) {
                    return error.InvalidDigitAfterZero;
                } else if (c == '.') {
                    // Either statement terminator or floating point.
                    // The '0.' syntax for floating point values is not allowed,
                    // as it is too ambiguous.
                    const next_byte = try self.readByte();
                    if (next_byte != null) {
                        try self.stream.putBackByte(next_byte.?);
                    }

                    if (next_byte != null and std.ascii.isDigit(next_byte.?)) {
                        parser_state = .Fraction;
                    } else {
                        parser_state = .Decimal;
                        did_finish_number = true;
                        break :outer;
                    }
                } else {
                    // The user simply entered "0".
                    parser_state = .Decimal;
                    did_finish_number = true;
                    break :outer;
                }
            },
            .Binary => {
                if (c != '0' and c != '1') {
                    did_finish_number = true;
                    break :outer;
                }

                if (@shlWithOverflow(i64, integer, 1, &integer)) {
                    return error.NumberLiteralTooLarge;
                }

                if (c == '1') {
                    integer += 1;
                }
            },
            .Hexadecimal => {
                if (!std.ascii.isXDigit(c)) {
                    did_finish_number = true;
                    break :outer;
                }

                if (@shlWithOverflow(i64, integer, 4, &integer)) {
                    return error.NumberLiteralTooLarge;
                }

                if (std.ascii.isDigit(c)) {
                    integer += c - '0';
                } else {
                    integer += std.ascii.toUpper(c) - 'A';
                }
            },
            .Decimal => {
                if (c == '.') {
                    // Either statement terminator or floating point.
                    const next_byte = try self.readByte();
                    if (next_byte != null) {
                        try self.stream.putBackByte(next_byte.?);
                    }

                    if (next_byte != null and std.ascii.isDigit(next_byte.?)) {
                        floating_point = @intToFloat(f64, integer);
                        integer = 0;
                        parser_state = .Fraction;
                    } else {
                        did_finish_number = true;
                        break :outer;
                    }
                } else if (std.ascii.isDigit(c)) {
                    if (@mulWithOverflow(i64, integer, 10, &integer)) {
                        return error.NumberLiteralTooLarge;
                    }

                    if (@addWithOverflow(i64, integer, c - '0', &integer)) {
                        return error.NumberLiteralTooLarge;
                    }
                } else {
                    did_finish_number = true;
                    break :outer;
                }
            },
            .Fraction => {
                // For the fraction, we store the fraction value in the integer
                // part until the final digit is reached, after which we add
                // it to the total floating point value.
                if (!std.ascii.isDigit(c)) {
                    var divisor = std.math.pow(f64, 10.0, @intToFloat(f64, fraction_counter));
                    var fraction_value = @intToFloat(f64, integer) / divisor;
                    floating_point += fraction_value;

                    did_finish_number = true;
                    break :outer;
                } else {
                    if (fraction_counter == 9) {
                        return error.FloatingPointFractionTooLong;
                    }

                    integer = (integer * 10) + (c - '0');
                    fraction_counter += 1;
                }
            },
        }

        self.token_end.advanceForCharacter(c);
    }

    if (did_finish_number) {
        // If the number wasn't the last token in the stream, then we need to put
        // the current character back.
        try self.stream.putBackByte(last_byte);
    }

    return switch (parser_state) {
        .Decimal, .Binary, .Hexadecimal => tokens.Token{ .Integer = integer },
        .Fraction => tokens.Token{ .FloatingPoint = floating_point },
        else => unreachable,
    };
}

/// Lex an identifier, if possible.
fn lexIdentifier(self: *Self) !?tokens.Token {
    var first_byte: u8 = (try self.readByte()) orelse return null;
    if (!std.ascii.isAlpha(first_byte) and first_byte != '_') {
        try self.stream.putBackByte(first_byte);
        return null;
    }
    self.token_end.advanceForCharacter(first_byte);

    var token = tokens.Token{ .Identifier = undefined };
    var offset: usize = 0;

    token.Identifier[offset] = first_byte;
    offset += 1;

    var did_finish_identifier = false;
    var last_byte: u8 = undefined;
    while (try self.readByte()) |c| {
        last_byte = c;

        if (!std.ascii.isAlNum(c) and c != '_') {
            did_finish_identifier = true;
            break;
        }

        if (offset == tokens.MaximumIdentifierLength) {
            return error.MaximumIdentifierLengthExceeded;
        }

        token.Identifier[offset] = c;
        offset += 1;
        self.token_end.advanceForCharacter(c);
    }

    if (did_finish_identifier) {
        try self.stream.putBackByte(last_byte);
    }

    token.Identifier[offset] = 0;
    return token;
}

/// Lex a symbol, if possible.
fn lexSymbol(self: *Self) !?tokens.Token {
    var buffer: [2]u8 = undefined;

    const tokensInfo = @typeInfo(tokens.Token);
    inline for (tokensInfo.Union.fields) |field| {
        if (@hasDecl(tokens.TokenRepresentation, field.name)) {
            const representation = @field(tokens.TokenRepresentation, field.name);

            var slice = buffer[0..representation.len];
            var nread = try self.reader.read(slice);

            // NOTE: Have to branch it like this because otherwise Zig complains.
            if (nread < representation.len) {
                try self.stream.putBack(slice);
            } else {
                if (std.mem.eql(u8, representation, slice)) {
                    self.token_end.advanceNColumns(representation.len);

                    return @unionInit(tokens.Token, field.name, .{});
                }

                try self.stream.putBack(slice);
            }
        }
    }

    return null;
}

// Zig's RLS is currently broken. :^(
initialized: bool = false,
allocator: *std.mem.Allocator = undefined,
file_contents: []const u8 = undefined,
buffer: BufferType = undefined,
stream: PeekStreamType = undefined,
reader: PeekStreamType.Reader = undefined,
current_token: tokens.Token = undefined,

token_start: Location = .{},
token_end: Location = .{},
consumed_whitespace: usize = 0,
