// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Token = @import("./Token.zig");

buffer: [:0]const u8,
offset: usize = 0,

const Self = @This();

const State = enum {
    Start,
    Identifier,
    PrimitiveStart,
    RestKeyword,
    Comment,
    StringLiteral,
    StringLiteralBackslash,
    Zero,
    DecimalOrFloatingPoint,
    Hexadecimal,
    Octal,
    FractionOrPeriod,
    Fraction,
    LessThan,
    Cap,
    Pipe,
};

pub fn init(buffer: [:0]const u8) Self {
    return .{ .buffer = buffer };
}

pub fn next(self: *Self) Token {
    var state = State.Start;
    var token = Token{
        .tag = .EOF,
        .location = .{
            .start = self.offset,
            .end = undefined,
        },
    };

    while (true) : (self.offset += 1) {
        const c = self.buffer[self.offset];
        switch (state) {
            .Start => switch (c) {
                0 => break,
                '\n', '\r', '\t', ' ' => {
                    token.location.start = self.offset + 1;
                },

                'a'...'z' => {
                    state = .Identifier;
                    token.tag = .Identifier;
                },
                '_' => {
                    state = .PrimitiveStart;
                    token.tag = .Identifier;
                },
                'A'...'Z' => {
                    state = .RestKeyword;
                    token.tag = .RestKeyword;
                },

                '"' => {
                    state = .Comment;
                },
                '\'' => {
                    state = .StringLiteral;
                    token.tag = .String;
                },
                '0' => {
                    state = .Zero;
                    token.tag = .Integer;
                },
                '1'...'9' => {
                    state = .DecimalOrFloatingPoint;
                    token.tag = .Integer;
                },

                '(' => {
                    token.tag = .ParenOpen;
                    self.offset += 1;
                    break;
                },
                ')' => {
                    token.tag = .ParenClose;
                    self.offset += 1;
                    break;
                },
                '[' => {
                    token.tag = .BracketOpen;
                    self.offset += 1;
                    break;
                },
                ']' => {
                    token.tag = .BracketClose;
                    self.offset += 1;
                    break;
                },
                '{' => {
                    token.tag = .BraceOpen;
                    self.offset += 1;
                    break;
                },
                '}' => {
                    token.tag = .BraceClose;
                    self.offset += 1;
                    break;
                },

                '!' => {
                    token.tag = .Bang;
                    self.offset += 1;
                    break;
                },
                '@' => {
                    token.tag = .Sigil;
                    self.offset += 1;
                    break;
                },
                '#' => {
                    token.tag = .Hash;
                    self.offset += 1;
                    break;
                },
                '$' => {
                    token.tag = .Dollar;
                    self.offset += 1;
                    break;
                },
                '%' => {
                    token.tag = .Percent;
                    self.offset += 1;
                    break;
                },
                '&' => {
                    token.tag = .Ampersand;
                    self.offset += 1;
                    break;
                },
                '*' => {
                    token.tag = .Asterisk;
                    self.offset += 1;
                    break;
                },
                ',' => {
                    token.tag = .Comma;
                    self.offset += 1;
                    break;
                },
                '/' => {
                    token.tag = .Slash;
                    self.offset += 1;
                    break;
                },
                '\\' => {
                    token.tag = .Backslash;
                    self.offset += 1;
                    break;
                },
                '>' => {
                    token.tag = .GreaterThan;
                    self.offset += 1;
                    break;
                },
                '=' => {
                    token.tag = .Equals;
                    self.offset += 1;
                    break;
                },
                '+' => {
                    token.tag = .Plus;
                    self.offset += 1;
                    break;
                },
                '-' => {
                    token.tag = .Minus;
                    self.offset += 1;
                    break;
                },
                '?' => {
                    token.tag = .QuestionMark;
                    self.offset += 1;
                    break;
                },
                '`' => {
                    token.tag = .Backtick;
                    self.offset += 1;
                    break;
                },
                '~' => {
                    token.tag = .Tilde;
                    self.offset += 1;
                    break;
                },
                ':' => {
                    token.tag = .Colon;
                    self.offset += 1;
                    break;
                },
                ';' => {
                    token.tag = .Semicolon;
                    self.offset += 1;
                    break;
                },
                '.' => {
                    token.tag = .Period;
                    self.offset += 1;
                    break;
                },

                '<' => {
                    state = .LessThan;
                },
                '^' => {
                    state = .Cap;
                },
                '|' => {
                    state = .Pipe;
                },

                else => {
                    token.tag = .Invalid;
                    break;
                },
            },
            .PrimitiveStart => switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                    state = .Identifier;
                },
                else => {
                    token.tag = .Invalid;
                    break;
                },
            },
            .Identifier => switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                ':' => {
                    token.tag = .FirstKeyword;
                    state = .Start;
                    self.offset += 1;
                    break;
                },
                else => {
                    state = .Start;
                    break;
                },
            },
            .RestKeyword => switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                ':' => {
                    state = .Start;
                    self.offset += 1;
                    break;
                },
                else => {
                    token.tag = .Invalid;
                    break;
                },
            },
            .Comment => switch (c) {
                '"' => {
                    state = .Start;
                },
                else => {},
            },
            .StringLiteral => switch (c) {
                '\'' => {
                    self.offset += 1;
                    break;
                },
                '\n' => {
                    token.tag = .Invalid;
                    break;
                },
                '\\' => {
                    state = .StringLiteralBackslash;
                },
                else => {},
            },
            .StringLiteralBackslash => switch (c) {
                'n', 'r', 't', '\\', '\'' => {
                    state = .StringLiteral;
                },
                'x' => {
                    self.offset += 1;
                    if (!std.ascii.isXDigit(self.buffer[self.offset])) {
                        token.tag = .Invalid;
                        break;
                    }

                    self.offset += 1;
                    if (!std.ascii.isXDigit(self.buffer[self.offset])) {
                        token.tag = .Invalid;
                        break;
                    }

                    self.offset += 1;
                    state = .StringLiteral;
                },
                else => {
                    token.tag = .Invalid;
                    break;
                },
            },
            .Zero => switch (c) {
                'x', 'X' => {
                    state = .Hexadecimal;
                },
                'o', 'O' => {
                    state = .Octal;
                },
                // It is invalid to have a number like 0123.
                '0'...'9' => {
                    token.tag = .Invalid;
                    break;
                },
                '.' => {
                    state = .FractionOrPeriod;
                    token.tag = .FloatingPoint;
                },
                else => {
                    token.tag = .Integer;
                    break;
                },
            },
            .DecimalOrFloatingPoint => switch (c) {
                '.' => {
                    state = .FractionOrPeriod;
                    token.tag = .FloatingPoint;
                },
                '0'...'9' => {},
                else => break,
            },
            .LessThan => switch (c) {
                '-' => {
                    token.tag = .Arrow;
                    self.offset += 1;
                    break;
                },
                else => {
                    token.tag = .LessThan;
                    break;
                },
            },
            .Cap => switch (c) {
                '^' => {
                    token.tag = .DoubleCap;
                    self.offset += 1;
                    break;
                },
                else => {
                    token.tag = .Cap;
                    break;
                },
            },
            .Pipe => switch (c) {
                '|' => {
                    token.tag = .DoublePipe;
                    self.offset += 1;
                    break;
                },
                else => {
                    token.tag = .Pipe;
                    break;
                },
            },
            .Hexadecimal => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {},
                else => break,
            },
            .Octal => switch (c) {
                '0'...'7' => {},
                '8', '9' => {
                    token.tag = .Invalid;
                    break;
                },
                else => break,
            },
            .FractionOrPeriod => switch (c) {
                '0'...'9' => {
                    state = .Fraction;
                },
                else => {
                    token.tag = .Integer;
                    self.offset -= 1;
                    break;
                },
            },
            .Fraction => switch (c) {
                '0'...'9' => {},
                else => break,
            },
        }
    }

    token.location.end = self.offset;
    return token;
}
