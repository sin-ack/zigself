// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! A single token.

tag: Tag,
location: Location,

pub const Location = struct {
    start: usize,
    end: usize,
};

pub const Tag = enum {
    Invalid,
    EOF,

    String,
    Identifier,
    FirstKeyword,
    RestKeyword,
    Integer,
    FloatingPoint,

    Bang,
    Sigil,
    Hash,
    Dollar,
    Percent,
    Ampersand,
    Asterisk,
    Comma,
    Slash,
    Backslash,
    LessThan,
    GreaterThan,
    Equals,
    Plus,
    Minus,
    QuestionMark,
    Backtick,
    Tilde,
    Colon,
    Cap,
    Semicolon,
    Period,
    Pipe,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,

    Arrow,
    DoublePipe,
    DoubleCap,

    pub fn symbol(self: Tag) []const u8 {
        return switch (self) {
            .Invalid => "invalid character",
            .EOF => "end of file",
            .String => "string",
            .Identifier => "identifier",
            .FirstKeyword => "first-keyword",
            .RestKeyword => "rest-keyword",
            .Integer => "integer",
            .FloatingPoint => "floating-point number",

            .ParenOpen => "(",
            .ParenClose => ")",
            .BracketOpen => "[",
            .BracketClose => "]",
            .BraceOpen => "{",
            .BraceClose => "}",

            .Bang => "!",
            .Sigil => "@",
            .Hash => "#",
            .Dollar => "$",
            .Percent => "%",
            .Cap => "^",
            .Ampersand => "&",
            .Asterisk => "*",
            .Comma => ",",
            .Slash => "/",
            .Backslash => "\\",
            .LessThan => "<",
            .GreaterThan => ">",
            .Equals => "=",
            .Plus => "+",
            .Minus => "-",
            .QuestionMark => "?",
            .Backtick => "`",
            .Tilde => "~",
            .Colon => ":",
            .Semicolon => ";",
            .Period => ".",
            .Pipe => "|",

            .Arrow => "<-",
            .DoublePipe => "||",
            .DoubleCap => "^^",
        };
    }

    /// Return whether this token can be part of an operator.
    pub fn isOperator(self: Tag) bool {
        return switch (self) {
            .Bang,
            .Sigil,
            .Hash,
            .Dollar,
            .Percent,
            .Ampersand,
            .Asterisk,
            .Comma,
            .Slash,
            .Backslash,
            .LessThan,
            .GreaterThan,
            .Equals,
            .Plus,
            .Minus,
            .QuestionMark,
            .Backtick,
            .Tilde,
            .DoublePipe,
            .DoubleCap,
            => true,

            else => false,
        };
    }
};
