const std = @import("std");

/// The maximum length of a single identifier. If your identifiers are longer
/// than this, you might have made a mistake somewhere. :^)
pub const MaximumIdentifierLength = 128;

/// This struct stores the string representations for all tokens that have them.
/// If a string representation does not exist for a token, then it is a token
/// which can have many representations, like strings or numbers.
pub const TokenRepresentation = struct {
    /// Used to signify a mutable slot.
    pub const Arrow = "<-";

    // These are all the symbols that can be part of a binary message name.
    pub const Bang = "!";
    pub const Sigil = "@";
    pub const Hash = "#";
    pub const Dollar = "$";
    pub const Percent = "%";
    pub const Cap = "^";
    pub const Ampersand = "&";
    pub const Asterisk = "*";
    pub const Comma = ",";
    pub const Semicolon = ";";
    pub const Slash = "/";
    pub const Backslash = "\\";
    pub const LessThan = "<";
    pub const GreaterThan = ">";
    pub const Equals = "=";
    pub const Plus = "+";
    pub const Minus = "-";
    pub const QuestionMark = "?";
    pub const Backtick = "`";
    pub const Tilde = "~";

    /// Used in keyword messages.
    pub const Colon = ":";

    /// Statement terminator, or floating point number.
    pub const Period = ".";

    /// Slot list delimiter.
    pub const Pipe = "|";

    // These symbols delimit objects, blocks and annotations respectively.
    pub const ParenOpen = "(";
    pub const ParenClose = ")";
    pub const BracketOpen = "[";
    pub const BracketClose = "]";
    pub const BraceOpen = "{";
    pub const BraceClose = "}";
};

/// All the tokens in the self language. The fields which are marked "void" have
/// a representation in TokenRepresentation.
pub const Token = union(enum) {
    Arrow: void,
    Bang: void,
    Sigil: void,
    Hash: void,
    Dollar: void,
    Percent: void,
    Cap: void,
    Ampersand: void,
    Asterisk: void,
    Comma: void,
    Semicolon: void,
    Slash: void,
    Backslash: void,
    LessThan: void,
    GreaterThan: void,
    Equals: void,
    Plus: void,
    Minus: void,
    QuestionMark: void,
    Backtick: void,
    Tilde: void,
    Colon: void,
    Period: void,
    Pipe: void,
    ParenOpen: void,
    ParenClose: void,
    BracketOpen: void,
    BracketClose: void,
    BraceOpen: void,
    BraceClose: void,
    EOF: void,

    // TODO: Will we ever need to store comments?
    Comment: void,

    String: []const u8,
    Identifier: [MaximumIdentifierLength:0]u8,
    Integer: u64,
    FloatingPoint: f64,

    pub fn toString(self: Token) []const u8 {
        return switch (self) {
            .String => "<string>",
            .Identifier => "<identifier>",
            .Integer => "<integer>",
            .FloatingPoint => "<floating point>",
            .EOF => "<end of file>",
            .Comment => "<comment>",
            else => {
                // FIXME: This could probably be improved by somehow generating
                //        a switch. Although maybe the compiler is sufficiently
                //        smart to do that.
                //        I'd like to use @hasField here, but name is not
                //        comptime.
                const name = @tagName(self);
                inline for (@typeInfo(TokenRepresentation).Struct.decls) |decl| {
                    if (std.mem.eql(u8, name, decl.name)) {
                        return @field(TokenRepresentation, decl.name);
                    }
                }

                @panic("Unknown token");
            },
        };
    }
};
