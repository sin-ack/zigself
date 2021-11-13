const std = @import("std");
const Allocator = std.mem.Allocator;

// FIXME: Move this to another file, or use a library
const DARKGRAY = "\x1b[90m";
const GRAY = "\x1b[37m";
const GREEN = "\x1b[32m";
const ORANGE = "\x1b[33m";
const MAGENTA = "\x1b[35m";
const CYAN = "\x1b[36m";
const CLEAR = "\x1b[0m";

pub const ASTPrinter = struct {
    indent_width: usize,

    branches: std.ArrayList(Branch),
    current_indent: usize = 0,
    is_stem: bool = false,

    const Branch = struct { indent: usize, concluded: bool };

    pub fn init(indent_width: usize, allocator: *Allocator) ASTPrinter {
        return .{
            .indent_width = indent_width,
            .branches = std.ArrayList(Branch).init(allocator),
        };
    }

    const StemIsLast = enum { Last, NotLast };
    pub fn setStem(self: *ASTPrinter, is_last: StemIsLast) void {
        self.is_stem = true;
        if (self.branches.items.len > 0) {
            self.branches.items[self.branches.items.len - 1].concluded = is_last == .Last;
        }
    }

    pub fn indent(self: *ASTPrinter) void {
        self.branches.append(.{ .indent = self.current_indent, .concluded = false }) catch unreachable;
        self.current_indent += self.indent_width;
    }

    pub fn dedent(self: *ASTPrinter) void {
        _ = self.branches.pop();
        self.current_indent -= self.indent_width;
    }

    pub fn print(self: *ASTPrinter, comptime fmt: []const u8, args: anytype) void {
        const writer = std.io.getStdErr().writer();
        writer.writeAll(DARKGRAY) catch return;

        var last_indent: usize = 0;
        for (self.branches.items) |branch, i| {
            if (i + 1 == self.branches.items.len) {
                if (self.is_stem) {
                    writer.writeAll(if (branch.concluded) "└" else "├") catch return;
                } else {
                    writer.writeAll("│") catch return;
                }

                var stem_i: usize = 0;
                while (stem_i < self.current_indent - branch.indent - 2) : (stem_i += 1) {
                    writer.writeAll(if (self.is_stem) "─" else " ") catch return;
                }
                writer.writeAll("╴") catch return;
            } else {
                writer.writeAll(if (!branch.concluded) "│" else " ") catch return;
                writer.writeByteNTimes(' ', self.indent_width - 1) catch return;
            }

            last_indent = branch.indent;
        }

        writer.writeAll(CLEAR) catch return;
        std.debug.print(fmt, args);
    }

    pub fn deinit(self: *ASTPrinter) void {
        self.branches.deinit();
    }
};

/// A node which describes a single script.
pub const ScriptNode = struct {
    statements: []StatementNode,

    pub fn deinit(self: *ScriptNode, allocator: *Allocator) void {
        for (self.statements) |*statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
    }

    pub fn dumpTree(self: ScriptNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "ScriptNode\n" ++ CLEAR, .{});

        printer.indent();
        for (self.statements) |statement, i| {
            printer.setStem(if (i == self.statements.len - 1) .Last else .NotLast);
            statement.dumpTree(printer);
        }
        printer.dedent();
    }
};

/// A single statement.
pub const StatementNode = struct {
    expression: ExpressionNode,

    pub fn deinit(self: *StatementNode, allocator: *Allocator) void {
        self.expression.deinit(allocator);
    }

    pub fn dumpTree(self: StatementNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "StatementNode\n" ++ CLEAR, .{});

        printer.indent();
        printer.setStem(.Last);
        self.expression.dumpTree(printer);
        printer.dedent();
    }
};

pub const ExpressionNode = union(enum) {
    Object: *ObjectNode,
    Block: *BlockNode,
    Message: *MessageNode,

    Identifier: IdentifierNode,
    String: StringNode,
    Number: NumberNode,

    pub fn deinit(self: *ExpressionNode, allocator: *Allocator) void {
        switch (self.*) {
            .Object => self.Object.destroy(allocator),
            .Block => self.Block.destroy(allocator),
            .Message => self.Message.destroy(allocator),

            .Identifier => self.Identifier.deinit(allocator),
            .String => self.String.deinit(allocator),

            else => {},
        }
    }

    pub fn dumpTree(self: ExpressionNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "ExpressionNode\n" ++ CLEAR, .{});

        printer.indent();
        printer.setStem(.Last);
        switch (self) {
            .Object => self.Object.dumpTree(printer),
            .Block => self.Block.dumpTree(printer),
            .Message => self.Message.dumpTree(printer),

            .Identifier => self.Identifier.dumpTree(printer),
            .String => self.String.dumpTree(printer),
            .Number => self.Number.dumpTree(printer),
        }
        printer.dedent();
    }
};

pub const ObjectNode = struct {
    slots: []SlotNode,
    statements: []StatementNode,

    pub fn deinit(self: *ObjectNode, allocator: *Allocator) void {
        for (self.slots) |*slot| {
            slot.deinit(allocator);
        }
        allocator.free(self.slots);

        for (self.statements) |*statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
    }

    pub fn destroy(self: *ObjectNode, allocator: *Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn dumpTree(self: ObjectNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "ObjectNode\n" ++ CLEAR, .{});
        printer.indent();

        printer.setStem(.NotLast);
        printer.print("slots:\n", .{});
        printer.indent();
        for (self.slots) |slot, i| {
            printer.setStem(if (i == self.slots.len - 1) .Last else .NotLast);
            slot.dumpTree(printer);
        }
        printer.dedent();

        printer.setStem(.Last);
        printer.print("statements:\n", .{});
        printer.indent();
        for (self.statements) |statement, i| {
            printer.setStem(if (i == self.statements.len - 1) .Last else .NotLast);
            statement.dumpTree(printer);
        }
        printer.dedent();

        printer.dedent();
    }
};

pub const SlotNode = struct {
    is_mutable: bool,
    is_parent: bool,
    is_argument: bool,

    name: []const u8,
    // NOTE: The arguments will be injected into the object during evaluation.
    arguments: [][]const u8,
    value: ExpressionNode,

    pub fn deinit(self: *SlotNode, allocator: *Allocator) void {
        allocator.free(self.name);

        for (self.arguments) |argument| {
            allocator.free(argument);
        }
        allocator.free(self.arguments);

        self.value.deinit(allocator);
    }

    pub fn dumpTree(self: SlotNode, printer: *ASTPrinter) void {
        const is_mutable_string: []const u8 = if (self.is_mutable) GREEN ++ "mutable" ++ CLEAR else GRAY ++ "not mutable" ++ CLEAR;
        const is_parent_string: []const u8 = if (self.is_parent) GREEN ++ "parent" ++ CLEAR else GRAY ++ "not parent" ++ CLEAR;
        const is_argument_string: []const u8 = if (self.is_argument) GREEN ++ "argument" ++ CLEAR else GRAY ++ "not argument" ++ CLEAR;
        printer.print(
            CYAN ++ "SlotNode" ++ CLEAR ++ " ({s}, {s}, {s})\n",
            .{ is_mutable_string, is_parent_string, is_argument_string },
        );
        printer.indent();

        printer.setStem(.NotLast);
        printer.print("name: " ++ GREEN ++ "\"{s}\"\n" ++ CLEAR, .{self.name});
        printer.setStem(.NotLast);
        printer.print("arguments: " ++ GREEN, .{});
        for (self.arguments) |argument, i| {
            if (i != 0) std.debug.print(", ", .{});
            std.debug.print("\"{s}\"", .{argument});
        }
        std.debug.print("\n" ++ CLEAR, .{});

        printer.setStem(.Last);
        printer.print("value:\n", .{});
        printer.indent();
        printer.setStem(.Last);
        self.value.dumpTree(printer);
        printer.dedent();

        printer.dedent();
    }
};

pub const BlockNode = struct {
    slots: []SlotNode,
    statements: []StatementNode,

    pub fn deinit(self: *BlockNode, allocator: *Allocator) void {
        for (self.slots) |*slot| {
            slot.deinit(allocator);
        }
        allocator.free(self.slots);

        for (self.statements) |*statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
    }

    pub fn destroy(self: *BlockNode, allocator: *Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn dumpTree(self: BlockNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "BlockNode\n" ++ CLEAR, .{});
        printer.indent();

        printer.setStem(.NotLast);
        printer.print("slots:\n", .{});
        printer.indent();
        for (self.slots) |slot, i| {
            printer.setStem(if (i == self.slots.len - 1) .Last else .NotLast);
            slot.dumpTree(printer);
        }
        printer.dedent();

        printer.setStem(.Last);
        printer.print("statements:\n", .{});
        printer.indent();
        for (self.statements) |statement, i| {
            printer.setStem(if (i == self.statements.len - 1) .Last else .NotLast);
            statement.dumpTree(printer);
        }
        printer.dedent();

        printer.dedent();
    }
};

pub const IdentifierNode = struct {
    value: []u8,

    pub fn deinit(self: *IdentifierNode, allocator: *Allocator) void {
        allocator.free(self.value);
    }

    pub fn dumpTree(self: IdentifierNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "IdentifierNode " ++ GREEN ++ "\"{s}\"\n" ++ CLEAR, .{self.value});
    }
};

pub const MessageNode = struct {
    receiver: ExpressionNode,
    message_name: []const u8,
    arguments: []ExpressionNode,

    pub fn deinit(self: *MessageNode, allocator: *Allocator) void {
        self.receiver.deinit(allocator);
        allocator.free(self.message_name);

        for (self.arguments) |*argument| {
            argument.deinit(allocator);
        }
        allocator.free(self.arguments);
    }

    pub fn destroy(self: *MessageNode, allocator: *Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn dumpTree(self: MessageNode, printer: *ASTPrinter) void {
        const message_type: []const u8 = blk: {
            if (self.arguments.len == 0) {
                break :blk GREEN ++ "unary" ++ CLEAR;
            } else if (self.arguments.len == 1 and !std.ascii.isAlpha(self.message_name[0]) and self.message_name[0] != '_') {
                break :blk MAGENTA ++ "binary" ++ CLEAR;
            } else {
                break :blk ORANGE ++ "keyword" ++ CLEAR;
            }
        };

        printer.print(CYAN ++ "MessageNode" ++ CLEAR ++ " ({s})\n", .{message_type});
        printer.indent();

        printer.setStem(.NotLast);
        printer.print("receiver:\n", .{});
        printer.indent();
        printer.setStem(.Last);
        self.receiver.dumpTree(printer);
        printer.dedent();

        printer.setStem(if (self.arguments.len == 0) .Last else .NotLast);
        printer.print("name: " ++ GREEN ++ "\"{s}\"\n" ++ CLEAR, .{self.message_name});

        if (self.arguments.len > 0) {
            printer.setStem(.Last);
            printer.print("arguments:\n", .{});
            printer.indent();
            for (self.arguments) |argument, i| {
                printer.setStem(if (i == self.arguments.len - 1) .Last else .NotLast);
                argument.dumpTree(printer);
            }
            printer.dedent();
        }

        printer.dedent();
    }
};

pub const StringNode = struct {
    value: []const u8,

    pub fn deinit(self: *StringNode, allocator: *Allocator) void {
        allocator.free(self.value);
    }

    pub fn dumpTree(self: StringNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "StringNode" ++ CLEAR ++ " (" ++ GREEN ++ "{}" ++ CLEAR ++ " bytes)\n", .{self.value.len});
        printer.indent();

        printer.setStem(.Last);
        printer.print("content: \"{s}", .{self.value[0..std.math.min(200, self.value.len)]});
        if (self.value.len > 200) {
            std.debug.print("...", .{});
        }
        std.debug.print("\"\n", .{});

        printer.dedent();
    }
};

pub const NumberNode = union(enum) {
    Integer: u64,
    FloatingPoint: f64,

    pub fn dumpTree(self: NumberNode, printer: *ASTPrinter) void {
        printer.print(CYAN ++ "NumberNode " ++ CLEAR, .{});
        switch (self) {
            .Integer => std.debug.print("{}\n", .{self.Integer}),
            .FloatingPoint => std.debug.print("{}\n", .{self.FloatingPoint}),
        }
    }
};
