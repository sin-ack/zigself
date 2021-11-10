const std = @import("std");
const Allocator = std.mem.Allocator;

fn printWithIndent(indent: usize, comptime fmt: []const u8, args: anytype) void {
    std.io.getStdErr().writer().writeByteNTimes(' ', indent) catch return;
    std.debug.print(fmt, args);
}

/// A node which describes a single script.
pub const ScriptNode = struct {
    statements: []StatementNode,

    pub fn deinit(self: *ScriptNode, allocator: *Allocator) void {
        for (self.statements) |*statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
    }

    pub fn dumpTree(self: ScriptNode, indent: usize) void {
        printWithIndent(indent, "ScriptNode\n", .{});
        for (self.statements) |statement| {
            statement.dumpTree(indent + 2);
        }
    }
};

/// A single statement.
pub const StatementNode = struct {
    expression: ExpressionNode,

    pub fn deinit(self: *StatementNode, allocator: *Allocator) void {
        self.expression.deinit(allocator);
    }

    pub fn dumpTree(self: StatementNode, indent: usize) void {
        printWithIndent(indent, "StatementNode\n", .{});
        self.expression.dumpTree(indent + 2);
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
            .Object => self.Object.deinit(allocator),
            .Block => self.Block.deinit(allocator),
            .Message => self.Message.deinit(allocator),

            .Identifier => self.Identifier.deinit(allocator),
            .String => self.String.deinit(allocator),

            else => {},
        }
    }

    pub fn dumpTree(self: ExpressionNode, indent: usize) void {
        printWithIndent(indent, "ExpressionNode\n", .{});
        switch (self) {
            .Object => self.Object.dumpTree(indent + 2),
            .Block => self.Block.dumpTree(indent + 2),
            .Message => self.Message.dumpTree(indent + 2),

            .Identifier => self.Identifier.dumpTree(indent + 2),
            .String => self.String.dumpTree(indent + 2),
            .Number => self.String.dumpTree(indent + 2),
        }
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

    pub fn dumpTree(self: ObjectNode, indent: usize) void {
        printWithIndent(indent, "ObjectNode\n", .{});

        printWithIndent(indent + 2, "slots:\n", .{});
        for (self.slots) |slot| {
            slot.dumpTree(indent + 4);
        }

        printWithIndent(indent + 4, "statements:\n", .{});
        for (self.statements) |statement| {
            statement.dumpTree(indent + 4);
        }
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

    pub fn dumpTree(self: SlotNode, indent: usize) void {
        const is_mutable_string: []const u8 = if (self.is_mutable) "mutable" else "not mutable";
        const is_parent_string: []const u8 = if (self.is_parent) "parent" else "not parent";
        const is_argument_string: []const u8 = if (self.is_argument) "argument" else "not argument";
        printWithIndent(indent, "SlotNode ({s}, {s}, {s})\n", .{ is_mutable_string, is_parent_string, is_argument_string });

        printWithIndent(indent + 2, "name: \"{s}\"\n", .{self.name});
        printWithIndent(indent + 2, "arguments: ", .{});
        for (self.arguments) |argument, i| {
            if (i != 0) std.debug.print(", ", .{});
            std.debug.print("\"{s}\"", .{argument});
        }
        std.debug.print("\n", .{});

        printWithIndent(indent + 2, "value:\n", .{});
        self.value.dumpTree(indent + 4);
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

    pub fn dumpTree(self: BlockNode, indent: usize) void {
        printWithIndent(indent, "BlockNode\n", .{});

        printWithIndent(indent + 2, "slots:\n", .{});
        for (self.slots) |slot| {
            slot.dumpTree(indent + 4);
        }

        printWithIndent(indent + 4, "statements:\n", .{});
        for (self.statements) |statement| {
            statement.dumpTree(indent + 4);
        }
    }
};

pub const IdentifierNode = struct {
    value: []u8,

    pub fn deinit(self: *IdentifierNode, allocator: *Allocator) void {
        allocator.free(self.value);
    }

    pub fn dumpTree(self: IdentifierNode, indent: usize) void {
        printWithIndent(indent, "IdentifierNode \"{s}\"\n", .{self.value});
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

    pub fn dumpTree(self: MessageNode, indent: usize) void {
        const message_type: []const u8 = if (self.arguments.len == 0) "unary" else "keyword";
        printWithIndent(indent, "MessageNode ({s})\n", .{message_type});

        printWithIndent(indent + 2, "receiver:", .{});
        self.receiver.dumpTree(indent + 4);

        printWithIndent(indent + 2, "name: \"{s}\"\n", .{self.message_name});

        printWithIndent(indent + 2, "arguments:", .{});
        for (self.arguments) |argument, i| {
            if (i != 0) std.debug.print(", ", .{});
            std.debug.print("\"{s}\"", .{argument});
        }
        std.debug.print("\n", .{});
    }
};

pub const StringNode = struct {
    value: []const u8,

    pub fn deinit(self: *StringNode, allocator: *Allocator) void {
        allocator.free(self.value);
    }

    pub fn dumpTree(self: StringNode, indent: usize) void {
        printWithIndent(indent, "StringNode ({} bytes)\n", .{self.value.len});
        printWithIndent(indent + 2, "content: \"{s}", .{self.value[0..std.math.min(200, self.value.len)]});
        if (self.value.len > 200) {
            std.debug.print("...", .{});
        }
        std.debug.print("\"\n", .{});
    }
};

pub const NumberNode = union(enum) {
    Integer: u64,
    FloatingPoint: f64,

    pub fn dumpTree(self: NumberNode, indent: usize) void {
        printWithIndent(indent, "NumberNode ", .{self.value.len});
        switch (self) {
            .Integer => std.debug.print("{}\n", .{self.Integer}),
            .FloatingPoint => std.debug.print("{}\n", .{self.FloatingPoint}),
        }
    }
};
