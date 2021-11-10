const std = @import("std");
const Allocator = std.mem.Allocator;

/// A node which describes a single script.
pub const ScriptNode = struct {
    statements: []StatementNode,

    pub fn deinit(self: *ScriptNode, allocator: *Allocator) void {
        for (self.statements) |statement| {
            statement.deinit(allocator);
        }
    }
};

/// A single statement.
pub const StatementNode = struct {
    expression: ExpressionNode,

    pub fn deinit(self: *StatementNode, allocator: *Allocator) void {
        self.expression.deinit(allocator);
    }
};

pub const ExpressionNode = union(enum) {
    Invalid: void,
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
};

pub const IdentifierNode = struct {
    value: []u8,

    pub fn deinit(self: *IdentifierNode, allocator: *Allocator) void {
        allocator.free(self.value);
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
};

pub const StringNode = struct {
    value: []const u8,

    pub fn deinit(self: *StringNode, allocator: *Allocator) void {
        allocator.free(self.value);
    }
};

pub const NumberNode = union(enum) {
    Integer: u64,
    FloatingPoint: f64,
};
