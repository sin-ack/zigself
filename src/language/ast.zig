// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Range = @import("./location_range.zig");

/// A node which describes a single script.
pub const ScriptNode = struct {
    statements: []StatementNode,

    pub fn deinit(self: *ScriptNode, allocator: *Allocator) void {
        for (self.statements) |*statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
    }
};

/// A single statement.
pub const StatementNode = struct {
    expression: ExpressionNode,

    range: Range,

    pub fn deinit(self: *StatementNode, allocator: *Allocator) void {
        self.expression.deinit(allocator);
    }
};

pub const ExpressionNode = union(enum) {
    Object: *ObjectNode,
    Block: *BlockNode,
    Message: *MessageNode,
    Return: *ReturnNode,

    Identifier: IdentifierNode,
    String: StringNode,
    Number: NumberNode,

    pub fn deinit(self: *ExpressionNode, allocator: *Allocator) void {
        switch (self.*) {
            .Object => self.Object.destroy(allocator),
            .Block => self.Block.destroy(allocator),
            .Message => self.Message.destroy(allocator),
            .Return => self.Return.destroy(allocator),

            .Identifier => self.Identifier.deinit(allocator),
            .String => self.String.deinit(allocator),

            else => {},
        }
    }
};

pub const ObjectNode = struct {
    slots: []SlotNode,
    statements: []StatementNode,

    range: Range,

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
};

pub const SlotNode = struct {
    is_mutable: bool,
    is_parent: bool,
    is_argument: bool,

    name: []const u8,
    // NOTE: The arguments will be injected into the object during evaluation.
    arguments: [][]const u8,
    value: ExpressionNode,

    range: Range,

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

    range: Range,

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
};

pub const IdentifierNode = struct {
    value: []u8,

    range: Range,

    pub fn deinit(self: *IdentifierNode, allocator: *Allocator) void {
        allocator.free(self.value);
    }
};

pub const MessageNode = struct {
    receiver: ExpressionNode,
    message_name: []const u8,
    arguments: []ExpressionNode,

    range: Range,

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
};

/// A return node is used to signify non-local returns from blocks. It is only
/// allowed within blocks and must be the last expression in the block.
pub const ReturnNode = struct {
    expression: ExpressionNode,

    range: Range,

    pub fn deinit(self: *ReturnNode, allocator: *Allocator) void {
        self.expression.deinit(allocator);
    }

    pub fn destroy(self: *ReturnNode, allocator: *Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }
};

pub const StringNode = struct {
    value: []const u8,

    range: Range,

    pub fn deinit(self: *StringNode, allocator: *Allocator) void {
        allocator.free(self.value);
    }
};

pub const NumberNode = struct {
    value: union(enum) {
        Integer: i64,
        FloatingPoint: f64,
    },

    range: Range,
};
