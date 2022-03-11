// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Range = @import("./location_range.zig");
const ref_counted = @import("../utility/ref_counted.zig");

/// A ref-counted slice of statements.
pub const StatementList = struct {
    ref: ref_counted.RefCount,
    statements: []ExpressionNode,

    pub const Ref = ref_counted.RefPtr(StatementList);

    /// Creates a new ref-counted statement slice. Takes ownership of the given
    /// statements slice.
    pub fn create(allocator: Allocator, statements: []ExpressionNode) !StatementList.Ref {
        const self = try allocator.create(StatementList);
        self.init(statements);

        return Ref.adopt(self);
    }

    fn init(self: *StatementList, statements: []ExpressionNode) void {
        self.ref = .{};
        self.statements = statements;
    }

    fn deinit(self: *StatementList, allocator: Allocator) void {
        for (self.statements) |*statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
    }

    pub fn destroy(self: *StatementList, allocator: Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }
};

/// A node which describes a single script.
pub const ScriptNode = struct {
    statements: StatementList.Ref,

    pub fn deinit(self: *ScriptNode, allocator: Allocator) void {
        self.statements.unrefWithAllocator(allocator);
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

    pub fn deinit(self: *ExpressionNode, allocator: Allocator) void {
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

    pub fn range(self: ExpressionNode) Range {
        inline for (comptime std.enums.values(std.meta.Tag(ExpressionNode))) |field| {
            if (self == field)
                return @field(self, @tagName(field)).range;
        }
        unreachable;
    }
};

pub const ObjectNode = struct {
    slots: []SlotNode,
    statements: StatementList.Ref,

    range: Range,

    pub fn deinit(self: *ObjectNode, allocator: Allocator) void {
        for (self.slots) |*slot| {
            slot.deinit(allocator);
        }
        allocator.free(self.slots);

        self.statements.unrefWithAllocator(allocator);
    }

    pub fn destroy(self: *ObjectNode, allocator: Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }
};

pub const SlotNode = struct {
    is_mutable: bool,
    is_parent: bool,
    is_argument: bool,
    is_inherited: bool,

    /// The ID of this slot in definition order.
    /// Inherited slots are always hoisted to the top.
    order: usize,

    name: []const u8,
    value: ?ExpressionNode,

    range: Range,

    pub fn deinit(self: *SlotNode, allocator: Allocator) void {
        allocator.free(self.name);

        if (self.value) |*value|
            value.deinit(allocator);
    }
};

pub const BlockNode = struct {
    slots: []SlotNode,
    statements: StatementList.Ref,

    range: Range,

    pub fn deinit(self: *BlockNode, allocator: Allocator) void {
        for (self.slots) |*slot| {
            slot.deinit(allocator);
        }
        allocator.free(self.slots);

        self.statements.unrefWithAllocator(allocator);
    }

    pub fn destroy(self: *BlockNode, allocator: Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }
};

pub const IdentifierNode = struct {
    value: []u8,

    range: Range,

    pub fn deinit(self: *IdentifierNode, allocator: Allocator) void {
        allocator.free(self.value);
    }
};

pub const MessageNode = struct {
    // NOTE: receiver == null implies the receiver is self.
    receiver: ?ExpressionNode,
    message_name: []const u8,
    arguments: []ExpressionNode,

    range: Range,

    pub fn deinit(self: *MessageNode, allocator: Allocator) void {
        if (self.receiver) |*receiver|
            receiver.deinit(allocator);
        allocator.free(self.message_name);

        for (self.arguments) |*argument| {
            argument.deinit(allocator);
        }
        allocator.free(self.arguments);
    }

    pub fn destroy(self: *MessageNode, allocator: Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }
};

/// A return node is used to signify non-local returns from blocks. It is only
/// allowed within blocks and must be the last expression in the block.
pub const ReturnNode = struct {
    expression: ExpressionNode,

    range: Range,

    pub fn deinit(self: *ReturnNode, allocator: Allocator) void {
        self.expression.deinit(allocator);
    }

    pub fn destroy(self: *ReturnNode, allocator: Allocator) void {
        self.deinit(allocator);
        allocator.destroy(self);
    }
};

pub const StringNode = struct {
    value: []const u8,

    range: Range,

    pub fn deinit(self: *StringNode, allocator: Allocator) void {
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
