// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Actor = @import("../Actor.zig");
const Block = @import("./Block.zig");
const Completion = @import("../completion.zig");
const VirtualMachine = @import("../virtual_machine.zig");
const RegisterLocation = @import("./register_location.zig").RegisterLocation;

// NOTE: This is set in Block.addOpcode
target: RegisterLocation = undefined,
tag: Tag,
// FIXME: Make this type less manual
arguments: [3]u64,

const Self = @This();

const Tag = enum(u32) {
    // Sends (entering an activation)
    Send,
    PrimSend,
    SelfSend,
    SelfPrimSend,

    // Slots
    PushConstantSlot,
    PushAssignableSlot,
    PushArgumentSlot,
    PushInheritedSlot,

    // Creation
    CreateInteger,
    CreateFloatingPoint,
    CreateObject,
    CreateMethod,
    CreateBlock,
    CreateByteArray,

    // Modifiers
    SetMethodInline,

    // Exiting an activation
    ExitActivation,
    NonlocalReturn,

    // Arguments
    PushArg,

    pub fn toString(self: Tag) []const u8 {
        return switch (self) {
            .Send => "send",
            .PrimSend => "prim_send",
            .SelfSend => "self_send",
            .SelfPrimSend => "self_prim_send",
            .PushConstantSlot => "push_constant_slot",
            .PushAssignableSlot => "push_assignable_slot",
            .PushArgumentSlot => "push_argument_slot",
            .PushInheritedSlot => "push_inherited_slot",
            .CreateInteger => "create_integer",
            .CreateFloatingPoint => "create_floating_point",
            .CreateObject => "create_object",
            .CreateMethod => "create_method",
            .CreateBlock => "create_block",
            .CreateByteArray => "create_byte_array",
            .SetMethodInline => "set_method_inline",
            .ExitActivation => "exit_activation",
            .NonlocalReturn => "nonlocal_return",
            .PushArg => "push_arg",
        };
    }

    pub fn Payload(comptime self: Tag) type {
        return switch (self) {
            .Send, .PrimSend => Self.Payload.Send,
            .SelfSend, .SelfPrimSend => Self.Payload.SelfSend,
            .PushConstantSlot, .PushAssignableSlot => Self.Payload.PushParentableSlot,
            .PushArgumentSlot, .PushInheritedSlot => Self.Payload.PushNonParentSlot,
            .CreateObject => Self.Payload.CreateObject,
            .CreateMethod => Self.Payload.CreateMethod,
            .CreateBlock => Self.Payload.CreateBlock,
            .CreateInteger => Self.Payload.CreateInteger,
            .CreateFloatingPoint => Self.Payload.CreateFloatingPoint,
            .CreateByteArray => Self.Payload.CreateByteArray,
            .ExitActivation, .NonlocalReturn => Self.Payload.ExitActivation,
            .PushArg => Self.Payload.PushArg,

            .SetMethodInline => @panic("Attempted to get payload type of opcode without payload"),
        };
    }
};

pub const Payload = struct {
    pub const Send = struct {
        receiver_location: RegisterLocation,
        message_name: []const u8,
    };

    pub const SelfSend = struct {
        message_name: []const u8,
    };

    pub const PushParentableSlot = struct {
        name_location: RegisterLocation,
        is_parent: bool,
        value_location: RegisterLocation,
    };

    pub const PushNonParentSlot = struct {
        name_location: RegisterLocation,
        value_location: RegisterLocation,
    };

    pub const CreateInteger = struct {
        value: i64,
    };

    pub const CreateFloatingPoint = struct {
        value: f64,
    };

    pub const CreateObject = struct {
        slot_count: u32,
    };

    pub const CreateMethod = struct {
        method_name: RegisterLocation,
        slot_count: u32,
        block_index: u32,
    };

    pub const CreateBlock = struct {
        slot_count: u32,
        block_index: u32,
    };

    pub const CreateByteArray = struct {
        string: []const u8,
    };

    pub const PushArg = struct {
        argument_location: RegisterLocation,
    };

    pub const ExitActivation = struct {
        value_location: RegisterLocation,
    };
};

// --- Creation ---

pub fn send(receiver_location: RegisterLocation, message_name: []const u8) Self {
    return .{ .tag = .Send, .arguments = .{ @enumToInt(receiver_location), @ptrToInt(message_name.ptr), message_name.len } };
}

pub fn primSend(receiver_location: RegisterLocation, message_name: []const u8) Self {
    return .{ .tag = .PrimSend, .arguments = .{ @enumToInt(receiver_location), @ptrToInt(message_name.ptr), message_name.len } };
}

pub fn selfSend(message_name: []const u8) Self {
    return .{ .tag = .SelfSend, .arguments = .{ @ptrToInt(message_name.ptr), message_name.len, undefined } };
}

pub fn selfPrimSend(message_name: []const u8) Self {
    return .{ .tag = .SelfPrimSend, .arguments = .{ @ptrToInt(message_name.ptr), message_name.len, undefined } };
}

pub fn pushConstantSlot(name_location: RegisterLocation, is_parent: bool, value_location: RegisterLocation) Self {
    return .{ .tag = .PushConstantSlot, .arguments = .{ @enumToInt(name_location), @boolToInt(is_parent), @enumToInt(value_location) } };
}

pub fn pushAssignableSlot(name_location: RegisterLocation, is_parent: bool, value_location: RegisterLocation) Self {
    return .{ .tag = .PushAssignableSlot, .arguments = .{ @enumToInt(name_location), @boolToInt(is_parent), @enumToInt(value_location) } };
}

pub fn pushArgumentSlot(name_location: RegisterLocation, value_location: RegisterLocation) Self {
    return .{ .tag = .PushArgumentSlot, .arguments = .{ @enumToInt(name_location), @enumToInt(value_location), undefined } };
}

pub fn pushInheritedSlot(name_location: RegisterLocation, value_location: RegisterLocation) Self {
    return .{ .tag = .PushInheritedSlot, .arguments = .{ @enumToInt(name_location), @enumToInt(value_location), undefined } };
}

// FIXME: Turn this into i62
pub fn createInteger(value: i64) Self {
    return .{ .tag = .CreateInteger, .arguments = .{ @bitCast(u64, value), undefined, undefined } };
}

pub fn createFloatingPoint(value: f64) Self {
    return .{ .tag = .CreateFloatingPoint, .arguments = .{ @bitCast(u64, value), undefined, undefined } };
}

pub fn createObject(slot_count: u32) Self {
    return .{ .tag = .CreateObject, .arguments = .{ slot_count, undefined, undefined } };
}

pub fn createMethod(method_name_location: RegisterLocation, slot_count: u32, block_index: u32) Self {
    return .{ .tag = .CreateMethod, .arguments = .{ @enumToInt(method_name_location), slot_count, block_index } };
}

pub fn createBlock(slot_count: u32, block_index: u32) Self {
    return .{ .tag = .CreateBlock, .arguments = .{ slot_count, block_index, undefined } };
}

pub fn createByteArray(value: []const u8) Self {
    return .{ .tag = .CreateByteArray, .arguments = .{ @ptrToInt(value.ptr), value.len, undefined } };
}

pub fn setMethodInline() Self {
    return .{ .tag = .SetMethodInline, .arguments = .{ undefined, undefined, undefined } };
}

pub fn exitActivation(expression_location: RegisterLocation) Self {
    return .{ .tag = .ExitActivation, .arguments = .{ @enumToInt(expression_location), undefined, undefined } };
}

pub fn nonlocalReturn(expression_location: RegisterLocation) Self {
    return .{ .tag = .NonlocalReturn, .arguments = .{ @enumToInt(expression_location), undefined, undefined } };
}

pub fn pushArg(argument_location: RegisterLocation) Self {
    return .{ .tag = .PushArg, .arguments = .{ @enumToInt(argument_location), undefined, undefined } };
}

pub fn format(
    opcode: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try std.fmt.format(writer, "{s}", .{opcode.tag.toString()});

    switch (opcode.tag) {
        .Send, .PrimSend => {
            const message_name = @intToPtr([*]const u8, opcode.arguments[1])[0..opcode.arguments[2]];
            try std.fmt.format(writer, "(%{}, \"{s}\")", .{ opcode.arguments[0], message_name });
        },
        .SelfSend, .SelfPrimSend => {
            const message_name = @intToPtr([*]const u8, opcode.arguments[0])[0..opcode.arguments[1]];
            try std.fmt.format(writer, "(\"{s}\")", .{message_name});
        },

        .PushConstantSlot, .PushAssignableSlot => {
            try std.fmt.format(writer, "(%{}, {}, %{})", .{ opcode.arguments[0], opcode.arguments[1] != 0, opcode.arguments[2] });
        },

        .PushArgumentSlot, .PushInheritedSlot => {
            try std.fmt.format(writer, "(%{}, %{})", .{ opcode.arguments[0], opcode.arguments[1] });
        },

        .CreateInteger => {
            try std.fmt.format(writer, "({})", .{@bitCast(i64, opcode.arguments[0])});
        },

        .CreateFloatingPoint => {
            try std.fmt.format(writer, "({})", .{@bitCast(f64, opcode.arguments[0])});
        },

        .CreateObject => {
            try std.fmt.format(writer, "({})", .{opcode.arguments[0]});
        },

        .CreateMethod => {
            try std.fmt.format(writer, "(%{}, {}, #{})", .{ opcode.arguments[0], opcode.arguments[1], opcode.arguments[2] });
        },

        .CreateBlock => {
            try std.fmt.format(writer, "({}, #{})", .{ opcode.arguments[0], opcode.arguments[1] });
        },

        .CreateByteArray => {
            if (opcode.arguments[0] == 0) {
                // Empty slice
                try std.fmt.formatText("(\"\")", "s", options, writer);
            } else {
                const value = @intToPtr([*]const u8, opcode.arguments[0])[0..opcode.arguments[1]];
                try std.fmt.format(writer, "(\"{s}\")", .{value});
            }
        },

        .SetMethodInline => {
            try std.fmt.format(writer, "()", .{});
        },

        .ExitActivation, .NonlocalReturn, .PushArg => {
            try std.fmt.format(writer, "(%{})", .{opcode.arguments[0]});
        },
    }
}

pub fn payload(self: Self, comptime tag: Tag) tag.Payload() {
    std.debug.assert(self.tag == tag);

    return switch (tag) {
        .Send, .PrimSend => Payload.Send{
            .receiver_location = RegisterLocation.fromIndex(@intCast(u32, self.arguments[0])),
            .message_name = @intToPtr([*]const u8, self.arguments[1])[0..self.arguments[2]],
        },
        .SelfSend, .SelfPrimSend => Payload.SelfSend{
            .message_name = @intToPtr([*]const u8, self.arguments[0])[0..self.arguments[1]],
        },
        .PushConstantSlot, .PushAssignableSlot => Self.Payload.PushParentableSlot{
            .name_location = RegisterLocation.fromIndex(@intCast(u32, self.arguments[0])),
            .is_parent = self.arguments[1] != 0,
            .value_location = RegisterLocation.fromIndexAllowNil(@intCast(u32, self.arguments[2])),
        },
        .PushArgumentSlot, .PushInheritedSlot => Self.Payload.PushNonParentSlot{
            .name_location = RegisterLocation.fromIndex(@intCast(u32, self.arguments[0])),
            .value_location = RegisterLocation.fromIndexAllowNil(@intCast(u32, self.arguments[1])),
        },
        .CreateInteger => Payload.CreateInteger{
            .value = @bitCast(i64, self.arguments[0]),
        },
        .CreateFloatingPoint => Payload.CreateFloatingPoint{
            .value = @bitCast(f64, self.arguments[0]),
        },
        .CreateObject => Payload.CreateObject{
            .slot_count = @intCast(u32, self.arguments[0]),
        },
        .CreateMethod => Payload.CreateMethod{
            .method_name = RegisterLocation.fromIndex(@intCast(u32, self.arguments[0])),
            .slot_count = @intCast(u32, self.arguments[1]),
            .block_index = @intCast(u32, self.arguments[2]),
        },
        .CreateBlock => Payload.CreateBlock{
            .slot_count = @intCast(u32, self.arguments[0]),
            .block_index = @intCast(u32, self.arguments[1]),
        },
        .CreateByteArray => Payload.CreateByteArray{
            // NOTE: Working around Zig compiler invariant violation for empty slices
            .string = if (self.arguments[0] == 0)
                ""
            else
                @intToPtr([*]const u8, self.arguments[0])[0..self.arguments[1]],
        },
        .ExitActivation, .NonlocalReturn => Payload.ExitActivation{
            .value_location = RegisterLocation.fromIndexAllowNil(@intCast(u32, self.arguments[0])),
        },
        .PushArg => Payload.PushArg{
            .argument_location = RegisterLocation.fromIndex(@intCast(u32, self.arguments[0])),
        },

        .SetMethodInline => @panic("Attempted to get payload of opcode without payload"),
    };
}
