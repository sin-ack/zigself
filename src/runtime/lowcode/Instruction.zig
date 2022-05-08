// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Actor = @import("../Actor.zig");
const Block = @import("./Block.zig");
const Range = @import("../../language/Range.zig");
const Completion = @import("../Completion.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const RegisterLocation = @import("./register_location.zig").RegisterLocation;
const RegisterPool = @import("./RegisterPool.zig");

target: RegisterLocation,
tag: Tag,
// FIXME: Make this type less manual
arguments: [3]u64,

// NOTE: This check is for push_registers and it verifies that the
//       RegisterBitSet mask can always fit into a single instruction argument
//       (this will hopefully be always true ^^;).
comptime {
    if (@sizeOf(RegisterPool.RegisterBitSet) > @sizeOf(u64))
        @compileError("!!! The RegisterBitSet can no longer fit in a single instruction argument!");
}

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

    // Register saving at activation entry point
    PushRegisters,

    // Exiting an activation
    WriteReturnValue,
    Return,
    NonlocalReturn,

    // Arguments
    PushArg,

    // Source range
    SourceRange,

    // Sentinels (debugging)
    PushArgumentSentinel,
    PushSlotSentinel,
    VerifyArgumentSentinel,
    VerifySlotSentinel,

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
            .PushRegisters => "push_registers",
            .CreateInteger => "create_integer",
            .CreateFloatingPoint => "create_floating_point",
            .CreateObject => "create_object",
            .CreateMethod => "create_method",
            .CreateBlock => "create_block",
            .CreateByteArray => "create_byte_array",
            .SetMethodInline => "set_method_inline",
            .WriteReturnValue => "write_return_value",
            .Return => "return",
            .NonlocalReturn => "nonlocal_return",
            .PushArg => "push_arg",
            .SourceRange => "source_range",
            .PushArgumentSentinel => "push_argument_sentinel",
            .PushSlotSentinel => "push_slot_sentinel",
            .VerifyArgumentSentinel => "verify_argument_sentinel",
            .VerifySlotSentinel => "verify_slot_sentinel",
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
            .PushRegisters => Self.Payload.PushRegisters,
            .WriteReturnValue => Self.Payload.WriteReturnValue,
            .PushArg => Self.Payload.PushArg,
            .SourceRange => Self.Payload.SourceRange,

            .Return,
            .NonlocalReturn,
            .SetMethodInline,
            .PushArgumentSentinel,
            .PushSlotSentinel,
            .VerifyArgumentSentinel,
            .VerifySlotSentinel,
            => @panic("Attempted to get payload type of instruction without payload"),
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

    pub const PushRegisters = struct {
        clobbered_registers: RegisterPool.RegisterBitSet,
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
        method_name_location: RegisterLocation,
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

    pub const WriteReturnValue = struct {
        value_location: RegisterLocation,
    };

    pub const SourceRange = Range;
};

// --- Creation ---

pub fn send(target: RegisterLocation, receiver_location: RegisterLocation, message_name: []const u8) Self {
    return .{ .tag = .Send, .target = target, .arguments = .{ receiver_location.asInt(), @ptrToInt(message_name.ptr), message_name.len } };
}

pub fn primSend(target: RegisterLocation, receiver_location: RegisterLocation, message_name: []const u8) Self {
    return .{ .tag = .PrimSend, .target = target, .arguments = .{ receiver_location.asInt(), @ptrToInt(message_name.ptr), message_name.len } };
}

pub fn selfSend(target: RegisterLocation, message_name: []const u8) Self {
    return .{ .tag = .SelfSend, .target = target, .arguments = .{ @ptrToInt(message_name.ptr), message_name.len, undefined } };
}

pub fn selfPrimSend(target: RegisterLocation, message_name: []const u8) Self {
    return .{ .tag = .SelfPrimSend, .target = target, .arguments = .{ @ptrToInt(message_name.ptr), message_name.len, undefined } };
}

pub fn pushConstantSlot(target: RegisterLocation, name_location: RegisterLocation, is_parent: bool, value_location: RegisterLocation) Self {
    return .{ .tag = .PushConstantSlot, .target = target, .arguments = .{ name_location.asInt(), @boolToInt(is_parent), value_location.asInt() } };
}

pub fn pushAssignableSlot(target: RegisterLocation, name_location: RegisterLocation, is_parent: bool, value_location: RegisterLocation) Self {
    return .{ .tag = .PushAssignableSlot, .target = target, .arguments = .{ name_location.asInt(), @boolToInt(is_parent), value_location.asInt() } };
}

pub fn pushArgumentSlot(target: RegisterLocation, name_location: RegisterLocation, value_location: RegisterLocation) Self {
    return .{ .tag = .PushArgumentSlot, .target = target, .arguments = .{ name_location.asInt(), value_location.asInt(), undefined } };
}

pub fn pushInheritedSlot(target: RegisterLocation, name_location: RegisterLocation, value_location: RegisterLocation) Self {
    return .{ .tag = .PushInheritedSlot, .target = target, .arguments = .{ name_location.asInt(), value_location.asInt(), undefined } };
}

// FIXME: Turn this into i62
pub fn createInteger(target: RegisterLocation, value: i64) Self {
    return .{ .tag = .CreateInteger, .target = target, .arguments = .{ @bitCast(u64, value), undefined, undefined } };
}

pub fn createFloatingPoint(target: RegisterLocation, value: f64) Self {
    return .{ .tag = .CreateFloatingPoint, .target = target, .arguments = .{ @bitCast(u64, value), undefined, undefined } };
}

pub fn createObject(target: RegisterLocation, slot_count: u32) Self {
    return .{ .tag = .CreateObject, .target = target, .arguments = .{ slot_count, undefined, undefined } };
}

pub fn createMethod(target: RegisterLocation, method_name_location: RegisterLocation, slot_count: u32, block_index: u32) Self {
    return .{ .tag = .CreateMethod, .target = target, .arguments = .{ method_name_location.asInt(), slot_count, block_index } };
}

pub fn createBlock(target: RegisterLocation, slot_count: u32, block_index: u32) Self {
    return .{ .tag = .CreateBlock, .target = target, .arguments = .{ slot_count, block_index, undefined } };
}

pub fn createByteArray(target: RegisterLocation, value: []const u8) Self {
    return .{ .tag = .CreateByteArray, .target = target, .arguments = .{ @ptrToInt(value.ptr), value.len, undefined } };
}

pub fn setMethodInline(target: RegisterLocation) Self {
    return .{ .tag = .SetMethodInline, .target = target, .arguments = .{ undefined, undefined, undefined } };
}

pub fn pushRegisters(target: RegisterLocation, clobbered_registers: RegisterPool.RegisterBitSet) Self {
    return .{ .tag = .PushRegisters, .target = target, .arguments = .{ clobbered_registers.mask, undefined, undefined } };
}

pub fn writeReturnValue(target: RegisterLocation, value_location: RegisterLocation) Self {
    return .{ .tag = .WriteReturnValue, .target = target, .arguments = .{ value_location.asInt(), undefined, undefined } };
}

pub fn return_(target: RegisterLocation) Self {
    return .{ .tag = .Return, .target = target, .arguments = .{ undefined, undefined, undefined } };
}

pub fn nonlocalReturn(target: RegisterLocation) Self {
    return .{ .tag = .NonlocalReturn, .target = target, .arguments = .{ undefined, undefined, undefined } };
}

pub fn pushArg(target: RegisterLocation, argument_location: RegisterLocation) Self {
    return .{ .tag = .PushArg, .target = target, .arguments = .{ argument_location.asInt(), undefined, undefined } };
}

pub fn sourceRange(target: RegisterLocation, range: Range) Self {
    return .{ .tag = .SourceRange, .target = target, .arguments = .{ range.start, range.end, undefined } };
}

pub fn pushArgumentSentinel(target: RegisterLocation) Self {
    return .{ .tag = .PushArgumentSentinel, .target = target, .arguments = .{ undefined, undefined, undefined } };
}

pub fn pushSlotSentinel(target: RegisterLocation) Self {
    return .{ .tag = .PushSlotSentinel, .target = target, .arguments = .{ undefined, undefined, undefined } };
}

pub fn verifyArgumentSentinel(target: RegisterLocation) Self {
    return .{ .tag = .VerifyArgumentSentinel, .target = target, .arguments = .{ undefined, undefined, undefined } };
}

pub fn verifySlotSentinel(target: RegisterLocation) Self {
    return .{ .tag = .VerifySlotSentinel, .target = target, .arguments = .{ undefined, undefined, undefined } };
}

pub fn format(
    inst: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try std.fmt.format(writer, "{s}", .{inst.tag.toString()});

    switch (inst.tag) {
        .Send, .PrimSend => {
            const message_name = @intToPtr([*]const u8, inst.arguments[1])[0..inst.arguments[2]];
            try std.fmt.format(writer, "({}, \"{s}\")", .{ RegisterLocation.fromInt(@intCast(u32, inst.arguments[0])), message_name });
        },
        .SelfSend, .SelfPrimSend => {
            const message_name = @intToPtr([*]const u8, inst.arguments[0])[0..inst.arguments[1]];
            try std.fmt.format(writer, "(\"{s}\")", .{message_name});
        },

        .PushConstantSlot, .PushAssignableSlot => {
            try std.fmt.format(writer, "({}, {}, {})", .{ RegisterLocation.fromInt(@intCast(u32, inst.arguments[0])), inst.arguments[1] != 0, RegisterLocation.fromInt(@intCast(u32, inst.arguments[2])) });
        },

        .PushArgumentSlot, .PushInheritedSlot => {
            try std.fmt.format(writer, "({}, {})", .{ RegisterLocation.fromInt(@intCast(u32, inst.arguments[0])), RegisterLocation.fromInt(@intCast(u32, inst.arguments[1])) });
        },

        .CreateInteger => {
            try std.fmt.format(writer, "({})", .{@bitCast(i64, inst.arguments[0])});
        },

        .CreateFloatingPoint => {
            try std.fmt.format(writer, "({})", .{@bitCast(f64, inst.arguments[0])});
        },

        .CreateObject => {
            try std.fmt.format(writer, "({})", .{inst.arguments[0]});
        },

        .CreateMethod => {
            try std.fmt.format(writer, "({}, {}, #{})", .{ RegisterLocation.fromInt(@intCast(u32, inst.arguments[0])), inst.arguments[1], inst.arguments[2] });
        },

        .CreateBlock => {
            try std.fmt.format(writer, "({}, #{})", .{ inst.arguments[0], inst.arguments[1] });
        },

        .CreateByteArray => {
            if (inst.arguments[0] == 0) {
                // Empty slice
                try std.fmt.formatText("(\"\")", "s", options, writer);
            } else {
                const value = @intToPtr([*]const u8, inst.arguments[0])[0..inst.arguments[1]];
                try std.fmt.format(writer, "(\"{s}\")", .{value});
            }
        },

        .PushRegisters => {
            // NOTE: Update width whenever MaskInt changes in bit width
            try std.fmt.format(writer, "({b:0>8})", .{@intCast(RegisterPool.RegisterBitSet.MaskInt, inst.arguments[0])});
        },

        .Return,
        .NonlocalReturn,
        .SetMethodInline,
        .PushArgumentSentinel,
        .PushSlotSentinel,
        .VerifyArgumentSentinel,
        .VerifySlotSentinel,
        => {
            try std.fmt.format(writer, "()", .{});
        },

        .WriteReturnValue, .PushArg => {
            try std.fmt.format(writer, "({})", .{RegisterLocation.fromInt(@intCast(u32, inst.arguments[0]))});
        },

        .SourceRange => {
            try std.fmt.format(writer, "({}:{})", .{ inst.arguments[0], inst.arguments[1] });
        },
    }
}

pub fn payload(self: Self, comptime tag: Tag) tag.Payload() {
    std.debug.assert(self.tag == tag);

    return switch (tag) {
        .Send, .PrimSend => Payload.Send{
            .receiver_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[0])),
            .message_name = @intToPtr([*]const u8, self.arguments[1])[0..self.arguments[2]],
        },
        .SelfSend, .SelfPrimSend => Payload.SelfSend{
            .message_name = @intToPtr([*]const u8, self.arguments[0])[0..self.arguments[1]],
        },
        .PushConstantSlot, .PushAssignableSlot => Payload.PushParentableSlot{
            .name_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[0])),
            .is_parent = self.arguments[1] != 0,
            .value_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[2])),
        },
        .PushArgumentSlot, .PushInheritedSlot => Payload.PushNonParentSlot{
            .name_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[0])),
            .value_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[1])),
        },
        .PushRegisters => Payload.PushRegisters{
            .clobbered_registers = RegisterPool.RegisterBitSet{ .mask = @intCast(RegisterPool.RegisterBitSet.MaskInt, self.arguments[0]) },
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
            .method_name_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[0])),
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
        .WriteReturnValue => Payload.WriteReturnValue{
            .value_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[0])),
        },
        .PushArg => Payload.PushArg{
            .argument_location = RegisterLocation.fromInt(@intCast(u32, self.arguments[0])),
        },
        .SourceRange => Payload.SourceRange{
            .start = self.arguments[0],
            .end = self.arguments[1],
        },

        .Return,
        .NonlocalReturn,
        .SetMethodInline,
        .PushArgumentSentinel,
        .PushSlotSentinel,
        .VerifyArgumentSentinel,
        .VerifySlotSentinel,
        => @panic("Attempted to get payload of instruction without payload"),
    };
}
