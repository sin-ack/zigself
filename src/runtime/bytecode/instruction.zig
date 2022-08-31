// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Range = @import("../../language/Range.zig");
const AstcodeRegisterLocation = @import("./astcode/register_location.zig").RegisterLocation;
const LowcodeRegisterLocation = @import("./lowcode/register_location.zig").RegisterLocation;

fn Instruction(comptime RegisterLocationT: type) type {
    return struct {
        target: RegisterLocation,
        // This is a union(enum) where the tag identifies the instruction and
        // payload is the instruction parameters.
        value: Payload,

        const Self = @This();
        pub const RegisterLocation = RegisterLocationT;

        const Payload = union(enum) {
            // Sends (entering an activation)
            Send: PayloadType.Send,
            PrimSend: PayloadType.Send,
            SelfSend: PayloadType.SelfSend,
            SelfPrimSend: PayloadType.SelfSend,

            // Pushing to the slot stack
            PushConstantSlot: PayloadType.PushParentableSlot,
            PushAssignableSlot: PayloadType.PushParentableSlot,
            PushArgumentSlot: PayloadType.PushNonParentSlot,
            PushInheritedSlot: PayloadType.PushNonParentSlot,

            // Pushing to the argument stack
            PushArg: struct { argument_location: RegisterLocation },

            // Creation (basic objects)
            CreateInteger: i62,
            CreateFloatingPoint: f64,
            CreateByteArray: []const u8,

            // Creation (slots objects)
            CreateObject: struct { slot_count: u32 },
            CreateMethod: struct {
                method_name_location: RegisterLocation,
                slot_count: u32,
                block_index: u32,
            },
            CreateBlock: struct {
                slot_count: u32,
                block_index: u32,
            },

            // Modifiers
            SetMethodInline,

            // Source range
            SourceRange: Range,

            // Exiting an activation
            Return: PayloadType.Return,
            NonlocalReturn: PayloadType.Return,

            // Sentinels (debugging)
            PushArgumentSentinel,
            PushSlotSentinel,
            VerifyArgumentSentinel,
            VerifySlotSentinel,

            // --- Lowcode only instructions ---

            // Register saving at activation entry point
            PushRegisters: if (RegisterLocation.isFinite())
                RegisterLocation.BitSet
            else
                void,

            const PayloadType = struct {
                const Send = struct {
                    receiver_location: RegisterLocation,
                    message_name: []const u8,
                };

                const SelfSend = struct {
                    message_name: []const u8,
                };

                pub const PushParentableSlot = struct {
                    name_location: RegisterLocation,
                    value_location: RegisterLocation,
                    is_parent: bool,
                };

                pub const PushNonParentSlot = struct {
                    name_location: RegisterLocation,
                    value_location: RegisterLocation,
                };

                pub const Return = struct {
                    value_location: RegisterLocation,
                };
            };

            pub fn toString(self: Payload) []const u8 {
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
        };

        pub fn init(target: RegisterLocation, value: Payload) Self {
            return .{ .target = target, .value = value };
        }

        pub fn format(
            inst: Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            try std.fmt.format(writer, "{s}", .{inst.value.toString()});

            switch (inst.value) {
                .Send, .PrimSend => |payload| {
                    try std.fmt.format(writer, "({}, \"{s}\")", .{ payload.receiver_location, payload.message_name });
                },
                .SelfSend, .SelfPrimSend => |payload| {
                    try std.fmt.format(writer, "(\"{s}\")", .{payload.message_name});
                },
                .PushConstantSlot, .PushAssignableSlot => |payload| {
                    try std.fmt.format(writer, "({}, {}, {})", .{ payload.name_location, payload.value_location, payload.is_parent });
                },

                .PushArgumentSlot, .PushInheritedSlot => |payload| {
                    try std.fmt.format(writer, "({}, {})", .{ payload.name_location, payload.value_location });
                },

                .CreateInteger => |payload| {
                    try std.fmt.format(writer, "({})", .{payload});
                },

                .CreateFloatingPoint => |payload| {
                    try std.fmt.format(writer, "({})", .{payload});
                },

                .CreateObject => |payload| {
                    try std.fmt.format(writer, "({})", .{payload.slot_count});
                },

                .CreateMethod => |payload| {
                    try std.fmt.format(writer, "({}, {}, #{})", .{ payload.method_name_location, payload.slot_count, payload.block_index });
                },

                .CreateBlock => |payload| {
                    try std.fmt.format(writer, "({}, #{})", .{ payload.slot_count, payload.block_index });
                },

                .CreateByteArray => |payload| {
                    try std.fmt.format(writer, "(\"{s}\")", .{payload});
                },

                .PushRegisters => |payload| {
                    if (comptime RegisterLocation.isFinite()) {
                        // NOTE: Update width whenever MaskInt changes in bit width
                        try std.fmt.format(writer, "({b:0>8})", .{payload.mask});
                    }
                },

                .Return, .NonlocalReturn => |payload| {
                    try std.fmt.format(writer, "({})", .{payload.value_location});
                },

                .PushArg => |payload| {
                    try std.fmt.format(writer, "({})", .{payload.argument_location});
                },

                .SourceRange => |payload| {
                    try std.fmt.format(writer, "({}:{})", .{ payload.start, payload.end });
                },

                .SetMethodInline,
                .PushArgumentSentinel,
                .PushSlotSentinel,
                .VerifyArgumentSentinel,
                .VerifySlotSentinel,
                => {
                    try std.fmt.format(writer, "()", .{});
                },
            }
        }
    };
}

pub const AstcodeInstruction = Instruction(AstcodeRegisterLocation);
pub const LowcodeInstruction = Instruction(LowcodeRegisterLocation);
