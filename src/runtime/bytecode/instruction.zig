// Copyright (c) 2022-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Range = @import("../../language/Range.zig");
const Selector = @import("../Selector.zig");
const PrimitiveIndex = @import("../primitives.zig").PrimitiveIndex;
const AstcodeRegisterLocation = @import("./astcode/register_location.zig").RegisterLocation;
const LowcodeRegisterLocation = @import("./lowcode/register_location.zig").RegisterLocation;

fn Instruction(comptime RegisterLocationT: type) type {
    return struct {
        target: RegisterLocation,
        opcode: Opcode,
        payload: Payload,
        source_range: Range,

        const Self = @This();
        pub const RegisterLocation = RegisterLocationT;

        /// The opcode for this instruction.
        pub const Opcode = enum(u8) {
            // Sends (entering an activation)
            Send,
            PrimSend,
            SelfSend,
            SelfPrimSend,

            // Pushing to the slot stack
            PushConstantSlot,
            PushAssignableSlot,
            PushArgumentSlot,

            // Pushing to the argument stack
            PushArg,

            // Creation (basic objects)
            CreateInteger,
            CreateFloatingPoint,
            CreateByteArray,

            // Creation (slots objects)
            CreateObject,
            CreateMethod,
            CreateBlock,

            // Modifiers
            SetMethodInline,

            // Exiting an activation
            Return,
            NonlocalReturn,

            // Sentinels (debugging)
            PushArgumentSentinel,
            PushSlotSentinel,
            VerifyArgumentSentinel,
            VerifySlotSentinel,

            // --- Lowcode only instructions ---

            // Register saving at activation entry point
            PushRegisters,

            pub fn toString(self: Opcode) []const u8 {
                return switch (self) {
                    .Send => "send",
                    .PrimSend => "prim_send",
                    .SelfSend => "self_send",
                    .SelfPrimSend => "self_prim_send",
                    .PushConstantSlot => "push_constant_slot",
                    .PushAssignableSlot => "push_assignable_slot",
                    .PushArgumentSlot => "push_argument_slot",
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
                    .PushArgumentSentinel => "push_argument_sentinel",
                    .PushSlotSentinel => "push_slot_sentinel",
                    .VerifyArgumentSentinel => "verify_argument_sentinel",
                    .VerifySlotSentinel => "verify_slot_sentinel",
                };
            }

            pub fn payloadField(self: Opcode) []const u8 {
                return switch (self) {
                    .Send => "Send",
                    .SelfSend => "SelfSend",
                    .PrimSend => "PrimSend",
                    .SelfPrimSend => "SelfPrimSend",
                    .PushConstantSlot, .PushAssignableSlot => "PushParentableSlot",
                    .PushArgumentSlot => "PushNonParentSlot",
                    .Return, .NonlocalReturn => "Return",

                    .PushRegisters => "PushRegisters",
                    .CreateInteger => "CreateInteger",
                    .CreateFloatingPoint => "CreateFloatingPoint",
                    .CreateObject => "CreateObject",
                    .CreateMethod => "CreateMethod",
                    .CreateBlock => "CreateBlock",
                    .CreateByteArray => "CreateByteArray",
                    .PushArg => "PushArg",

                    .PushArgumentSentinel, .PushSlotSentinel, .VerifyArgumentSentinel, .VerifySlotSentinel, .SetMethodInline => "None",
                };
            }

            pub fn PayloadT(comptime opcode: Opcode) type {
                const opcode_field = opcode.payloadField();
                inline for (@typeInfo(Payload).@"union".fields) |field| {
                    if (std.mem.eql(u8, opcode_field, field.name))
                        return field.type;
                }

                @compileError("Unknown opcode " ++ @tagName(opcode));
            }
        };

        /// The payload for the opcode.
        pub const Payload = union {
            None: void,

            Send: struct {
                receiver_location: RegisterLocation,
                selector: Selector,
                send_index: u32,
            },
            SelfSend: struct {
                selector: Selector,
                send_index: u32,
            },
            PrimSend: struct {
                receiver_location: RegisterLocation,
                index: PrimitiveIndex,
            },
            SelfPrimSend: struct {
                index: PrimitiveIndex,
            },
            PushParentableSlot: struct {
                name_location: RegisterLocation,
                value_location: RegisterLocation,
                is_parent: bool,
            },
            PushNonParentSlot: struct {
                name_location: RegisterLocation,
                value_location: RegisterLocation,
            },
            PushArg: struct {
                argument_location: RegisterLocation,
            },
            CreateInteger: i62,
            CreateFloatingPoint: f64,
            CreateByteArray: []const u8,
            CreateObject: struct { slot_count: u16 },
            CreateMethod: struct {
                method_name_location: RegisterLocation,
                slot_count: u16,
                block_index: u32,
            },
            CreateBlock: struct {
                slot_count: u16,
                block_index: u32,
            },
            Return: struct {
                value_location: RegisterLocation,
            },

            // --- Lowcode only payloads ---

            PushRegisters: if (RegisterLocation.isFinite())
                RegisterLocation.BitSet
            else
                void,
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

            try std.fmt.format(writer, "{s}", .{inst.opcode.toString()});

            switch (inst.opcode) {
                .Send => {
                    const payload = inst.payload.Send;
                    try std.fmt.format(writer, "({}, {}) (send #{})", .{ payload.receiver_location, payload.selector, payload.send_index });
                },
                .SelfSend => {
                    const payload = inst.payload.SelfSend;
                    try std.fmt.format(writer, "({}) (send #{})", .{ payload.selector, payload.send_index });
                },
                .PrimSend => {
                    const payload = inst.payload.PrimSend;
                    try std.fmt.format(writer, "({}, {})", .{ payload.receiver_location, payload.index });
                },
                .SelfPrimSend => {
                    const payload = inst.payload.SelfPrimSend;
                    try std.fmt.format(writer, "({})", .{payload.index});
                },
                .PushConstantSlot, .PushAssignableSlot => {
                    const payload = inst.payload.PushParentableSlot;
                    try std.fmt.format(writer, "({}, {}, {})", .{ payload.name_location, payload.value_location, payload.is_parent });
                },

                .PushArgumentSlot => {
                    const payload = inst.payload.PushNonParentSlot;
                    try std.fmt.format(writer, "({}, {})", .{ payload.name_location, payload.value_location });
                },

                .CreateInteger => {
                    try std.fmt.format(writer, "({})", .{inst.payload.CreateInteger});
                },

                .CreateFloatingPoint => {
                    try std.fmt.format(writer, "({})", .{inst.payload.CreateFloatingPoint});
                },

                .CreateObject => {
                    try std.fmt.format(writer, "({})", .{inst.payload.CreateObject.slot_count});
                },

                .CreateMethod => {
                    const payload = inst.payload.CreateMethod;
                    try std.fmt.format(writer, "({}, {}, #{})", .{ payload.method_name_location, payload.slot_count, payload.block_index });
                },

                .CreateBlock => {
                    const payload = inst.payload.CreateBlock;
                    try std.fmt.format(writer, "({}, #{})", .{ payload.slot_count, payload.block_index });
                },

                .CreateByteArray => {
                    try std.fmt.format(writer, "(\"{s}\")", .{inst.payload.CreateByteArray});
                },

                .PushRegisters => {
                    if (comptime RegisterLocation.isFinite()) {
                        // NOTE: Update width whenever MaskInt changes in bit width
                        try std.fmt.format(writer, "({b:0>8})", .{inst.payload.PushRegisters.mask});
                    }
                },

                .Return, .NonlocalReturn => {
                    try std.fmt.format(writer, "({})", .{inst.payload.Return.value_location});
                },

                .PushArg => {
                    try std.fmt.format(writer, "({})", .{inst.payload.PushArg.argument_location});
                },

                .PushArgumentSentinel,
                .PushSlotSentinel,
                .VerifyArgumentSentinel,
                .VerifySlotSentinel,
                .SetMethodInline,
                => {
                    try std.fmt.format(writer, "()", .{});
                },
            }
        }
    };
}

pub const AstcodeInstruction = Instruction(AstcodeRegisterLocation);
pub const LowcodeInstruction = Instruction(LowcodeRegisterLocation);
