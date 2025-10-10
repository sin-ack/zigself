// Copyright (c) 2022-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Range = @import("../../language/Range.zig");
const Selector = @import("../Selector.zig");
const LocalIndex = @import("../bytecode.zig").LocalIndex;
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

            // Local manipulation
            GetLocal,
            PutLocal,

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
            VerifyArgumentSentinel,

            // --- Lowcode only instructions ---

            // Register saving at activation entry point
            PushRegisters,

            pub fn toString(self: Opcode) []const u8 {
                return switch (self) {
                    .Send => "send",
                    .PrimSend => "prim_send",
                    .SelfSend => "self_send",
                    .SelfPrimSend => "self_prim_send",
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
                    .GetLocal => "get_local",
                    .PutLocal => "put_local",
                    .PushArg => "push_arg",
                    .PushArgumentSentinel => "push_argument_sentinel",
                    .VerifyArgumentSentinel => "verify_argument_sentinel",
                };
            }

            pub fn payloadField(self: Opcode) []const u8 {
                return switch (self) {
                    .Send => "Send",
                    .SelfSend => "SelfSend",
                    .PrimSend => "PrimSend",
                    .SelfPrimSend => "SelfPrimSend",
                    .Return, .NonlocalReturn => "Return",
                    .GetLocal => "GetLocal",
                    .PutLocal => "PutLocal",

                    .PushRegisters => "PushRegisters",
                    .CreateInteger => "CreateInteger",
                    .CreateFloatingPoint => "CreateFloatingPoint",
                    .CreateObject => "CreateObject",
                    .CreateMethod => "CreateMethod",
                    .CreateBlock => "CreateBlock",
                    .CreateByteArray => "CreateByteArray",
                    .PushArg => "PushArg",

                    .PushArgumentSentinel, .VerifyArgumentSentinel => "ArgumentSentinel",

                    .SetMethodInline => "None",
                };
            }

            pub fn PayloadT(comptime opcode: Opcode) type {
                @setEvalBranchQuota(10000);

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
            PushArg: struct {
                argument_location: RegisterLocation,
            },
            CreateInteger: i62,
            CreateFloatingPoint: f64,
            CreateByteArray: []const u8,
            CreateObject: struct { descriptor_index: u32 },
            CreateMethod: struct {
                method_name_location: RegisterLocation,
                descriptor_index: u32,
                block_index: u32,
            },
            CreateBlock: struct {
                descriptor_index: u32,
                block_index: u32,
            },
            Return: struct {
                value_location: RegisterLocation,
            },
            GetLocal: struct {
                local_index: LocalIndex,
            },
            PutLocal: struct {
                local_index: LocalIndex,
                value_location: RegisterLocation,
            },
            ArgumentSentinel: usize,

            // --- Lowcode only payloads ---

            PushRegisters: if (RegisterLocation.isFinite())
                RegisterLocation.BitSet
            else
                void,
        };

        pub fn format(inst: Self, writer: *std.Io.Writer) !void {
            try writer.print("{s}", .{inst.opcode.toString()});

            switch (inst.opcode) {
                .Send => {
                    const payload = inst.payload.Send;
                    try writer.print("({f}, {f}) (send #{})", .{ payload.receiver_location, payload.selector, payload.send_index });
                },
                .SelfSend => {
                    const payload = inst.payload.SelfSend;
                    try writer.print("({f}) (send #{})", .{ payload.selector, payload.send_index });
                },
                .PrimSend => {
                    const payload = inst.payload.PrimSend;
                    try writer.print("({f}, {f})", .{ payload.receiver_location, payload.index });
                },
                .SelfPrimSend => {
                    const payload = inst.payload.SelfPrimSend;
                    try writer.print("({f})", .{payload.index});
                },

                .CreateInteger => {
                    try writer.print("({})", .{inst.payload.CreateInteger});
                },

                .CreateFloatingPoint => {
                    try writer.print("({})", .{inst.payload.CreateFloatingPoint});
                },

                .CreateObject => {
                    try writer.print("(OD#{})", .{inst.payload.CreateObject.descriptor_index});
                },

                .CreateMethod => {
                    const payload = inst.payload.CreateMethod;
                    try writer.print("({f}, OD#{}, #{})", .{ payload.method_name_location, payload.descriptor_index, payload.block_index });
                },

                .CreateBlock => {
                    const payload = inst.payload.CreateBlock;
                    try writer.print("(OD#{}, #{})", .{ payload.descriptor_index, payload.block_index });
                },

                .CreateByteArray => {
                    try writer.print("(\"{s}\")", .{inst.payload.CreateByteArray});
                },

                .PushRegisters => {
                    if (comptime RegisterLocation.isFinite()) {
                        // NOTE: Update width whenever MaskInt changes in bit width
                        try writer.print("({b:0>8})", .{inst.payload.PushRegisters.mask});
                    }
                },

                .Return, .NonlocalReturn => {
                    try writer.print("({f})", .{inst.payload.Return.value_location});
                },

                .PushArg => {
                    try writer.print("({f})", .{inst.payload.PushArg.argument_location});
                },

                .GetLocal => {
                    try writer.print("({f})", .{inst.payload.GetLocal.local_index});
                },

                .PutLocal => {
                    const payload = inst.payload.PutLocal;
                    try writer.print("({f}, {f})", .{ payload.local_index, payload.value_location });
                },

                .PushArgumentSentinel, .VerifyArgumentSentinel => {
                    try writer.print("({})", .{inst.payload.ArgumentSentinel});
                },

                .SetMethodInline,
                => {
                    try writer.writeAll("()");
                },
            }
        }
    };
}

pub const AstcodeInstruction = Instruction(AstcodeRegisterLocation);
pub const LowcodeInstruction = Instruction(LowcodeRegisterLocation);
