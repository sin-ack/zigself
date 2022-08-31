// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../debug.zig");
const bytecode = @import("./bytecode.zig");
const Liveness = bytecode.lowcode.Liveness;
const RegisterPool = bytecode.lowcode.RegisterPool;

const LOW_EXECUTABLE_DUMP_DEBUG = debug.LOW_EXECUTABLE_DUMP_DEBUG;

pub fn lowerExecutable(allocator: Allocator, ast_executable: *bytecode.AstcodeExecutable) !bytecode.LowcodeExecutable.Ref {
    var low_executable = try bytecode.LowcodeExecutable.create(allocator, ast_executable.definition_script);
    errdefer low_executable.unref();

    for (ast_executable.blocks.items) |block| {
        try lowerBlock(allocator, low_executable.value, block);
    }

    if (LOW_EXECUTABLE_DUMP_DEBUG)
        std.debug.print("Executable dump: {}\n", .{low_executable.value});

    return low_executable;
}

fn lowerBlock(allocator: Allocator, executable: *bytecode.LowcodeExecutable, ast_block: *bytecode.AstcodeBlock) !void {
    var liveness = try Liveness.analyzeBlock(allocator, ast_block);
    defer liveness.deinit(allocator);

    var register_pool = try RegisterPool.init(allocator);
    defer register_pool.deinit(allocator);

    const low_block_index = try executable.makeBlock();
    const low_block = executable.getBlock(low_block_index);

    const push_registers_inst_offset = try low_block.reserveInstruction(allocator);

    for (ast_block.instructions.items) |inst, i| {
        try lowerInstruction(allocator, low_block, &liveness, &register_pool, inst);
        register_pool.expireOldIntervals(i);
    }

    low_block.instructions.items[push_registers_inst_offset] = bytecode.LowcodeInstruction.init(.zero, .{ .PushRegisters = register_pool.clobbered_registers });
}

fn lowerInstruction(
    allocator: Allocator,
    block: *bytecode.LowcodeBlock,
    liveness: *Liveness,
    register_pool: *RegisterPool,
    inst: bytecode.AstcodeInstruction,
) !void {
    const Instruction = bytecode.LowcodeInstruction;

    switch (inst.value) {
        .Send => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, Instruction.init(target, .{
                .Send = .{
                    .receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location),
                    .message_name = payload.message_name,
                },
            }));
        },
        .PrimSend => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, Instruction.init(target, .{
                .PrimSend = .{
                    .receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location),
                    .message_name = payload.message_name,
                },
            }));
        },
        .SelfSend => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, Instruction.init(target, .{
                .SelfSend = .{ .message_name = payload.message_name },
            }));
        },
        .SelfPrimSend => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, Instruction.init(target, .{
                .SelfPrimSend = .{ .message_name = payload.message_name },
            }));
        },
        .PushConstantSlot => |payload| {
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, Instruction.init(.zero, .{ .PushConstantSlot = .{
                .name_location = name_location,
                .value_location = value_location,
                .is_parent = payload.is_parent,
            } }));
        },
        .PushAssignableSlot => |payload| {
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, Instruction.init(.zero, .{ .PushAssignableSlot = .{
                .name_location = name_location,
                .value_location = value_location,
                .is_parent = payload.is_parent,
            } }));
        },
        .PushArgumentSlot => |payload| {
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, Instruction.init(.zero, .{ .PushArgumentSlot = .{
                .name_location = name_location,
                .value_location = value_location,
            } }));
        },
        .PushInheritedSlot => |payload| {
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, Instruction.init(.zero, .{ .PushInheritedSlot = .{
                .name_location = name_location,
                .value_location = value_location,
            } }));
        },
        .CreateInteger => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            try block.addInstruction(allocator, Instruction.init(target, .{ .CreateInteger = payload }));
        },
        .CreateFloatingPoint => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            try block.addInstruction(allocator, Instruction.init(target, .{ .CreateFloatingPoint = payload }));
        },
        .CreateByteArray => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            try block.addInstruction(allocator, Instruction.init(target, .{ .CreateByteArray = payload }));
        },
        .CreateObject => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, Instruction.init(target, .{
                .CreateObject = .{ .slot_count = payload.slot_count },
            }));
        },
        .CreateMethod => |payload| {
            const method_name_location = register_pool.getAllocatedRegisterFor(payload.method_name_location);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, Instruction.init(target, .{
                .CreateMethod = .{
                    .method_name_location = method_name_location,
                    .slot_count = payload.slot_count,
                    .block_index = payload.block_index,
                },
            }));
        },
        .CreateBlock => |payload| {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, Instruction.init(target, .{
                .CreateBlock = .{
                    .slot_count = payload.slot_count,
                    .block_index = payload.block_index,
                },
            }));
        },
        .SetMethodInline => {
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .SetMethodInline = {} }));
        },
        .Return => |payload| {
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .Return = .{ .value_location = value_location } }));
        },
        .NonlocalReturn => |payload| {
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .NonlocalReturn = .{ .value_location = value_location } }));
        },
        .PushArg => |payload| {
            const argument_location = register_pool.getAllocatedRegisterFor(payload.argument_location);
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .PushArg = .{ .argument_location = argument_location } }));
        },
        .SourceRange => |payload| {
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .SourceRange = payload }));
        },
        .PushArgumentSentinel => {
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .PushArgumentSentinel = {} }));
        },
        .PushSlotSentinel => {
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .PushSlotSentinel = {} }));
        },
        .VerifyArgumentSentinel => {
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .VerifyArgumentSentinel = {} }));
        },
        .VerifySlotSentinel => {
            try block.addInstruction(allocator, Instruction.init(.zero, .{ .VerifySlotSentinel = {} }));
        },
        .PushRegisters => unreachable,
    }
}
