// Copyright (c) 2022-2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../../debug.zig");
const bytecode = @import("../bytecode.zig");
const Liveness = bytecode.astcode.Liveness;
const RegisterPool = bytecode.lowcode.RegisterPool;

const LOW_EXECUTABLE_DUMP_DEBUG = debug.LOW_EXECUTABLE_DUMP_DEBUG;

pub fn lowerExecutable(allocator: Allocator, ast_executable: *bytecode.astcode.Executable) !bytecode.lowcode.Executable.Ref {
    var low_executable = try bytecode.lowcode.Executable.create(allocator, ast_executable.definition_script);
    errdefer low_executable.unref();

    for (ast_executable.blocks.items) |block| {
        try lowerBlock(allocator, low_executable.value, block);
    }

    if (LOW_EXECUTABLE_DUMP_DEBUG)
        std.debug.print("Executable dump: {}\n", .{low_executable.value});

    return low_executable;
}

fn lowerBlock(allocator: Allocator, executable: *bytecode.lowcode.Executable, ast_block: *bytecode.astcode.Block) !void {
    var liveness = try Liveness.analyzeBlock(allocator, ast_block);
    defer liveness.deinit(allocator);

    var register_pool = try RegisterPool.init(allocator);
    defer register_pool.deinit(allocator);

    const low_block_index = try executable.makeBlock();
    const low_block = executable.getBlock(low_block_index);

    const push_registers_inst_offset = try low_block.reserveInstruction(allocator);

    for (ast_block.instructions.items, 0..) |inst, i| {
        try lowerInstruction(allocator, low_block, &liveness, &register_pool, inst);
        register_pool.expireOldIntervals(i);
    }

    // TODO: better source location
    low_block.setInstruction(push_registers_inst_offset, .PushRegisters, .zero, register_pool.clobbered_registers, ast_block.instructions.items[0].source_range);
    low_block.seal();
}

fn lowerInstruction(
    allocator: Allocator,
    block: *bytecode.lowcode.Block,
    liveness: *Liveness,
    register_pool: *RegisterPool,
    inst: bytecode.astcode.Instruction,
) !void {
    switch (inst.opcode) {
        .Send => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            const payload = inst.payload.Send;

            try block.addInstruction(allocator, .Send, target, .{
                .receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location),
                .message_name = payload.message_name,
            }, inst.source_range);
        },
        .PrimSend => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            const payload = inst.payload.Send;

            try block.addInstruction(allocator, .PrimSend, target, .{
                .receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location),
                .message_name = payload.message_name,
            }, inst.source_range);
        },
        .SelfSend => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            const payload = inst.payload.SelfSend;

            try block.addInstruction(allocator, .SelfSend, target, .{ .message_name = payload.message_name }, inst.source_range);
        },
        .SelfPrimSend => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            const payload = inst.payload.SelfSend;

            try block.addInstruction(allocator, .SelfPrimSend, target, .{ .message_name = payload.message_name }, inst.source_range);
        },
        .PushConstantSlot => {
            const payload = inst.payload.PushParentableSlot;
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, .PushConstantSlot, .zero, .{
                .name_location = name_location,
                .value_location = value_location,
                .is_parent = payload.is_parent,
            }, inst.source_range);
        },
        .PushAssignableSlot => {
            const payload = inst.payload.PushParentableSlot;
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, .PushAssignableSlot, .zero, .{
                .name_location = name_location,
                .value_location = value_location,
                .is_parent = payload.is_parent,
            }, inst.source_range);
        },
        .PushArgumentSlot => {
            const payload = inst.payload.PushNonParentSlot;
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, .PushArgumentSlot, .zero, .{
                .name_location = name_location,
                .value_location = value_location,
            }, inst.source_range);
        },
        .CreateInteger => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            try block.addInstruction(allocator, .CreateInteger, target, inst.payload.CreateInteger, inst.source_range);
        },
        .CreateFloatingPoint => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            try block.addInstruction(allocator, .CreateFloatingPoint, target, inst.payload.CreateFloatingPoint, inst.source_range);
        },
        .CreateByteArray => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            try block.addInstruction(allocator, .CreateByteArray, target, inst.payload.CreateByteArray, inst.source_range);
        },
        .CreateObject => {
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            try block.addInstruction(allocator, .CreateObject, target, .{
                .slot_count = inst.payload.CreateObject.slot_count,
            }, inst.source_range);
        },
        .CreateMethod => {
            const payload = inst.payload.CreateMethod;
            const method_name_location = register_pool.getAllocatedRegisterFor(payload.method_name_location);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, .CreateMethod, target, .{
                .method_name_location = method_name_location,
                .slot_count = payload.slot_count,
                .block_index = payload.block_index,
            }, inst.source_range);
        },
        .CreateBlock => {
            const payload = inst.payload.CreateBlock;
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, .CreateBlock, target, .{
                .slot_count = payload.slot_count,
                .block_index = payload.block_index,
            }, inst.source_range);
        },
        .SetMethodInline => {
            try block.addInstruction(allocator, .SetMethodInline, .zero, {}, inst.source_range);
        },
        .Return => {
            const value_location = register_pool.getAllocatedRegisterFor(inst.payload.Return.value_location);
            try block.addInstruction(allocator, .Return, .zero, .{ .value_location = value_location }, inst.source_range);
        },
        .NonlocalReturn => {
            const value_location = register_pool.getAllocatedRegisterFor(inst.payload.Return.value_location);
            try block.addInstruction(allocator, .NonlocalReturn, .zero, .{ .value_location = value_location }, inst.source_range);
        },
        .PushArg => {
            const argument_location = register_pool.getAllocatedRegisterFor(inst.payload.PushArg.argument_location);
            try block.addInstruction(allocator, .PushArg, .zero, .{ .argument_location = argument_location }, inst.source_range);
        },
        .PushArgumentSentinel => {
            try block.addInstruction(allocator, .PushArgumentSentinel, .zero, {}, inst.source_range);
        },
        .PushSlotSentinel => {
            try block.addInstruction(allocator, .PushSlotSentinel, .zero, {}, inst.source_range);
        },
        .VerifyArgumentSentinel => {
            try block.addInstruction(allocator, .VerifyArgumentSentinel, .zero, {}, inst.source_range);
        },
        .VerifySlotSentinel => {
            try block.addInstruction(allocator, .VerifySlotSentinel, .zero, {}, inst.source_range);
        },
        .PushRegisters => unreachable,
    }
}
