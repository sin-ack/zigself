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
    // FIXME: Is there a way we can avoid copying this manually?
    for (ast_executable.object_descriptors.items, 0..) |descriptor, i| {
        const descriptor_copy = try descriptor.copy(allocator);
        errdefer descriptor_copy.deinit(allocator);

        const new_index = try low_executable.value.addObjectDescriptor(descriptor_copy);
        std.debug.assert(new_index == i);
    }

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
    // FIXME: Is there a way we can avoid setting this manually?
    low_block.send_count = ast_block.send_count;

    const push_registers_inst_offset = try low_block.reserveInstruction(allocator);

    for (0..ast_block.getLength()) |index| {
        try lowerInstruction(allocator, ast_block, index, low_block, &liveness, &register_pool);
        register_pool.expireOldIntervals(index);
    }

    // TODO: better source location
    low_block.setInstruction(push_registers_inst_offset, .PushRegisters, .zero, register_pool.clobbered_registers, ast_block.getSourceRange(push_registers_inst_offset));
    low_block.seal();
}

fn lowerInstruction(
    allocator: Allocator,
    ast_block: *bytecode.astcode.Block,
    index: usize,
    low_block: *bytecode.lowcode.Block,
    liveness: *Liveness,
    register_pool: *RegisterPool,
) !void {
    switch (ast_block.getOpcode(index)) {
        .Send => {
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            const payload = ast_block.getTypedPayload(index, .Send);

            try low_block.addInstruction(allocator, .Send, target, .{
                .receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location),
                .selector = payload.selector,
                .send_index = payload.send_index,
            }, ast_block.getSourceRange(index));
        },
        .PrimSend => {
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            const payload = ast_block.getTypedPayload(index, .PrimSend);

            try low_block.addInstruction(allocator, .PrimSend, target, .{
                .receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location),
                .index = payload.index,
            }, ast_block.getSourceRange(index));
        },
        .SelfSend => {
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            const payload = ast_block.getTypedPayload(index, .SelfSend);

            try low_block.addInstruction(allocator, .SelfSend, target, .{
                .selector = payload.selector,
                .send_index = payload.send_index,
            }, ast_block.getSourceRange(index));
        },
        .SelfPrimSend => {
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            const payload = ast_block.getTypedPayload(index, .SelfPrimSend);

            try low_block.addInstruction(allocator, .SelfPrimSend, target, .{ .index = payload.index }, ast_block.getSourceRange(index));
        },
        .CreateInteger => {
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            try low_block.addInstruction(allocator, .CreateInteger, target, ast_block.getTypedPayload(index, .CreateInteger), ast_block.getSourceRange(index));
        },
        .CreateFloatingPoint => {
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            try low_block.addInstruction(allocator, .CreateFloatingPoint, target, ast_block.getTypedPayload(index, .CreateFloatingPoint), ast_block.getSourceRange(index));
        },
        .CreateByteArray => {
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            try low_block.addInstruction(allocator, .CreateByteArray, target, ast_block.getTypedPayload(index, .CreateByteArray), ast_block.getSourceRange(index));
        },
        .CreateObject => {
            const payload = ast_block.getTypedPayload(index, .CreateObject);
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));
            try low_block.addInstruction(allocator, .CreateObject, target, .{
                .descriptor_index = payload.descriptor_index,
            }, ast_block.getSourceRange(index));
        },
        .CreateMethod => {
            const payload = ast_block.getTypedPayload(index, .CreateMethod);
            const method_name_location = register_pool.getAllocatedRegisterFor(payload.method_name_location);
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));

            try low_block.addInstruction(allocator, .CreateMethod, target, .{
                .method_name_location = method_name_location,
                .descriptor_index = payload.descriptor_index,
                .block_index = payload.block_index,
            }, ast_block.getSourceRange(index));
        },
        .CreateBlock => {
            const payload = ast_block.getTypedPayload(index, .CreateBlock);
            const target = try register_pool.allocateRegister(allocator, low_block, liveness, ast_block.getTargetLocation(index));

            try low_block.addInstruction(allocator, .CreateBlock, target, .{
                .descriptor_index = payload.descriptor_index,
                .block_index = payload.block_index,
            }, ast_block.getSourceRange(index));
        },
        .SetMethodInline => {
            try low_block.addInstruction(allocator, .SetMethodInline, .zero, {}, ast_block.getSourceRange(index));
        },
        .Return => {
            const value_location = register_pool.getAllocatedRegisterFor(ast_block.getTypedPayload(index, .Return).value_location);
            try low_block.addInstruction(allocator, .Return, .zero, .{ .value_location = value_location }, ast_block.getSourceRange(index));
        },
        .NonlocalReturn => {
            const value_location = register_pool.getAllocatedRegisterFor(ast_block.getTypedPayload(index, .NonlocalReturn).value_location);
            try low_block.addInstruction(allocator, .NonlocalReturn, .zero, .{ .value_location = value_location }, ast_block.getSourceRange(index));
        },
        .PushArg => {
            const argument_location = register_pool.getAllocatedRegisterFor(ast_block.getTypedPayload(index, .PushArg).argument_location);
            try low_block.addInstruction(allocator, .PushArg, .zero, .{ .argument_location = argument_location }, ast_block.getSourceRange(index));
        },
        .PushArgumentSentinel => {
            try low_block.addInstruction(allocator, .PushArgumentSentinel, .zero, {}, ast_block.getSourceRange(index));
        },
        .VerifyArgumentSentinel => {
            try low_block.addInstruction(allocator, .VerifyArgumentSentinel, .zero, {}, ast_block.getSourceRange(index));
        },
        .PushRegisters => unreachable,
    }
}
