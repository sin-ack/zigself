// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("../debug.zig");
const Liveness = @import("./lowcode/Liveness.zig");
const RegisterPool = @import("./lowcode/RegisterPool.zig");
const AstCodeBlock = @import("./astcode/Block.zig");
const LowCodeBlock = @import("./lowcode/Block.zig");
const AstCodeExecutable = @import("./astcode/Executable.zig");
const LowCodeExecutable = @import("./lowcode/Executable.zig");
const AstCodeInstruction = @import("./astcode/Instruction.zig");
const LowCodeInstruction = @import("./lowcode/Instruction.zig");
const LowCodeRegisterLocation = @import("./lowcode/register_location.zig").RegisterLocation;

const LOW_EXECUTABLE_DUMP_DEBUG = debug.LOW_EXECUTABLE_DUMP_DEBUG;

pub fn lowerExecutable(allocator: Allocator, ast_executable: *AstCodeExecutable) !LowCodeExecutable.Ref {
    var low_executable = try LowCodeExecutable.create(allocator, ast_executable.definition_script);
    errdefer low_executable.unref();

    for (ast_executable.blocks.items) |block| {
        try lowerBlock(allocator, low_executable.value, block);
    }

    if (LOW_EXECUTABLE_DUMP_DEBUG)
        std.debug.print("Executable dump: {}\n", .{low_executable.value});

    return low_executable;
}

fn lowerBlock(allocator: Allocator, executable: *LowCodeExecutable, ast_block: *AstCodeBlock) !void {
    var liveness = try Liveness.analyzeBlock(allocator, ast_block);
    defer liveness.deinit(allocator);

    var register_pool = try RegisterPool.init(allocator);
    defer register_pool.deinit(allocator);

    const low_block_index = try executable.makeBlock();
    const low_block = executable.getBlock(low_block_index);

    for (ast_block.instructions.items) |inst, i| {
        try lowerInstruction(allocator, low_block, &liveness, &register_pool, inst);
        register_pool.expireOldIntervals(i);
    }

    try low_block.insertPushRegistersAtPrelude(allocator, register_pool.clobbered_registers);
}

fn lowerInstruction(allocator: Allocator, block: *LowCodeBlock, liveness: *Liveness, register_pool: *RegisterPool, inst: AstCodeInstruction) !void {
    switch (inst.tag) {
        .Send => {
            const payload = inst.payload(.Send);
            const receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.send(target, receiver_location, payload.message_name));
        },
        .PrimSend => {
            const payload = inst.payload(.PrimSend);
            const receiver_location = register_pool.getAllocatedRegisterFor(payload.receiver_location);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.primSend(target, receiver_location, payload.message_name));
        },
        .SelfSend => {
            const payload = inst.payload(.SelfSend);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.selfSend(target, payload.message_name));
        },
        .SelfPrimSend => {
            const payload = inst.payload(.SelfPrimSend);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.selfPrimSend(target, payload.message_name));
        },
        .PushConstantSlot => {
            const payload = inst.payload(.PushConstantSlot);
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, LowCodeInstruction.pushConstantSlot(.zero, name_location, payload.is_parent, value_location));
        },
        .PushAssignableSlot => {
            const payload = inst.payload(.PushAssignableSlot);
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, LowCodeInstruction.pushAssignableSlot(.zero, name_location, payload.is_parent, value_location));
        },
        .PushArgumentSlot => {
            const payload = inst.payload(.PushArgumentSlot);
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, LowCodeInstruction.pushArgumentSlot(.zero, name_location, value_location));
        },
        .PushInheritedSlot => {
            const payload = inst.payload(.PushInheritedSlot);
            const name_location = register_pool.getAllocatedRegisterFor(payload.name_location);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, LowCodeInstruction.pushInheritedSlot(.zero, name_location, value_location));
        },
        .CreateInteger => {
            const payload = inst.payload(.CreateInteger);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.createInteger(target, payload.value));
        },
        .CreateFloatingPoint => {
            const payload = inst.payload(.CreateFloatingPoint);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.createFloatingPoint(target, payload.value));
        },
        .CreateObject => {
            const payload = inst.payload(.CreateObject);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.createObject(target, payload.slot_count));
        },
        .CreateMethod => {
            const payload = inst.payload(.CreateMethod);
            const method_name_location = register_pool.getAllocatedRegisterFor(payload.method_name_location);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.createMethod(target, method_name_location, payload.slot_count, payload.block_index));
        },
        .CreateBlock => {
            const payload = inst.payload(.CreateBlock);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);
            const target_block_index = payload.block_index;

            try block.addInstruction(allocator, LowCodeInstruction.createBlock(target, payload.slot_count, target_block_index));
        },
        .CreateByteArray => {
            const payload = inst.payload(.CreateByteArray);
            const target = try register_pool.allocateRegister(allocator, block, liveness, inst.target);

            try block.addInstruction(allocator, LowCodeInstruction.createByteArray(target, payload.string));
        },
        .SetMethodInline => {
            try block.addInstruction(allocator, LowCodeInstruction.setMethodInline(.zero));
        },
        .ExitActivation => {
            const payload = inst.payload(.ExitActivation);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, LowCodeInstruction.writeReturnValue(.zero, value_location));
            try block.addInstruction(allocator, LowCodeInstruction.return_(.zero));
        },
        .NonlocalReturn => {
            const payload = inst.payload(.NonlocalReturn);
            const value_location = register_pool.getAllocatedRegisterFor(payload.value_location);

            try block.addInstruction(allocator, LowCodeInstruction.writeReturnValue(.zero, value_location));
            try block.addInstruction(allocator, LowCodeInstruction.nonlocalReturn(.zero));
        },
        .PushArg => {
            const payload = inst.payload(.PushArg);
            const argument_location = register_pool.getAllocatedRegisterFor(payload.argument_location);

            try block.addInstruction(allocator, LowCodeInstruction.pushArg(.zero, argument_location));
        },
        .SourceRange => {
            const range = inst.payload(.SourceRange);
            try block.addInstruction(allocator, LowCodeInstruction.sourceRange(.zero, range));
        },
        .PushArgumentSentinel => {
            try block.addInstruction(allocator, LowCodeInstruction.pushArgumentSentinel(.zero));
        },
        .PushSlotSentinel => {
            try block.addInstruction(allocator, LowCodeInstruction.pushSlotSentinel(.zero));
        },
        .VerifyArgumentSentinel => {
            try block.addInstruction(allocator, LowCodeInstruction.verifyArgumentSentinel(.zero));
        },
        .VerifySlotSentinel => {
            try block.addInstruction(allocator, LowCodeInstruction.verifySlotSentinel(.zero));
        },
    }
}
