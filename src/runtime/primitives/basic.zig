// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Slot = @import("../slot.zig").Slot;
const Actor = @import("../Actor.zig");
const Script = @import("../../language/script.zig");
const AstGen = @import("../bytecode/AstGen.zig");
const CodeGen = @import("../bytecode/CodeGen.zig");
const SlotsMap = @import("../objects/slots.zig").SlotsMap;
const MethodMap = @import("../objects/method.zig").MethodMap;
const ByteArray = @import("../ByteArray.zig");
const Completion = @import("../Completion.zig");
const ActorObject = @import("../objects/actor.zig").Actor;
const SlotsObject = @import("../objects/slots.zig").Slots;
const interpreter = @import("../interpreter.zig");
const MethodObject = @import("../objects/method.zig").Method;
const object_bless = @import("../object_bless.zig");
const error_set_utils = @import("../../utility/error_set.zig");
const ActorProxyObject = @import("../objects/actor_proxy.zig").ActorProxy;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

const ExecutionResult = interpreter.ExecutionResult;
const runtime_error = @import("../error.zig");

/// Return the static "nil" slots object.
pub fn Nil(context: *PrimitiveContext) !ExecutionResult {
    return ExecutionResult.completion(Completion.initNormal(context.vm.nil()));
}

/// Return the given path relative to the current activation's definition
/// script's running path. The caller must free the returned slice.
fn getRelativePathToScript(context: *PrimitiveContext, path: []const u8) ![]const u8 {
    const current_activation = context.actor.activation_stack.getCurrent();
    const current_script = current_activation.definitionExecutable().value.definition_script.value;
    const running_script_path = current_script.running_path;

    const paths_to_join = &[_][]const u8{ std.fs.path.dirname(running_script_path) orelse ".", path };
    const target_path = try std.fs.path.join(context.vm.allocator, paths_to_join);
    return target_path;
}

/// Run the given script file, and return the result of the last expression.
pub fn RunScript(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_RunScript:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);

    // FIXME: Find a way to handle errors here. These hacks are nasty.

    const target_path = try getRelativePathToScript(context, receiver.getValues());
    defer context.vm.allocator.free(target_path);

    var script = Script.createFromFilePath(context.vm.allocator, target_path) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |e| return ExecutionResult.completion(try Completion.initRuntimeError(
            context.vm,
            context.source_range,
            "An unexpected error was raised from script.initInPlaceFromFilePath: {s}",
            .{@errorName(e)},
        )),
    };
    defer script.unref();

    const did_parse_without_errors = script.value.parseScript() catch |err| switch (err) {
        error.OutOfMemory => return err,
    };

    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;
    if (!did_parse_without_errors) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Failed parsing the script passed to _RunScript", .{}),
        );
    }

    const ast_executable = AstGen.generateExecutableFromScript(context.vm.allocator, script) catch |err| switch (err) {
        error.AstGenFailure => return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Code generation for the script passed to _RunScript failed", .{}),
        ),
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer ast_executable.unref();

    const executable = try CodeGen.lowerExecutable(context.vm.allocator, ast_executable.value);
    defer executable.unref();

    // Advance the instruction for the activation that will be returned to.
    context.actor.activation_stack.getCurrent().advanceInstruction();

    try context.actor.activation_stack.pushSubEntrypointActivation(context.vm, context.target_location, executable);
    return ExecutionResult.activationChange();
}

pub fn EvaluateStringContext_IfFail(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_EvaluateStringContext:IfFail:");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .ByteArray);
    const failure_block = arguments.getValue(1);

    const running_script_path = context.actor.activation_stack.getCurrent().definitionExecutable().value.definition_script.value.running_path;
    var script = try Script.createFromString(context.vm.allocator, running_script_path, receiver.getValues());
    defer script.unref();

    const did_parse_without_errors = try script.value.parseScript();

    script.value.reportDiagnostics(std.io.getStdErr().writer()) catch unreachable;
    if (!did_parse_without_errors) {
        // TODO: Pass error information to the failure block.
        if (try interpreter.sendMessage(
            context.vm,
            context.actor,
            failure_block,
            "value",
            context.target_location,
            context.source_range,
        )) |completion| {
            return ExecutionResult.completion(completion);
        }
        return ExecutionResult.activationChange();
    }

    const ast_executable = AstGen.generateExecutableFromScript(context.vm.allocator, script) catch |err| switch (err) {
        error.AstGenFailure => {
            // TODO: Pass error information to the failure block.
            std.debug.print("Code generation for the script passed to _EvaluateStringIfFail: failed", .{});
            if (try interpreter.sendMessage(
                context.vm,
                context.actor,
                failure_block,
                "value",
                context.target_location,
                context.source_range,
            )) |completion| {
                return ExecutionResult.completion(completion);
            }
            return ExecutionResult.activationChange();
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer ast_executable.unref();

    const executable = try CodeGen.lowerExecutable(context.vm.allocator, ast_executable.value);
    defer executable.unref();

    const required_memory_for_actor_context =
        ByteArray.requiredSizeForAllocation("evaluate".len) +
        MethodMap.requiredSizeForAllocation(0) +
        MethodObject.requiredSizeForAllocation(0);
    const required_memory_for_spawn = ActorObject.requiredSizeForAllocation() +
        ActorProxyObject.requiredSizeForAllocation();
    var token = try context.vm.heap.getAllocation(required_memory_for_actor_context + required_memory_for_spawn);
    defer token.deinit();

    const evaluation_context = arguments.getValue(0);
    const genesis_actor = context.vm.genesis_actor.?;

    // Create the actor object.
    // NOTE: evaluation_context is a placeholder until we get the actor ID.
    const new_actor = try Actor.create(context.vm, &token, evaluation_context);
    errdefer new_actor.destroy(context.vm.allocator);

    try context.vm.registerRegularActor(new_actor);

    const new_actor_proxy = ActorProxyObject.create(context.vm.getMapMap(), &token, context.actor.id, new_actor.actor_object.get());

    const evaluate_method_name = ByteArray.createFromString(&token, "evaluate");
    const evaluate_method_map = try MethodMap.create(context.vm.getMapMap(), &token, 0, 0, false, evaluate_method_name, executable.value.getEntrypointBlock(), executable);
    var evaluate_method = MethodObject.create(&token, new_actor.id, evaluate_method_map, &.{});

    // Bless the evaluation context so that we can send it to the new actor.
    const blessed_evaluation_context = try object_bless.bless(context.vm.heap, new_actor.id, evaluation_context);
    new_actor.actor_object.get().context = blessed_evaluation_context;

    // NOTE: Need to advance the current actor to the next instruction to be returned to after the this actor exits.
    context.actor.activation_stack.getCurrent().advanceInstruction();

    context.actor.writeRegister(context.target_location, new_actor_proxy.asValue());
    context.actor.yield_reason = .ActorSpawned;
    context.vm.switchToActor(genesis_actor);

    token.deinit();
    token = token: {
        var tracked_method = try context.vm.heap.track(evaluate_method.asValue());
        defer tracked_method.untrack(context.vm.heap);

        var inner_token = try context.vm.heap.getAllocation(evaluate_method.requiredSizeForActivation());

        evaluate_method = tracked_method.getValue().asObject().mustBeType(.Method);
        break :token inner_token;
    };

    try new_actor.activateMethod(context.vm, &token, evaluate_method, context.target_location, context.source_range);

    // Since we are switching actors (which we should in the "spawning an actor
    // in another actor" case), we cannot return the new actor object normally.
    // What we need to do instead is to write the new actor object to the target
    // location of either the _ActorResume or the _ActorSpawn: prim_send
    // instruction, which will be the instruction that's before the current one
    // (because we advance the pc in each of the aforementioned primitives).
    const genesis_current_activation = genesis_actor.activation_stack.getCurrent();
    const genesis_pc_before_last = genesis_current_activation.pc - 1;
    const genesis_activation_object = genesis_current_activation.activation_object.get();
    const genesis_definition_block = genesis_activation_object.getBytecodeBlock();
    const genesis_inst_before_last = genesis_definition_block.getInstruction(genesis_pc_before_last);

    genesis_actor.writeRegister(genesis_inst_before_last.target, new_actor.actor_object.value);

    return ExecutionResult.actorSwitch();
}

/// Raise the argument as an error. The argument must be a byte vector.
pub fn Error(context: *PrimitiveContext) !ExecutionResult {
    const arguments = context.getArguments("_Error:");
    const message = try arguments.getObject(0, .ByteArray);

    return ExecutionResult.completion(try Completion.initRuntimeError(
        context.vm,
        context.source_range,
        "Error raised in Self code: {s}",
        .{message.getValues()},
    ));
}

/// Restarts the current method, executing it from the first statement.
/// This primitive is intended to be used internally only.
pub fn Restart(context: *PrimitiveContext) !ExecutionResult {
    _ = context;
    return ExecutionResult.completion(Completion.initRestart());
}
