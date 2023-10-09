// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Actor = @import("../Actor.zig");
const Value = @import("../value.zig").Value;
const bless = @import("../object_bless.zig");
const Completion = @import("../Completion.zig");
const ActorObject = @import("../objects/actor.zig").Actor;
const SourceRange = @import("../SourceRange.zig");
const MethodObject = @import("../objects/method.zig").Method;
const runtime_error = @import("../error.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ExecutionResult = @import("../interpreter.zig").ExecutionResult;
const ActorProxyObject = @import("../objects/actor_proxy.zig").ActorProxy;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Find the given selector as a method suitable for spawning an actor.
///
/// Returns an Completion if something other than a method is found (not
/// necessarily an error). Writes the method to out_method on success.
fn findActorMethod(
    vm: *VirtualMachine,
    source_range: SourceRange,
    receiver: Value,
    selector: []const u8,
    out_method: **MethodObject,
) !?Completion {
    const method = method: {
        switch (receiver.lookup(vm, selector)) {
            .Nothing => {
                return try Completion.initRuntimeError(vm, source_range, "Unknown selector '{s}'", .{selector});
            },
            .Assignment => {
                return try Completion.initRuntimeError(vm, source_range, "Spawning actor with non-unary method '{s}' not permitted", .{selector});
            },
            .Regular => |lookup_result| {
                if (lookup_result.isObjectReference()) {
                    if (lookup_result.asObject().asType(.Method)) |lookup_result_as_method| {
                        if (lookup_result_as_method.getArgumentSlotCount() != 0) {
                            return try Completion.initRuntimeError(vm, source_range, "Spawning actor with non-unary method '{s}' not permitted", .{selector});
                        }

                        break :method lookup_result_as_method;
                    }
                }

                return Completion.initNormal(lookup_result);
            },
            .ActorMessage => {
                return try Completion.initRuntimeError(vm, source_range, "Spawning actor by sending message to actor proxy not permitted", .{});
            },
        }
    };

    out_method.* = method;
    return null;
}

/// Create a new actor which then becomes the genesis actor.
pub fn Genesis(context: *PrimitiveContext) !ExecutionResult {
    if (context.vm.isInActorMode()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_Genesis: sent while the VM is in actor mode", .{}),
        );
    }

    const arguments = context.getArguments("_Genesis:");
    var receiver = context.receiver.getValue();
    const selector = (try arguments.getObject(0, .ByteArray)).getValues();

    // FIXME: We should perhaps allow any object to be the genesis actor
    //        context, so blocks can also work for example.
    if (!(receiver.isObjectReference() and receiver.asObject().object_information.object_type == .Slots)) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected receiver of _Genesis: to be a slots object", .{}),
        );
    }

    var method: *MethodObject = undefined;
    if (try findActorMethod(
        context.vm,
        context.source_range,
        receiver,
        selector,
        &method,
    )) |completion| {
        return ExecutionResult.completion(completion);
    }

    var token = token: {
        var tracked_method = try context.vm.heap.track(method.asValue());
        defer tracked_method.untrack(context.vm.heap);

        var token = try context.vm.heap.getAllocation(
            method.requiredSizeForActivation() +
                ActorObject.requiredSizeForAllocation(),
        );

        method = tracked_method.getValue().asObject().mustBeType(.Method);
        receiver = context.receiver.getValue();

        break :token token;
    };
    defer token.deinit();

    // NOTE: Need to advance the global actor to the next instruction to be returned to after the genesis actor exits.
    _ = context.vm.current_actor.activation_stack.getCurrent().advanceInstruction();

    // NOTE: The receiver here is passed as a dummy in order to get the new actor ID.
    const genesis_actor = try Actor.create(context.vm, &token, receiver);

    const blessed_receiver = blessed_receiver: {
        var tracked_method = try context.vm.heap.track(method.asValue());
        defer tracked_method.untrack(context.vm.heap);

        const blessed_receiver = try bless.bless(context.vm, context.vm.heap, genesis_actor.id, receiver);

        method = tracked_method.getValue().asObject().mustBeType(.Method);

        break :blessed_receiver blessed_receiver;
    };

    genesis_actor.actor_object.get().context = blessed_receiver;
    try genesis_actor.activateMethod(context.vm, &token, method, context.target_location, context.source_range);
    context.vm.setGenesisActor(genesis_actor);
    context.vm.switchToActor(genesis_actor);

    return ExecutionResult.actorSwitch();
}

/// Spawn a new actor by creating an Actor and its associated actor context
/// after sending the message given in the first argument to it. If this is sent
/// in a regular actor, then additionally create an ActorProxy object and write
/// it to the result location of the primitive.
pub fn ActorSpawn(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInActorMode()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorSpawn: sent while the VM is not in actor mode", .{}),
        );
    }

    const arguments = context.getArguments("_ActorSpawn:");
    var receiver = context.receiver.getValue();
    const spawn_selector = (try arguments.getObject(0, .ByteArray)).getValues();

    const genesis_actor = context.vm.genesis_actor.?;

    // FIXME: We should perhaps allow any object to receive a message context,
    //        so blocks can also work for example.
    if (!(receiver.isObjectReference() and receiver.asObject().object_information.object_type == .Slots)) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected receiver of _ActorSpawn: to be a slots object", .{}),
        );
    }

    // NOTE: Need to advance the current actor to the next instruction to be returned to after the this actor exits.
    _ = context.actor.activation_stack.getCurrent().advanceInstruction();

    var spawn_method: *MethodObject = undefined;
    if (try findActorMethod(
        context.vm,
        context.source_range,
        receiver,
        spawn_selector,
        &spawn_method,
    )) |completion| {
        switch (completion.data) {
            .Normal => {
                return ExecutionResult.completion(
                    try Completion.initRuntimeError(
                        context.vm,
                        context.source_range,
                        "The actor spawn message pointed to a non-method slot. This is not allowed as the spawned actor does not own that value.",
                        .{},
                    ),
                );
            },
            .RuntimeError => return ExecutionResult.completion(completion),
        }
    }

    var token = token: {
        var tracked_method = try context.vm.heap.track(spawn_method.asValue());
        defer tracked_method.untrack(context.vm.heap);

        var token = try context.vm.heap.getAllocation(
            spawn_method.requiredSizeForActivation(),
        );

        spawn_method = tracked_method.getValue().asObject().mustBeType(.Method);
        receiver = context.receiver.getValue();

        break :token token;
    };
    defer token.deinit();

    const stack_snapshot = context.vm.takeStackSnapshot();
    var before_spawn_method_activation = context.actor.activation_stack.getCurrent();
    const before_spawn_method_activation_ref = before_spawn_method_activation.takeRef(context.actor.activation_stack);

    // Activate the method in the actor which requested the spawn. This is required because
    // any new actor we create does not have any memory of its own yet, so we need to create a
    // new context for it before we can do anything.
    try context.actor.activateMethodWithContext(context.vm, &token, receiver, spawn_method, context.target_location, context.source_range);

    var actor_result = try context.actor.executeUntil(context.vm, before_spawn_method_activation_ref);
    // This object is owned by the actor that requested the spawn.
    var new_actor_context = switch (actor_result) {
        .ActorSwitch => {
            return ExecutionResult.completion(
                try Completion.initRuntimeError(context.vm, context.source_range, "The actor spawn activation caused an actor switch", .{}),
            );
        },
        .Completion => |*completion| value: {
            defer completion.deinit(context.vm);

            switch (completion.data) {
                .Normal => |value| break :value value,
                .RuntimeError => |err| {
                    context.vm.switchToActor(genesis_actor);

                    // Refresh activation pointer
                    before_spawn_method_activation = before_spawn_method_activation_ref.get(context.actor.activation_stack).?;

                    if (!context.vm.silent_errors) {
                        std.debug.print("Received error at top level while spawning actor: {s}\n", .{err.message});
                        runtime_error.printTraceFromActivationStackUntil(context.actor.activation_stack.getStack(), err.source_range, before_spawn_method_activation);
                    }

                    context.actor.activation_stack.restoreTo(before_spawn_method_activation);
                    context.vm.restoreStackSnapshot(stack_snapshot);

                    context.actor.yield_reason = .RuntimeError;
                    // FIXME: What is a sensible return value here?
                    return ExecutionResult.completion(Completion.initNormal(context.vm.nil()));
                },
            }
        },
    };

    // Refresh pointers in case the actor execution caused a GC
    receiver = context.receiver.getValue();

    var entrypoint_selector = entrypoint_selector: {
        if (context.actor.entrypoint_selector) |message_value| {
            break :entrypoint_selector message_value.get().getValues();
        }

        return ExecutionResult.completion(
            try Completion.initRuntimeError(
                context.vm,
                context.source_range,
                "The actor did not set an entrypoint selector during its spawn activation",
                .{},
            ),
        );
    };

    var entrypoint_method: *MethodObject = undefined;

    if (try findActorMethod(
        context.vm,
        context.source_range,
        receiver,
        entrypoint_selector,
        &entrypoint_method,
    )) |completion| {
        return ExecutionResult.completion(completion);
    }

    context.actor.entrypoint_selector = null;

    token.deinit();
    token = token: {
        var tracked_method = try context.vm.heap.track(entrypoint_method.asValue());
        defer tracked_method.untrack(context.vm.heap);
        var tracked_new_actor_context = try context.vm.heap.track(new_actor_context);
        defer tracked_new_actor_context.untrack(context.vm.heap);

        var required_memory = ActorObject.requiredSizeForAllocation();
        if (context.actor != genesis_actor)
            required_memory += ActorProxyObject.requiredSizeForAllocation();

        var inner_token = try context.vm.heap.getAllocation(required_memory);

        entrypoint_method = tracked_method.getValue().asObject().mustBeType(.Method);
        new_actor_context = tracked_new_actor_context.getValue();
        receiver = context.receiver.getValue();

        break :token inner_token;
    };

    // NOTE: new_actor_context is a placeholder until the blessing operation happens, because we need the
    //       new actor's ID.
    const new_actor = try Actor.create(context.vm, &token, new_actor_context);
    errdefer new_actor.destroy(context.vm.allocator);

    try context.vm.registerRegularActor(new_actor);

    // Create a new ActorProxy object and write it to the result location of the
    // actor who spawned it, if it wasn't the genesis actor that spawned the
    // new actor.
    if (context.actor != genesis_actor) {
        const new_actor_proxy = ActorProxyObject.create(context.vm.getMapMap(), &token, context.actor.id, new_actor.actor_object.get());
        context.actor.writeRegister(context.target_location, new_actor_proxy.asValue());
        context.actor.yield_reason = .ActorSpawned;
        context.vm.switchToActor(genesis_actor);
    }

    // Bless the new actor context
    const blessed_new_actor_context = blessed_new_actor_context: {
        var tracked_method = try context.vm.heap.track(entrypoint_method.asValue());
        defer tracked_method.untrack(context.vm.heap);

        const blessed_new_actor_context = try bless.bless(context.vm, context.vm.heap, new_actor.id, new_actor_context);

        entrypoint_method = tracked_method.getValue().asObject().mustBeType(.Method);

        break :blessed_new_actor_context blessed_new_actor_context;
    };
    new_actor.actor_object.get().context = blessed_new_actor_context;

    token.deinit();
    token = token: {
        var tracked_method = try context.vm.heap.track(entrypoint_method.asValue());
        defer tracked_method.untrack(context.vm.heap);

        var inner_token = try context.vm.heap.getAllocation(entrypoint_method.requiredSizeForActivation());

        entrypoint_method = tracked_method.getValue().asObject().mustBeType(.Method);
        break :token inner_token;
    };

    try new_actor.activateMethod(context.vm, &token, entrypoint_method, context.target_location, context.source_range);

    // Since we are switching actors (which we should in the "spawning an actor
    // in another actor" case), we cannot return the new actor object normally.
    // What we need to do instead is to write the new actor object to the target
    // location of either the _ActorResume or the _ActorSpawn: prim_send
    // instruction, which will be the instruction that's before the current one
    // (because we advance the pc in each of the aforementioned primitives).
    const genesis_current_activation = genesis_actor.activation_stack.getCurrent();
    const genesis_pc_before_last = genesis_current_activation.instruction_index - 1;
    const genesis_activation_object = genesis_current_activation.activation_object.get();
    const genesis_definition_block = genesis_activation_object.getBytecodeBlock();
    const genesis_inst_before_last_target = genesis_definition_block.getTargetLocation(genesis_pc_before_last);

    genesis_actor.writeRegister(genesis_inst_before_last_target, new_actor.actor_object.value);

    return ExecutionResult.actorSwitch();
}

/// Sets the actor's entrypoint selector, which is the message that is sent to
/// the activation once the spawn activation is complete (in order to prime it
/// for its resuming).
pub fn ActorSetEntrypoint(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInActorMode()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorSetEntrypoint: sent outside of actor mode", .{}),
        );
    }

    const arguments = context.getArguments("_ActorSetEntrypoint:");
    const entrypoint_selector_name = try arguments.getObject(0, .ByteArray);

    context.actor.entrypoint_selector = .{ .value = entrypoint_selector_name.asValue() };
    return ExecutionResult.completion(Completion.initNormal(context.vm.nil()));
}

/// Resume the activation from where it last left off.
pub fn ActorResume(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorResume sent outside of the genesis actor", .{}),
        );
    }

    const arguments = context.getArguments("_ActorResume");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Actor);

    const actor = receiver.getActor();
    std.debug.assert(context.vm.regularActorIsRegistered(actor));

    // NOTE: Need to advance the current actor to the next instruction to be returned to after the this actor exits.
    _ = context.actor.activation_stack.getCurrent().advanceInstruction();

    // Preemptively write a nil to the _ActorResume location so we don't hold
    // onto a temporary accidentally.
    context.actor.writeRegister(context.target_location, context.vm.nil());

    switch (actor.yield_reason) {
        .None, .Yielded, .Blocked, .ActorSpawned => {
            context.vm.switchToActor(actor);
            return ExecutionResult.actorSwitch();
        },
        .RuntimeError => {
            return ExecutionResult.completion(
                try Completion.initRuntimeError(context.vm, context.source_range, "Attempting to resume actor with error", .{}),
            );
        },
        .Dead => {
            return ExecutionResult.completion(
                try Completion.initRuntimeError(context.vm, context.source_range, "Attempting to resume dead actor", .{}),
            );
        },
    }
}

/// Return the reason this actor has yielded.
pub fn ActorYieldReason(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorYieldReason sent outside of the genesis actor", .{}),
        );
    }

    const arguments = context.getArguments("_ActorResume");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Actor);
    const actor = receiver.getActor();
    return ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(@intFromEnum(actor.yield_reason))));
}

/// Yield this actor.
pub fn ActorYield(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInRegularActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorYield sent outside of a regular actor", .{}),
        );
    }

    _ = context.actor.activation_stack.getCurrent().advanceInstruction();
    context.actor.yield_reason = .Yielded;
    context.vm.switchToActor(context.vm.genesis_actor.?);

    return ExecutionResult.actorSwitch();
}

/// Return the current actor's sender. Raise a runtime error if the actor
/// doesn't have a sender (isn't in a message).
pub fn ActorSender(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInRegularActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorSender sent outside of a regular actor", .{}),
        );
    }

    if (context.actor.message_sender == null) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorSender sent outside of a message", .{}),
        );
    }

    // FIXME: It would be nice to use a single actor proxy instead of spawning
    //        them on demand, as replying to senders is a common operation.
    var token = try context.vm.heap.getAllocation(
        ActorProxyObject.requiredSizeForAllocation(),
    );
    defer token.deinit();

    const actor_object = context.actor.message_sender.?.get();
    const actor_proxy = ActorProxyObject.create(context.vm.getMapMap(), &token, context.actor.id, actor_object);

    return ExecutionResult.completion(Completion.initNormal(actor_proxy.asValue()));
}

/// Return the managed file descriptor object that the actor is blocked on.
/// If the actor's yield reason isn't Blocked, then raise a runtime error.
pub fn ActorBlockedFD(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorBlockedFD sent outside of the genesis actor", .{}),
        );
    }

    const arguments = context.getArguments("_ActorBlockedFD");
    const actor_object = try arguments.getObject(PrimitiveContext.Receiver, .Actor);
    const actor = actor_object.getActor();

    if (actor.yield_reason != .Blocked) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorBlockedFD sent to an actor that wasn't blocked", .{}),
        );
    }

    return ExecutionResult.completion(Completion.initNormal(actor.blocked_fd.?.value));
}
