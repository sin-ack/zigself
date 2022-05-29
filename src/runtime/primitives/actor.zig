// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Heap = @import("../Heap.zig");
const Actor = @import("../Actor.zig");
const Value = @import("../value.zig").Value;
const Object = @import("../Object.zig");
const Completion = @import("../Completion.zig");
const SourceRange = @import("../SourceRange.zig");
const runtime_error = @import("../error.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ExecutionResult = @import("../interpreter.zig").ExecutionResult;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

/// Find the given selector as a method suitable for spawning an actor, and
/// refresh the pointers for the method and its receiver after ensuring that
/// there is enough space in eden to activate this method (+ any extra requested
/// by the user).
///
/// Returns an Completion if something other than a method is found (not
/// necessarily an error). Writes the method to out_method on success.
fn findMethodForActorSpawnAndReserveMemoryForActivation(
    vm: *VirtualMachine,
    source_range: SourceRange,
    receiver: *Value,
    selector: []const u8,
    out_method: **Object.Method,
    extra_memory: usize,
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
                if (!(lookup_result.isObjectReference() and lookup_result.asObject().isMethodObject())) {
                    return Completion.initNormal(lookup_result);
                }

                const lookup_result_as_method = lookup_result.asObject().asMethodObject();
                if (lookup_result_as_method.getArgumentSlotCount() != 0) {
                    return try Completion.initRuntimeError(vm, source_range, "Spawning actor with non-unary method '{s}' not permitted", .{selector});
                }

                break :method lookup_result_as_method;
            },
            .ActorMessage => {
                return try Completion.initRuntimeError(vm, source_range, "Spawning actor by sending message to actor proxy not permitted", .{});
            },
        }
    };

    const tracked_receiver = try vm.heap.track(receiver.*);
    defer tracked_receiver.untrack(vm.heap);
    const tracked_method = try vm.heap.track(method.asValue());
    defer tracked_method.untrack(vm.heap);

    try vm.heap.ensureSpaceInEden(
        Object.Activation.requiredSizeForAllocation(method.getArgumentSlotCount(), method.getAssignableSlotCount()) + extra_memory,
    );

    receiver.* = tracked_receiver.getValue();
    out_method.* = tracked_method.getValue().asObject().asMethodObject();
    return null;
}

/// Create a new actor which then becomes the genesis actor.
pub fn Genesis(context: PrimitiveContext) !ExecutionResult {
    var receiver = context.receiver.getValue();
    const message_name = context.arguments[0];

    if (context.vm.isInActorMode()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_Genesis: sent while the VM is in actor mode", .{}),
        );
    }

    // FIXME: We should perhaps allow any object to be the genesis actor
    //        context, so blocks can also work for example.
    if (!(receiver.isObjectReference() and receiver.asObject().isSlotsObject())) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected receiver of _Genesis: to be a slots object", .{}),
        );
    }

    if (!(message_name.isObjectReference() and message_name.asObject().isByteArrayObject())) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected first argument of _Genesis: to be a byte array", .{}),
        );
    }

    const selector = message_name.asObject().asByteArrayObject().getValues();
    var method: *Object.Method = undefined;
    if (try findMethodForActorSpawnAndReserveMemoryForActivation(
        context.vm,
        context.source_range,
        &receiver,
        selector,
        &method,
        Object.Actor.requiredSizeForAllocation(),
    )) |completion| {
        return ExecutionResult.completion(completion);
    }

    // NOTE: Need to advance the global actor to the next instruction to be returned to after the genesis actor exits.
    context.vm.current_actor.activation_stack.getCurrent().advanceInstruction();

    const genesis_actor = try Actor.spawn(context.vm, receiver, method, context.source_range, context.target_location);
    context.vm.setGenesisActor(genesis_actor);
    context.vm.switchToActor(genesis_actor);

    return ExecutionResult.actorSwitch();
}

/// Spawn a new actor by creating an Actor and its associated actor context
/// after sending the message given in the first argument to it. If this is sent
/// in a regular actor, then additionally create an ActorProxy object and write
/// it to the result location of the primitive.
pub fn ActorSpawn(context: PrimitiveContext) !ExecutionResult {
    var receiver = context.receiver.getValue();
    const message_name = context.arguments[0];

    if (!context.vm.isInActorMode()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorSpawn: sent while the VM is not in actor mode", .{}),
        );
    }

    const genesis_actor = context.vm.genesis_actor.?;

    // FIXME: We should perhaps allow any object to receive a message context,
    //        so blocks can also work for example.
    if (!(receiver.isObjectReference() and receiver.asObject().isSlotsObject())) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected receiver of _ActorSpawn: to be a slots object", .{}),
        );
    }

    if (!(message_name.isObjectReference() and message_name.asObject().isByteArrayObject())) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected first argument of _ActorSpawn: to be a byte array", .{}),
        );
    }

    // NOTE: Need to advance the current actor to the next instruction to be returned to after the this actor exits.
    context.vm.current_actor.activation_stack.getCurrent().advanceInstruction();

    const spawn_selector = message_name.asObject().asByteArrayObject().getValues();
    var spawn_method: *Object.Method = undefined;

    if (try findMethodForActorSpawnAndReserveMemoryForActivation(
        context.vm,
        context.source_range,
        &receiver,
        spawn_selector,
        &spawn_method,
        Object.Actor.requiredSizeForAllocation(),
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
            else => unreachable,
        }
    }

    // Create the new actor by sending the message to the receiver.
    const new_actor = try Actor.spawn(context.vm, receiver, spawn_method, context.source_range, context.target_location);
    try context.vm.registerRegularActor(new_actor);
    context.vm.switchToActor(new_actor);

    switch (try new_actor.executeUntil(context.vm, null)) {
        .ActorSwitch => {
            return ExecutionResult.completion(
                try Completion.initRuntimeError(context.vm, context.source_range, "The actor spawn activation caused an actor switch", .{}),
            );
        },
        .Completion => |*completion| {
            defer completion.deinit(context.vm);
            context.vm.switchToActor(genesis_actor);

            switch (completion.data) {
                .Normal => |value| {
                    new_actor.unwindStacks();
                    new_actor.actor_object.asObject().asActorObject().context = value;
                },
                .RuntimeError => |err| {
                    if (!context.vm.silent_errors) {
                        std.debug.print("Received error at top level while spawning actor: {s}\n", .{err.message});
                        runtime_error.printTraceFromActivationStack(new_actor.activation_stack.getStack(), err.source_range);
                    }

                    new_actor.yield_reason = .RuntimeError;
                    return ExecutionResult.completion(Completion.initNormal(new_actor.actor_object));
                },
                else => unreachable,
            }
        },
    }

    // Refresh pointers in case the actor execution caused a GC
    receiver = context.receiver.getValue();

    var entrypoint_selector = entrypoint_selector: {
        if (new_actor.entrypoint_selector) |message_value| {
            break :entrypoint_selector message_value.asObject().asByteArrayObject().getValues();
        }

        if (!context.vm.unregisterRegularActor(new_actor))
            @panic("!!! Somehow the actor unregistered itself?!");

        return ExecutionResult.completion(
            try Completion.initRuntimeError(
                context.vm,
                context.source_range,
                "The actor did not set an entrypoint selector during its spawn activation",
                .{},
            ),
        );
    };

    var entrypoint_method: *Object.Method = undefined;

    var extra_memory: usize = 0;
    if (context.actor != genesis_actor)
        extra_memory = Object.ActorProxy.requiredSizeForAllocation();

    if (try findMethodForActorSpawnAndReserveMemoryForActivation(
        context.vm,
        context.source_range,
        &receiver,
        entrypoint_selector,
        &entrypoint_method,
        extra_memory,
    )) |completion| {
        if (!context.vm.unregisterRegularActor(new_actor))
            @panic("!!! Somehow the actor unregistered itself?!");
        return ExecutionResult.completion(completion);
    }

    const actor_object = new_actor.actor_object.asObject().asActorObject();

    // FIXME: Make this nicer by providing a "actor.activateMethod or something."
    const new_activation = try new_actor.activation_stack.getNewActivationSlot(context.vm.allocator);
    try entrypoint_method.activateMethod(
        context.vm,
        actor_object.context,
        &.{},
        context.target_location,
        context.source_range,
        new_activation,
    );

    // Create a new ActorProxy object and write it to the result location of the
    // actor who spawned it, if it wasn't the genesis actor that spawned the
    // new actor.
    if (context.actor != genesis_actor) {
        const new_actor_proxy = try Object.ActorProxy.create(context.vm.heap, actor_object);
        context.actor.writeRegister(context.target_location, new_actor_proxy.asValue());
        context.actor.yield_reason = .ActorSpawned;
    }

    // Since we are switching actors (which we should in the "spawning an actor
    // in another actor" case), we cannot return the new actor object normally.
    // What we need to do instead is to write the new actor object to the target
    // location of either the _ActorResume or the _ActorSpawn: prim_send
    // instruction, which will be the instruction that's before the current one
    // (because we advance the pc in each of the aforementioned primitives).
    const genesis_current_activation = genesis_actor.activation_stack.getCurrent();
    const genesis_pc_before_last = genesis_current_activation.pc - 1;
    const genesis_activation_object = genesis_current_activation.activation_object.asObject().asActivationObject();
    const genesis_definition_block = genesis_activation_object.getBytecodeBlock();
    const genesis_inst_before_last = genesis_definition_block.getInstruction(genesis_pc_before_last);

    genesis_actor.writeRegister(genesis_inst_before_last.target, new_actor.actor_object);

    return ExecutionResult.actorSwitch();
}

/// Sets the actor's entrypoint selector, which is the message that is sent to
/// the activation once the spawn activation is complete (in order to prime it
/// for its resuming).
pub fn ActorSetEntrypoint(context: PrimitiveContext) !ExecutionResult {
    var entrypoint_selector_name = context.arguments[0];

    if (!context.vm.isInRegularActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorSetEntrypoint: sent outside of a regular actor", .{}),
        );
    }

    if (!(entrypoint_selector_name.isObjectReference() and entrypoint_selector_name.asObject().isByteArrayObject())) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected first argument of _ActorSetEntrypoint: to be a byte array", .{}),
        );
    }

    context.actor.entrypoint_selector = entrypoint_selector_name;
    return ExecutionResult.completion(Completion.initNormal(context.vm.nil()));
}

/// Resume the activation from where it last left off.
pub fn ActorResume(context: PrimitiveContext) !ExecutionResult {
    const receiver = context.receiver.getValue();

    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorResume sent outside of the genesis actor", .{}),
        );
    }

    if (!(receiver.isObjectReference() and receiver.asObject().isActorObject())) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected receiver of _ActorResume to be an actor object", .{}),
        );
    }

    // NOTE: Need to advance the current actor to the next instruction to be returned to after the this actor exits.
    context.actor.activation_stack.getCurrent().advanceInstruction();

    const actor = receiver.asObject().asActorObject().getActor();
    std.debug.assert(context.vm.regularActorIsRegistered(actor));

    // Preemptively write a nil to the _ActorResume location so we don't hold
    // onto a temporary accidentally.
    context.actor.writeRegister(context.target_location, context.vm.nil());

    switch (actor.yield_reason) {
        .None, .Yielded, .ActorSpawned => {
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
        .Blocked => @panic("TODO handle YieldReason.Blocked"),
    }
}

/// Return the reason this actor has yielded.
pub fn ActorYieldReason(context: PrimitiveContext) !ExecutionResult {
    const receiver = context.receiver.getValue();

    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorYieldReason sent outside of the genesis actor", .{}),
        );
    }

    if (!(receiver.isObjectReference() and receiver.asObject().isActorObject())) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "Expected receiver of _ActorYieldReason to be an actor object", .{}),
        );
    }

    const actor = receiver.asObject().asActorObject().getActor();
    return ExecutionResult.completion(Completion.initNormal(Value.fromUnsignedInteger(@enumToInt(actor.yield_reason))));
}

/// Yield this actor.
pub fn ActorYield(context: PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInRegularActor()) {
        return ExecutionResult.completion(
            try Completion.initRuntimeError(context.vm, context.source_range, "_ActorYield sent outside of a regular actor", .{}),
        );
    }

    context.actor.activation_stack.getCurrent().advanceInstruction();
    context.actor.yield_reason = .Yielded;
    context.vm.switchToActor(context.vm.genesis_actor.?);

    return ExecutionResult.actorSwitch();
}

/// Return the current actor's sender. Raise a runtime error if the actor
/// doesn't have a sender (isn't in a message).
pub fn ActorSender(context: PrimitiveContext) !ExecutionResult {
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
    try context.vm.heap.ensureSpaceInEden(
        Object.ActorProxy.requiredSizeForAllocation(),
    );

    const actor_object = context.actor.message_sender.?.asObject().asActorObject();
    const actor_proxy = try Object.ActorProxy.create(context.vm.heap, actor_object);

    return ExecutionResult.completion(Completion.initNormal(actor_proxy.asValue()));
}
