// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const tracy = @import("tracy");

const Actor = @import("../Actor.zig");
const Value = @import("../value.zig").Value;
const bless = @import("../object_bless.zig");
const Selector = @import("../Selector.zig");
const ActorObject = @import("../objects/actor.zig").Actor;
const SourceRange = @import("../SourceRange.zig");
const stack_trace = @import("../stack_trace.zig");
const MethodObject = @import("../objects/method.zig").Method;
const RuntimeError = @import("../RuntimeError.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ExecutionResult = @import("../execution_result.zig").ExecutionResult;
const ActorProxyObject = @import("../objects/actor_proxy.zig").ActorProxy;
const PrimitiveContext = @import("../primitives.zig").PrimitiveContext;

const FindActorMethodResult = union(enum) {
    /// The actor method was found.
    Method: MethodObject.Ptr,
    /// Another kind of value has resolved from the lookup.
    Value: Value,
    /// An error has occurred.
    RuntimeError: RuntimeError,
};

/// Find the given selector as a method suitable for spawning an actor.
///
/// Returns an Completion if something other than a method is found (not
/// necessarily an error). Writes the method to out_method on success.
fn findActorMethod(
    source_range: SourceRange,
    receiver: Value,
    selector: Selector,
) !FindActorMethodResult {
    return lookup: switch (receiver.lookup(selector)) {
        .Nothing => FindActorMethodResult{ .RuntimeError = try RuntimeError.initFormatted(
            source_range,
            "Unknown selector '{f}'",
            .{selector},
        ) },
        .ActorMessage => FindActorMethodResult{ .RuntimeError = RuntimeError.initLiteral(
            source_range,
            "Spawning actor by sending message to actor proxy not permitted",
        ) },
        .FoundUncacheable => |value_slot| switch (value_slot) {
            .Constant => |target| blk: {
                if (target.asObject()) |target_object| {
                    if (target_object.asType(.Method)) |target_as_method| {
                        if (target_as_method.getArgumentSlotCount() != 0) {
                            break :blk FindActorMethodResult{ .RuntimeError = try RuntimeError.initFormatted(
                                source_range,
                                "Spawning actor with non-unary method '{f}' not permitted",
                                .{selector},
                            ) };
                        }

                        break :blk FindActorMethodResult{ .Method = target_as_method };
                    }
                }

                break :blk FindActorMethodResult{ .Value = target };
            },
            .Assignable => FindActorMethodResult{ .RuntimeError = try RuntimeError.initFormatted(
                source_range,
                "Spawning actor with non-unary method '{f}' not permitted",
                .{selector},
            ) },
        },
        .Found => |lookup_target| {
            const value_slot = lookup_target.object.getValueSlot(lookup_target.value_slot_index);
            continue :lookup .{ .FoundUncacheable = value_slot };
        },
    };
}

/// Create a new actor which then becomes the genesis actor.
pub fn Genesis(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (context.vm.isInActorMode()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_Genesis: sent while the VM is in actor mode",
        ));
    }

    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(&context.vm.heap);
    defer handles.deinit(&context.vm.heap);

    const arguments = context.getArguments("_Genesis:");
    var receiver = context.receiver;
    handles.trackValue(&receiver);

    const selector_name = (try arguments.getObject(0, .ByteArray)).getValues();
    const selector = Selector.fromName(selector_name);

    // FIXME: We should perhaps allow any object to be the genesis actor
    //        context, so blocks can also work for example.
    if (!(receiver.type == .Object and receiver.unsafeAsObject().getMetadata().type == .Slots)) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "Expected receiver of _Genesis: to be a slots object",
        ));
    }

    var method: MethodObject.Ptr = undefined;
    switch (try findActorMethod(context.source_range, receiver, selector)) {
        .Method => |m| method = m,
        .Value => |value| return ExecutionResult.resolve(value),
        .RuntimeError => |err| return ExecutionResult.runtimeError(err),
    }
    handles.trackObject(&method);

    var token = try context.vm.heap.allocate(
        method.requiredSizeForActivation() +
            ActorObject.requiredSizeForAllocation(),
    );
    defer token.deinit();

    // NOTE: Need to advance the global actor to the next instruction to be returned to after the genesis actor exits.
    _ = context.actor.activation_stack.getCurrent().advanceInstruction();

    // NOTE: The receiver here is passed as a dummy in order to get the new actor ID.
    const genesis_actor = try Actor.create(context.vm, &token, receiver);
    const blessed_receiver = try bless.bless(genesis_actor.id, receiver);

    genesis_actor.actor_object.get().context = blessed_receiver;
    try genesis_actor.activateMethod(&token, method, context.target_location, context.source_range);
    context.vm.setGenesisActor(genesis_actor);
    context.vm.switchToActor(genesis_actor);

    return ExecutionResult.yield();
}

/// Spawn a new actor by creating an Actor and its associated actor context
/// after sending the message given in the first argument to it. If this is sent
/// in a regular actor, then additionally create an ActorProxy object and write
/// it to the result location of the primitive.
pub fn ActorSpawn(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (!context.vm.isInActorMode()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorSpawn: sent while the VM is not in actor mode",
        ));
    }

    var handles: VirtualMachine.Heap.Handles = undefined;
    handles.init(&context.vm.heap);
    defer handles.deinit(&context.vm.heap);

    const arguments = context.getArguments("_ActorSpawn:");
    var receiver = context.receiver;
    handles.trackValue(&receiver);

    const spawn_selector_name = (try arguments.getObject(0, .ByteArray)).getValues();
    const spawn_selector = Selector.fromName(spawn_selector_name);

    const genesis_actor = context.vm.genesis_actor.?;

    // FIXME: We should perhaps allow any object to receive a message context,
    //        so blocks can also work for example.
    if (!(receiver.type == .Object and receiver.unsafeAsObject().getMetadata().type == .Slots)) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "Expected receiver of _ActorSpawn: to be a slots object",
        ));
    }

    // NOTE: Need to advance the current actor to the next instruction to be returned to after the this actor exits.
    _ = context.actor.activation_stack.getCurrent().advanceInstruction();

    var spawn_method: MethodObject.Ptr = undefined;
    switch (try findActorMethod(context.source_range, receiver, spawn_selector)) {
        .Method => |m| spawn_method = m,
        .Value => {
            return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                context.source_range,
                "The actor spawn message pointed to a non-method slot. This is not allowed as the spawned actor does not own that value.",
            ));
        },
        .RuntimeError => |err| return ExecutionResult.runtimeError(err),
    }
    handles.trackObject(&spawn_method);

    var token = try context.vm.heap.allocate(spawn_method.requiredSizeForActivation());
    defer token.deinit();

    const stack_snapshot = context.vm.takeStackSnapshot();
    var before_spawn_method_activation = context.actor.activation_stack.getCurrent();
    const before_spawn_method_activation_ref = before_spawn_method_activation.takeRef(context.actor.activation_stack);

    // Activate the method in the actor which requested the spawn. This is required because
    // any new actor we create does not have any memory of its own yet, so we need to create a
    // new context for it before we can do anything.
    try context.actor.activateMethodWithContext(&token, receiver, spawn_method, context.target_location, context.source_range);

    var actor_result = try context.actor.executeUntil(before_spawn_method_activation_ref);
    // This object is owned by the actor that requested the spawn.
    var new_actor_context = switch (actor_result) {
        .Switched => {
            return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                context.source_range,
                "The actor spawn activation caused an actor switch",
            ));
        },
        .Finished => |value| value,
        .RuntimeError => |*err| {
            defer err.deinit(context.vm.allocator);

            context.vm.switchToActor(genesis_actor);

            // Refresh activation pointer
            before_spawn_method_activation = before_spawn_method_activation_ref.get(context.actor.activation_stack).?;

            if (!context.vm.silent_errors) {
                std.debug.print("Received error at top level while spawning actor: {s}\n", .{err.getMessage()});
                stack_trace.printTraceFromActivationStackUntil(context.actor.activation_stack.getStack(), err.source_range, before_spawn_method_activation);
            }

            context.actor.activation_stack.restoreTo(before_spawn_method_activation);
            context.vm.restoreStackSnapshot(stack_snapshot);

            context.actor.yield_reason = .RuntimeError;
            // FIXME: What is a sensible return value here?
            return ExecutionResult.resolve(context.vm.global_nil);
        },
    };
    handles.trackValue(&new_actor_context);

    const entrypoint_selector_name = entrypoint_selector_name: {
        if (context.actor.entrypoint_selector) |message_value| {
            break :entrypoint_selector_name message_value.get().getValues();
        }

        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "The actor did not set an entrypoint selector during its spawn activation",
        ));
    };
    const entrypoint_selector = Selector.fromName(entrypoint_selector_name);

    var entrypoint_method: MethodObject.Ptr = undefined;
    switch (try findActorMethod(context.source_range, receiver, entrypoint_selector)) {
        .Method => |m| entrypoint_method = m,
        .Value => |value| return ExecutionResult.resolve(value),
        .RuntimeError => |err| return ExecutionResult.runtimeError(err),
    }
    handles.trackObject(&entrypoint_method);

    context.actor.entrypoint_selector = null;

    token.deinit();
    token = token: {
        var required_memory = ActorObject.requiredSizeForAllocation();
        if (context.actor != genesis_actor)
            required_memory += ActorProxyObject.requiredSizeForAllocation();
        break :token try context.vm.heap.allocate(required_memory);
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
        const new_actor_proxy = ActorProxyObject.create(&token, context.actor.id, new_actor.actor_object.get());
        context.actor.writeRegister(context.target_location, new_actor_proxy.asValue());
        context.actor.yield_reason = .ActorSpawned;
        context.vm.switchToActor(genesis_actor);
    }

    // Bless the new actor context
    const blessed_new_actor_context = try bless.bless(new_actor.id, new_actor_context);
    new_actor.actor_object.get().context = blessed_new_actor_context;

    token.deinit();
    token = try context.vm.heap.allocate(entrypoint_method.requiredSizeForActivation());
    try new_actor.activateMethod(&token, entrypoint_method, context.target_location, context.source_range);

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

    return ExecutionResult.yield();
}

/// Sets the actor's entrypoint selector, which is the message that is sent to
/// the activation once the spawn activation is complete (in order to prime it
/// for its resuming).
pub fn ActorSetEntrypoint(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (!context.vm.isInActorMode()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorSetEntrypoint: sent outside of actor mode",
        ));
    }

    const arguments = context.getArguments("_ActorSetEntrypoint:");
    const entrypoint_selector_name = try arguments.getObject(0, .ByteArray);

    context.actor.entrypoint_selector = .{ .value = entrypoint_selector_name.asValue() };
    return ExecutionResult.resolve(context.vm.global_nil);
}

/// Resume the activation from where it last left off.
pub fn ActorResume(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorResume sent outside of the genesis actor",
        ));
    }

    const arguments = context.getArguments("_ActorResume");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Actor);

    const actor = receiver.getActor();
    std.debug.assert(context.vm.regularActorIsRegistered(actor));

    // NOTE: Need to advance the current actor to the next instruction to be returned to after the this actor exits.
    _ = context.actor.activation_stack.getCurrent().advanceInstruction();

    // Preemptively write a nil to the _ActorResume location so we don't hold
    // onto a temporary accidentally.
    context.actor.writeRegister(context.target_location, context.vm.global_nil);

    switch (actor.yield_reason) {
        .None, .Yielded, .Blocked, .ActorSpawned => {
            context.vm.switchToActor(actor);
            return ExecutionResult.yield();
        },
        .RuntimeError => {
            return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                context.source_range,
                "Attempting to resume actor with error",
            ));
        },
        .Dead => {
            return ExecutionResult.runtimeError(RuntimeError.initLiteral(
                context.source_range,
                "Attempting to raise the dead from the grave",
            ));
        },
    }
}

/// Return the reason this actor has yielded.
pub fn ActorYieldReason(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorYieldReason sent outside of the genesis actor",
        ));
    }

    const arguments = context.getArguments("_ActorResume");
    const receiver = try arguments.getObject(PrimitiveContext.Receiver, .Actor);
    const actor = receiver.getActor();
    return ExecutionResult.resolve(Value.fromUnsignedInteger(@intFromEnum(actor.yield_reason)));
}

/// Yield this actor.
pub fn ActorYield(context: *PrimitiveContext) !ExecutionResult {
    if (!context.vm.isInRegularActor()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorYield sent outside of a regular actor",
        ));
    }

    _ = context.actor.activation_stack.getCurrent().advanceInstruction();
    context.actor.yield_reason = .Yielded;
    context.vm.switchToActor(context.vm.genesis_actor.?);

    return ExecutionResult.yield();
}

/// Return the current actor's sender. Raise a runtime error if the actor
/// doesn't have a sender (isn't in a message).
pub fn ActorSender(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (!context.vm.isInRegularActor()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorSender sent outside of a regular actor",
        ));
    }

    if (context.actor.message_sender == null) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorSender sent outside of a message",
        ));
    }

    // FIXME: It would be nice to use a single actor proxy instead of spawning
    //        them on demand, as replying to senders is a common operation.
    var token = try context.vm.heap.allocate(
        ActorProxyObject.requiredSizeForAllocation(),
    );
    defer token.deinit();

    const actor_object = context.actor.message_sender.?.get();
    const actor_proxy = ActorProxyObject.create(&token, context.actor.id, actor_object);

    return ExecutionResult.resolve(actor_proxy.asValue());
}

/// Return the managed file descriptor object that the actor is blocked on.
/// If the actor's yield reason isn't Blocked, then raise a runtime error.
pub fn ActorBlockedFD(context: *PrimitiveContext) !ExecutionResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (!context.vm.isInGenesisActor()) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorBlockedFD sent outside of the genesis actor",
        ));
    }

    const arguments = context.getArguments("_ActorBlockedFD");
    const actor_object = try arguments.getObject(PrimitiveContext.Receiver, .Actor);
    const actor = actor_object.getActor();

    if (actor.yield_reason != .Blocked) {
        return ExecutionResult.runtimeError(RuntimeError.initLiteral(
            context.source_range,
            "_ActorBlockedFD sent to an actor that wasn't blocked",
        ));
    }

    return ExecutionResult.resolve(actor.blocked_fd.?.value);
}
