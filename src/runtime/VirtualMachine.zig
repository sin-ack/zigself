// Copyright (c) 2022-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Actor = @import("./Actor.zig");
const Value = @import("./value.zig").Value;
const Script = @import("../language/Script.zig");
const AstGen = @import("./bytecode/AstGen.zig");
const context = @import("context.zig");
const CodeGen = @import("./bytecode/CodeGen.zig");
const SlotsMap = slots_object.SlotsMap;
const ByteArray = @import("./ByteArray.zig");
const heap_import = @import("./Heap.zig");
const ActorObject = @import("objects/actor.zig").Actor;
const SlotsObject = slots_object.Slots;
const stack_trace = @import("./stack_trace.zig");
const slots_object = @import("objects/slots.zig");
const AllocationToken = heap_import.AllocationToken;
const RegisterLocation = @import("./bytecode.zig").RegisterLocation;

/// The allocator object that will be used throughout the virtual machine's
/// lifetime.
allocator: Allocator,
/// The object heap.
heap: Heap,
/// A mapping from argument counts to the related block message names.
/// Since block names will not be unique, this mapping allows us to store
/// a single instance of each message name for the respective block arities.
block_message_names: std.AutoArrayHashMapUnmanaged(u8, ByteArray),

// --- References to global objects ---

/// The root of the current Self world.
lobby_object: Heap.Handle = undefined,

/// The global nil object used to represent the default assignable slot value.
/// Can also be used in place of "nothing".
global_nil: Heap.Handle = undefined,
/// The global truth value.
global_true: Heap.Handle = undefined,
/// The global falsity value.
global_false: Heap.Handle = undefined,

// --- Primitive object traits ---

actor_traits: Heap.Handle = undefined,
array_traits: Heap.Handle = undefined,
block_traits: Heap.Handle = undefined,
float_traits: Heap.Handle = undefined,
string_traits: Heap.Handle = undefined,
integer_traits: Heap.Handle = undefined,

// --- Settings ---

/// Whether the interpreter should be silent when an error happens.
silent_errors: bool = false,

// --- Actors ---

/// The "global actor", which is the actor that is spawned when the VM is started.
// NOTE: Initialization deferred until the VM object is complete
global_actor: *Actor = undefined,
/// The "genesis actor" which is the actor that acts as the
/// coordinator/scheduler for other actors once the _Genesis: message is sent
/// until actor mode is exited.
genesis_actor: ?*Actor = null,
/// All the regular actors that currently exist. A regular actor is an actor
/// that is spawned by either another regular actor or the genesis actor.
///
/// These actors are owned by the Actor object, and are weakly referenced here.
regular_actors: RegularActorSet = .{},

// --- Current execution state ---

/// The actor that is currently executing.
// NOTE: Initialization deferred until the VM object is complete
current_actor: *Actor = undefined,

pub const Heap = heap_import.Heap(VirtualMachine);
const VirtualMachine = @This();
const RegularActorSet = std.AutoArrayHashMapUnmanaged(*Actor, void);

/// Creates the virtual machine, including the heap and the global objects.
pub fn create(allocator: Allocator) !*VirtualMachine {
    var self = try allocator.create(VirtualMachine);
    errdefer allocator.destroy(self);

    self.* = .{
        .allocator = allocator,
        .heap = try .init(allocator, self),
        .block_message_names = .{},
    };
    errdefer self.heap.deinit();

    var token = try self.heap.allocate(
        // Global objects
        SlotsMap.requiredSizeForAllocation(0) +
            (10 * SlotsObject.requiredSizeForAllocation(0)) +
            // Global actor
            ActorObject.requiredSizeForAllocation(),
    );
    defer token.deinit();

    const empty_map = SlotsMap.create(&token, 0);

    self.lobby_object = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);

    self.global_nil = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);
    self.global_true = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);
    self.global_false = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);

    self.actor_traits = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);
    self.array_traits = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);
    self.block_traits = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);
    self.float_traits = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);
    self.string_traits = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);
    self.integer_traits = try makeEmptyGloballyReachableObject(&self.heap, &token, empty_map);

    // NOTE: The actor object of the global actor should never be reachable in the first place,
    //       so the global reachability bit does not matter here.
    self.global_actor = try Actor.create(self, &token, self.lobby_object.get());
    self.current_actor = self.global_actor;

    return self;
}

fn makeEmptyGloballyReachableObject(heap: *Heap, token: *AllocationToken, map: SlotsMap.Ptr) !Heap.Handle {
    // NOTE: These objects will always belong to the global actor, so we hardcode the actor ID 0 to them.
    //       Otherwise we would hit a chicken-and-egg situation where the global actor needs the lobby
    //       and the lobby needs the global actor.
    const slots = SlotsObject.create(token, .Global, map, &.{});
    slots.object.getMetadata().reachability = .Global;
    return heap.track(slots.asValue());
}

pub fn destroy(self: *VirtualMachine) void {
    // NOTE: All actors are finalized by the actor object that they're owned
    //       by when the heap is deallocated.
    self.regular_actors.deinit(self.allocator);

    self.lobby_object.deinit(&self.heap);
    self.global_nil.deinit(&self.heap);
    self.global_true.deinit(&self.heap);
    self.global_false.deinit(&self.heap);
    self.actor_traits.deinit(&self.heap);
    self.array_traits.deinit(&self.heap);
    self.block_traits.deinit(&self.heap);
    self.float_traits.deinit(&self.heap);
    self.string_traits.deinit(&self.heap);
    self.integer_traits.deinit(&self.heap);

    {
        var it = self.block_message_names.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
    }
    self.block_message_names.deinit(self.allocator);

    self.heap.deinit();
    self.allocator.destroy(self);
}

pub fn getTrue(self: VirtualMachine) Value {
    return self.global_true.get();
}

pub fn getFalse(self: VirtualMachine) Value {
    return self.global_false.get();
}

pub fn nil(self: VirtualMachine) Value {
    return self.global_nil.get();
}

pub fn lobby(self: VirtualMachine) Value {
    return self.lobby_object.get();
}

pub fn pushContext(self: *VirtualMachine) void {
    context.pushVM(self);
}

pub fn popContext(self: *VirtualMachine) void {
    const popped_vm = context.popVM();
    std.debug.assert(popped_vm == self);
}

// TODO: This is just string interning with more steps. Create a generic string
//       interning facility and cache block message names there.
const BlockMessageNameContext = struct {
    exists: bool,
    value_ptr: *ByteArray,
    argument_count: u8,

    fn writeBlockMessageName(name: []u8, argument_count: u8) void {
        std.debug.assert(name.len == blockMessageNameLength(argument_count));
        @memcpy(name[0..5], "value");

        if (argument_count > 0) {
            name[5] = ':';

            var remaining_buffer = name[6..];
            while (remaining_buffer.len > 0) {
                @memcpy(remaining_buffer[0..5], "With:");
                remaining_buffer = remaining_buffer[5..];
            }
        }
    }

    fn blockMessageNameLength(argument_count: u8) usize {
        var needed_space: usize = 5; // value
        if (argument_count > 0) {
            needed_space += 1; // :
            needed_space += 5 * (argument_count - 1); // Any other With:s needed
        }

        return needed_space;
    }

    pub fn get(self: @This(), allocator: Allocator) !ByteArray {
        if (self.exists) return self.value_ptr.*;

        const byte_array = try ByteArray.createUninitialized(allocator, blockMessageNameLength(self.argument_count));
        writeBlockMessageName(byte_array.getValues(), self.argument_count);

        self.value_ptr.* = byte_array;
        return self.value_ptr.*;
    }
};

/// Return a block message name with the given argument count, creating it
/// if it does not exist.
///
/// A block message name looks like: `value:With:With:With:...`.
pub fn getOrCreateBlockMessageName(self: *VirtualMachine, argument_count: u8) !BlockMessageNameContext {
    const result = try self.block_message_names.getOrPut(self.allocator, argument_count);
    return BlockMessageNameContext{ .exists = result.found_existing, .value_ptr = result.value_ptr, .argument_count = argument_count };
}

pub fn executeEntrypointScript(self: *VirtualMachine, script: Script.Ref) !?Value {
    self.pushContext();
    defer self.popContext();

    var entrypoint_ast_executable = try AstGen.generateExecutableFromScript(self.allocator, script);
    defer entrypoint_ast_executable.unref();

    var entrypoint_executable = try CodeGen.lowerExecutable(self.allocator, entrypoint_ast_executable.value);
    defer entrypoint_executable.unref();

    // XXX: Temporary hack to make the current actor available from context.
    self.current_actor.pushContext();
    try self.current_actor.activation_stack.pushEntrypointActivation(entrypoint_executable);
    self.current_actor.popContext();

    while (true) {
        // The location to which the actor's parent actor should have the result
        // written.
        const current_actor_target_location = self.current_actor.activation_stack.getStack()[0].target_location;

        var actor_result = try self.current_actor.execute();
        switch (actor_result) {
            .Switched => continue,
            .Finished => |value| {
                self.current_actor.unwindStacks();

                if (self.current_actor == self.global_actor) {
                    return value;
                }

                if (self.genesis_actor) |genesis_actor| {
                    if (self.current_actor == genesis_actor) {
                        self.global_actor.writeRegister(current_actor_target_location, value);
                        self.genesis_actor = null;
                        self.switchToActor(self.global_actor);
                    } else {
                        self.current_actor.yield_reason = .Dead;
                        if (!self.unregisterRegularActor(self.current_actor))
                            std.debug.panic("!!! Actor {*} was not registered as a regular actor while dying!", .{self.current_actor});
                        // FIXME: Write something meaningful to the return
                        //        location of _ActorResume for the genesis
                        //        actor.
                        self.switchToActor(genesis_actor);
                    }
                }
            },
            .RuntimeError => |*err| {
                defer err.deinit(self.allocator);

                if (self.isInRegularActor()) {
                    if (!self.silent_errors) {
                        std.debug.print("Actor received error at top level: {s}\n", .{err.getMessage()});
                        stack_trace.printTraceFromActivationStack(self.current_actor.activation_stack.getStack(), err.source_range);
                    }

                    const actor = self.current_actor;
                    // FIXME: In the future we will want to hang onto the
                    //        regular actor in order to obtain debug information
                    //        from it.
                    if (!self.unregisterRegularActor(actor))
                        std.debug.print("!!! Actor {*} was not registered as a regular actor while receiving a runtime error!", .{actor});

                    actor.yield_reason = .RuntimeError;
                    // FIXME: Write something meaningful to the return location
                    //        of _ActorResume for the genesis actor.
                    self.switchToActor(self.genesis_actor.?);
                    continue;
                }

                if (!self.silent_errors) {
                    std.debug.print("Received error at top level: {s}\n", .{err.getMessage()});
                    stack_trace.printTraceFromActivationStack(self.current_actor.activation_stack.getStack(), err.source_range);
                }

                self.switchToActor(self.global_actor);
                // Clean up the genesis actor in case more code is run in the VM
                // later on.
                self.genesis_actor = null;

                self.current_actor.unwindStacks();
                return null;
            },
        }
    }
}

pub fn readRegister(self: VirtualMachine, location: RegisterLocation) Value {
    return switch (location) {
        .zero => self.nil(),
        else => self.current_actor.readRegister(location),
    };
}

pub fn writeRegister(self: *VirtualMachine, location: RegisterLocation, value: Value) void {
    self.current_actor.writeRegister(location, value);
}

pub fn takeStackSnapshot(self: *VirtualMachine) Actor.StackSnapshot {
    return self.current_actor.takeStackSnapshot();
}

pub fn restoreStackSnapshot(self: *VirtualMachine, snapshot: Actor.StackSnapshot) void {
    self.current_actor.restoreStackSnapshot(snapshot);
}

pub fn visitEdges(
    self: *VirtualMachine,
    // TODO: Write interfaces proposal for Zig
    visitor: anytype,
) !void {
    // NOTE: The global objects are all handles so no need to visit them.

    try self.global_actor.visitEdges(visitor);
    if (self.genesis_actor) |actor| {
        try actor.visitEdges(visitor);
    }

    for (self.regular_actors.keys()) |actor| {
        try actor.visitEdges(visitor);
    }
}

pub fn isInActorMode(self: *VirtualMachine) bool {
    return self.genesis_actor != null;
}

pub fn isInRegularActor(self: *VirtualMachine) bool {
    return self.current_actor != self.global_actor and self.current_actor != self.genesis_actor;
}

pub fn isInGenesisActor(self: *VirtualMachine) bool {
    if (self.genesis_actor) |genesis_actor| {
        return self.current_actor == genesis_actor;
    }

    return false;
}

pub fn setGenesisActor(self: *VirtualMachine, actor: *Actor) void {
    std.debug.assert(!self.isInActorMode());
    self.genesis_actor = actor;
}

pub fn switchToActor(self: *VirtualMachine, actor: *Actor) void {
    self.current_actor = actor;
    actor.yield_reason = .None;
}

pub fn registerRegularActor(self: *VirtualMachine, actor: *Actor) !void {
    const gop = try self.regular_actors.getOrPut(self.allocator, actor);
    std.debug.assert(!gop.found_existing);
}

pub fn unregisterRegularActor(self: *VirtualMachine, actor: *Actor) bool {
    return self.regular_actors.swapRemove(actor);
}

pub fn regularActorIsRegistered(self: VirtualMachine, actor: *Actor) bool {
    return self.regular_actors.contains(actor);
}
