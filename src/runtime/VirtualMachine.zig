// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("objects/map.zig").Map;
const Heap = @import("./Heap.zig");
const Slot = @import("./slot.zig").Slot;
const Actor = @import("./Actor.zig");
const Value = @import("./value.zig").Value;
const Range = @import("../language/Range.zig");
const Script = @import("../language/script.zig");
const AstGen = @import("./bytecode/AstGen.zig");
const CodeGen = @import("./bytecode/CodeGen.zig");
const SlotsMap = slots_object.SlotsMap;
const ByteArray = @import("./ByteArray.zig");
const ActorObject = @import("objects/actor.zig").Actor;
const SlotsObject = slots_object.Slots;
const slots_object = @import("objects/slots.zig");
const stack_trace = @import("./stack_trace.zig");
const RegisterLocation = @import("./bytecode.zig").RegisterLocation;

/// The allocator object that will be used throughout the virtual machine's
/// lifetime.
allocator: Allocator,
/// The object heap.
heap: *Heap,
/// A mapping from argument counts to the related block message names.
/// Since block names will not be unique, this mapping allows us to store
/// a single instance of each message name for the respective block arities.
block_message_names: std.AutoArrayHashMapUnmanaged(u8, Heap.Tracked),

// --- References to global objects ---

/// The map-map is what all maps point to as their map. It is a map that
/// points to itself, making it the "prototype" of maps and aiding GC by
/// handling the "map pointer" case for maps. Some objects also use it in place
/// of their maps where their map would otherwise contain no useful information.
map_map: Heap.Tracked = undefined,

/// The root of the current Self world.
lobby_object: Heap.Tracked = undefined,

/// The global nil object used to represent the default assignable slot value.
/// Can also be used in place of "nothing".
global_nil: Heap.Tracked = undefined,
/// The global truth value.
global_true: Heap.Tracked = undefined,
/// The global falsity value.
global_false: Heap.Tracked = undefined,

// --- Primitive object traits ---

actor_traits: Heap.Tracked = undefined,
array_traits: Heap.Tracked = undefined,
block_traits: Heap.Tracked = undefined,
float_traits: Heap.Tracked = undefined,
string_traits: Heap.Tracked = undefined,
integer_traits: Heap.Tracked = undefined,

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

const Self = @This();
const RegularActorSet = std.AutoArrayHashMapUnmanaged(*Actor, void);

/// Creates the virtual machine, including the heap and the global objects.
pub fn create(allocator: Allocator) !*Self {
    var self = try allocator.create(Self);
    errdefer allocator.destroy(self);

    var heap = try Heap.create(allocator, self);
    errdefer heap.destroy();

    self.* = .{
        .allocator = allocator,
        .heap = heap,
        .block_message_names = .{},
    };

    var token = try heap.getAllocation(
        // Map map
        Map.requiredSizeForAllocatingMapMap() +
            // Global objects
            SlotsMap.requiredSizeForAllocation(0) +
            (10 * SlotsObject.requiredSizeForAllocation(0)) +
            // Global actor
            ActorObject.requiredSizeForAllocation(),
    );
    defer token.deinit();

    // Before creating any objects, we first need to create the map-map.
    try self.createMapMap(&token);

    const empty_map = SlotsMap.create(self.getMapMap(), &token, 0);
    empty_map.map.object.object_information.reachability = .Global;

    self.lobby_object = try makeEmptyGloballyReachableObject(&token, empty_map);

    self.global_nil = try makeEmptyGloballyReachableObject(&token, empty_map);
    self.global_true = try makeEmptyGloballyReachableObject(&token, empty_map);
    self.global_false = try makeEmptyGloballyReachableObject(&token, empty_map);

    self.actor_traits = try makeEmptyGloballyReachableObject(&token, empty_map);
    self.array_traits = try makeEmptyGloballyReachableObject(&token, empty_map);
    self.block_traits = try makeEmptyGloballyReachableObject(&token, empty_map);
    self.float_traits = try makeEmptyGloballyReachableObject(&token, empty_map);
    self.string_traits = try makeEmptyGloballyReachableObject(&token, empty_map);
    self.integer_traits = try makeEmptyGloballyReachableObject(&token, empty_map);

    // NOTE: The actor object of the global actor should never be reachable in the first place,
    //       so the global reachability bit does not matter here.
    self.global_actor = try Actor.create(self, &token, self.lobby_object.getValue());
    self.current_actor = self.global_actor;

    return self;
}

fn createMapMap(self: *Self, token: *Heap.AllocationToken) !void {
    self.map_map = try token.heap.track(Map.createMapMap(token));
}

pub fn getMapMap(self: Self) Map.Ptr {
    return self.map_map.getValue().asObject().mustBeType(.Map);
}

fn makeEmptyGloballyReachableObject(token: *Heap.AllocationToken, map: SlotsMap.Ptr) !Heap.Tracked {
    // NOTE: These objects will always belong to the global actor, so we hardcode the actor ID 0 to them.
    //       Otherwise we would hit a chicken-and-egg situation where the global actor needs the lobby
    //       and the lobby needs the global actor.
    const GlobalActorID = 0;

    const slots = SlotsObject.create(token, GlobalActorID, map, &.{});
    slots.object.object_information.reachability = .Global;
    return try token.heap.track(slots.asValue());
}

pub fn destroy(self: *Self) void {
    // NOTE: All actors are finalized by the actor object that they're owned
    //       by when the heap is deallocated.
    self.regular_actors.deinit(self.allocator);

    self.lobby_object.untrack(self.heap);
    self.global_nil.untrack(self.heap);
    self.global_true.untrack(self.heap);
    self.global_false.untrack(self.heap);
    self.actor_traits.untrack(self.heap);
    self.array_traits.untrack(self.heap);
    self.block_traits.untrack(self.heap);
    self.float_traits.untrack(self.heap);
    self.string_traits.untrack(self.heap);
    self.integer_traits.untrack(self.heap);

    {
        var it = self.block_message_names.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.untrack(self.heap);
        }
    }
    self.block_message_names.deinit(self.allocator);

    self.heap.destroy();
    self.allocator.destroy(self);
}

pub fn getTrue(self: Self) Value {
    return self.global_true.getValue();
}

pub fn getFalse(self: Self) Value {
    return self.global_false.getValue();
}

pub fn nil(self: Self) Value {
    return self.global_nil.getValue();
}

pub fn lobby(self: Self) Value {
    return self.lobby_object.getValue();
}

const BlockMessageNameContext = struct {
    exists: bool,
    value_ptr: *Heap.Tracked,
    argument_count: u8,

    pub fn requiredSize(self: @This()) usize {
        return ByteArray.requiredSizeForAllocation(blockMessageNameLength(self.argument_count));
    }

    pub fn get(self: @This(), token: *Heap.AllocationToken) !Value {
        if (self.exists) return self.value_ptr.getValue();

        const byte_array = ByteArray.createUninitialized(token, blockMessageNameLength(self.argument_count));
        writeBlockMessageName(byte_array.getValues(), self.argument_count);

        const tracked_value = try token.heap.track(byte_array.asValue());
        self.value_ptr.* = tracked_value;
        return tracked_value.getValue();
    }
};

/// Return a block message name with the given argument count, creating it
/// if it does not exist.
///
/// A block message name looks like: `value:With:With:With:...`.
pub fn getOrCreateBlockMessageName(self: *Self, argument_count: u8) !BlockMessageNameContext {
    const result = try self.block_message_names.getOrPut(self.allocator, argument_count);
    return BlockMessageNameContext{ .exists = result.found_existing, .value_ptr = result.value_ptr, .argument_count = argument_count };
}

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

pub fn executeEntrypointScript(self: *Self, script: Script.Ref) !?Value {
    var entrypoint_ast_executable = try AstGen.generateExecutableFromScript(self.allocator, script);
    defer entrypoint_ast_executable.unref();

    var entrypoint_executable = try CodeGen.lowerExecutable(self.allocator, entrypoint_ast_executable.value);
    defer entrypoint_executable.unref();

    try self.current_actor.activation_stack.pushEntrypointActivation(self, entrypoint_executable);

    while (true) {
        // The location to which the actor's parent actor should have the result
        // written.
        const current_actor_target_location = self.current_actor.activation_stack.getStack()[0].target_location;

        var actor_result = try self.current_actor.execute(self);
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

pub fn readRegister(self: Self, location: RegisterLocation) Value {
    return switch (location) {
        .zero => self.nil(),
        else => self.current_actor.readRegister(location),
    };
}

pub fn writeRegister(self: *Self, location: RegisterLocation, value: Value) void {
    self.current_actor.writeRegister(location, value);
}

pub fn takeStackSnapshot(self: *Self) Actor.StackSnapshot {
    return self.current_actor.takeStackSnapshot();
}

pub fn restoreStackSnapshot(self: *Self, snapshot: Actor.StackSnapshot) void {
    self.current_actor.restoreStackSnapshot(snapshot);
}

pub fn visitValues(
    self: *Self,
    // TODO: Write interfaces proposal for Zig
    visitor: anytype,
) !void {
    try self.global_actor.visitValues(visitor);
    if (self.genesis_actor) |actor|
        try actor.visitValues(visitor);

    for (self.regular_actors.keys()) |actor|
        try actor.visitValues(visitor);
}

pub fn isInActorMode(self: *Self) bool {
    return self.genesis_actor != null;
}

pub fn isInRegularActor(self: *Self) bool {
    return self.current_actor != self.global_actor and self.current_actor != self.genesis_actor;
}

pub fn isInGenesisActor(self: *Self) bool {
    if (self.genesis_actor) |genesis_actor| {
        return self.current_actor == genesis_actor;
    }

    return false;
}

pub fn setGenesisActor(self: *Self, actor: *Actor) void {
    std.debug.assert(!self.isInActorMode());
    self.genesis_actor = actor;
}

pub fn switchToActor(self: *Self, actor: *Actor) void {
    self.current_actor = actor;
    actor.yield_reason = .None;
}

pub fn registerRegularActor(self: *Self, actor: *Actor) !void {
    const gop = try self.regular_actors.getOrPut(self.allocator, actor);
    std.debug.assert(!gop.found_existing);
}

pub fn unregisterRegularActor(self: *Self, actor: *Actor) bool {
    return self.regular_actors.swapRemove(actor);
}

pub fn regularActorIsRegistered(self: Self, actor: *Actor) bool {
    return self.regular_actors.contains(actor);
}
