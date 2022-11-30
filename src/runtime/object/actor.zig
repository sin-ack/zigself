// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("../Heap.zig");
const Slot = @import("../slot.zig").Slot;
const Actor = @import("../Actor.zig");
const value = @import("../value.zig");
const Value = value.Value;
const Stack = @import("../stack.zig").Stack;
const Object = @import("../Object.zig");
const bytecode = @import("../bytecode.zig");
const BoolValue = value.BoolValue;
const EnumValue = value.EnumValue;
const Activation = @import("../Activation.zig");
const ActorValue = value.ActorValue;
const Completion = @import("../Completion.zig");
const RangeValue = value.RangeValue;
const MethodValue = value.MethodValue;
const SourceRange = @import("../SourceRange.zig");
const stage2_compat = @import("../../utility/stage2_compat.zig");
const VirtualMachine = @import("../VirtualMachine.zig");
const ActivationStack = Activation.ActivationStack;
const RegisterLocation = @import("../bytecode.zig").RegisterLocation;
const OptionalActorValue = value.OptionalActorValue;
const IntegerValueAdvanced = value.IntegerValueAdvanced;
const OptionalManagedValue = value.OptionalManagedValue;
const OptionalByteArrayValue = value.OptionalByteArrayValue;

comptime {
    @compileLog(@sizeOf(ActorObject));
}

/// An actor object which is the object that the genesis actor interacts with in
/// Self code.
pub const ActorObject = extern struct {
    header: Object.Header align(@alignOf(u64)),

    /// The actor context, which is the object the actor spawn activation
    /// returns.
    context: Value align(@alignOf(u64)),
    /// The selector that will be sent to the actor context after the actor
    /// spawn message has been sent to the actor. This selector must be set via
    /// _ActorSetEntrypoint: in the actor spawn method.
    entrypoint_selector: OptionalByteArrayValue align(@alignOf(u64)) = OptionalByteArrayValue.init(null),
    /// The file descriptor managed object that this actor was blocked on, if
    /// any.
    blocked_fd: OptionalManagedValue align(@alignOf(u64)) = OptionalManagedValue.init(null),
    /// The reason this actor has yielded.
    yield_reason: EnumValue(YieldReason) align(@alignOf(u64)) = EnumValue(YieldReason).init(.None),
    /// The actor object that sent the current message. Can be queried inside
    /// messages with _ActorSender. It is an error to query it outside of
    /// messages.
    // message_sender: OptionalActorValue align(@alignOf(u64)) = OptionalActorValue.init(null),
    /// The register file stores the register values for this actor. Lowcode
    /// execution uses these Segisters to perform its operations.
    //register_file: bytecode.lowcode.RegisterFile align(@alignOf(u64)) = .{},
    /// Whether the next created method is going to be an inline method.
    next_method_is_inline: BoolValue = BoolValue.init(false),
    /// The currently active source range. This is updated by the source_range
    /// instruction.
    range: RangeValue = RangeValue.init(.{ .start = 0, .end = 0 }),
    /// The ID of this actor, which determines the ownership of each object in the
    /// system.
    id: IntegerValueAdvanced(u31),

    /// The activation stack stores the list of activations that are currently
    /// on this actor. When an activation is exited, execution flow returns to
    /// the previous activation on the stack. If a non-local return happens,
    /// however, the control flow instead returns to the non-local return
    /// target.
    activation_stack: ActivationStack align(@alignOf(u64)),
    /// The mailbox stores the messages that were sent to this actor through
    /// actor proxy objects.
    //mailbox: Mailbox align(@alignOf(u64)) = .{},
    // The various stacks.
    //argument_stack: Stack(Value, "Argument stack", ValueSentinel) align(@alignOf(u64)) = .{},
    //slot_stack: Stack(Slot, "Slot stack", SlotSentinel) align(@alignOf(u64)) = .{},
    //saved_register_stack: Stack(SavedRegister, "Saved register stack", null) align(@alignOf(u64)) = .{},

    pub const Ptr = stage2_compat.HeapPtr(ActorObject, .Mutable);

    /// A snapshot of the current heights of each stack in the VM which can be
    /// restored after a non-local return.
    pub const StackSnapshot = struct {
        argument_height: usize,
        slot_height: usize,
        saved_register_height: usize,

        /// Bump just the argument stack height. This is necessary because the stack
        /// snapshot for an activation is created while the stack still contains the
        /// arguments for the activation, meaning the stack will be higher than it
        /// actually is when the activation is entered.
        pub fn bumpArgumentHeight(self: *StackSnapshot, actor: ActorObject.Ptr) void {
            self.argument_height = actor.argument_stack.height();
        }
    };

    /// A saved register, which is restored at activation exit.
    pub const SavedRegister = struct {
        /// The register to restore the value to.
        register: bytecode.RegisterLocation,
        /// The value which should be restored.
        value: Value,
    };

    /// The result of actor execution.
    pub const ActorResult = union(enum) {
        ActorSwitch,
        Completion: Completion,
    };

    /// The reason this actor has yielded.
    // NOTE: Keep in sync with objects/actor.self!
    pub const YieldReason = enum(u32) {
        /// This actor hasn't yielded.
        None = 0,
        /// The actor has received a runtime error.
        RuntimeError = 1,
        /// The actor has blocked on a primitive.
        Blocked = 2,
        /// The _ActorYield primitive was sent.
        Yielded = 3,
        /// The actor has finished its execution normally.
        Dead = 4,
        /// The actor has sent _ActorSpawn: which has spawned another actor. The
        /// return value of _ActorResume will be the newly spawned actor object.
        ActorSpawned = 5,
    };

    /// A single message sent to this actor from another actor.
    pub const Message = struct {
        /// The actor object that sent this message.
        sender: ActorValue,
        /// The method that will be executed on the actor context. Belongs to this
        /// actor.
        method: MethodValue,
        /// The arguments of this message as an owned slice.
        arguments: []Value,
        /// The SourceRange which spawned this message.
        source_range: SourceRange,

        pub fn deinit(self: *Message, allocator: Allocator) void {
            allocator.free(self.arguments);
        }
    };

    const Mailbox = std.TailQueue(Message);
    // Sentinel values for the stacks
    pub const ValueSentinel = Value{ .data = 0xCCCCCCCCCCCCCCCC };
    pub const SlotSentinel = Slot{ .name = ValueSentinel, .properties = .{ .properties = ValueSentinel }, .value = ValueSentinel };

    pub fn create(map_map: Value, token: *Heap.AllocationToken, genesis_actor_id: u31, actor: ActorObject.Ptr, context: Value) !ActorObject.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self = @ptrCast(ActorObject.Ptr, memory_area);
        self.init(genesis_actor_id, map_map, actor, context);

        try token.heap.markAddressAsNeedingFinalization(memory_area);
        return self;
    }

    fn init(self: ActorObject.Ptr, genesis_actor_id: u31, actor_map: Value, context: Value) void {
        self.header.init(.Actor, genesis_actor_id, actor_map);
        self.context = context;
    }

    pub fn asObjectAddress(self: ActorObject.Ptr) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: ActorObject.Ptr) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getSizeInMemory(self: ActorObject.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    /// Return the amount of bytes that this actor needs to spawn itself. Note
    /// that if spawning this actor, the activation's cost should be accounted
    /// for.
    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ActorObject);
    }

    pub fn finalize(self: ActorObject.Ptr, allocator: Allocator) void {
        // FIXME: Ask the VM whether the actor has been quit by the time we
        //        reach here.
        self.actor.get().destroy(allocator);
    }
};

/// An actor proxy which relays message to the mailbox of an actor.
pub const ActorProxyObject = extern struct {
    header: Object.Header align(@alignOf(u64)),
    /// The Actor object to proxy messages to.
    actor_object: ActorValue align(@alignOf(u64)),

    pub const Ptr = stage2_compat.HeapPtr(ActorProxyObject, .Mutable);

    /// Create the Actor object without sending a message to it.
    pub fn create(map_map: Value, token: *Heap.AllocationToken, current_actor_id: u31, actor_object: ActorObject.Ptr) ActorProxyObject.Ptr {
        const memory_area = token.allocate(.Object, requiredSizeForAllocation());
        const self = @ptrCast(ActorProxyObject.Ptr, memory_area);
        self.init(current_actor_id, map_map, actor_object);
        return self;
    }

    fn init(self: ActorProxyObject.Ptr, current_actor_id: u31, actor_map: Value, actor_object: ActorObject.Ptr) void {
        self.header.init(.ActorProxy, current_actor_id, actor_map);
        self.actor_object = ActorValue.init(actor_object);
    }

    pub fn asObjectAddress(self: ActorProxyObject.Ptr) [*]u64 {
        return @ptrCast([*]u64, @alignCast(@alignOf(u64), self));
    }

    pub fn asValue(self: ActorProxyObject.Ptr) Value {
        return Value.fromObjectAddress(self.asObjectAddress());
    }

    pub fn getActorObject(self: ActorProxyObject.Ptr) *ActorObject {
        return self.actor_object.get();
    }

    pub fn clone(self: ActorProxyObject.Ptr, token: *Heap.AllocationToken, actor_id: u31) ActorProxyObject.Ptr {
        const map_map = self.header.map_pointer;
        return create(map_map, token, actor_id, self.getActorObject());
    }

    pub fn getSizeInMemory(self: ActorProxyObject.Ptr) usize {
        _ = self;
        return requiredSizeForAllocation();
    }

    pub fn requiredSizeForAllocation() usize {
        return @sizeOf(ActorProxyObject);
    }
};
