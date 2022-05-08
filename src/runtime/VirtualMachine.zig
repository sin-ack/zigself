// Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./Heap.zig");
const slot_import = @import("./slot.zig");
const Slot = slot_import.Slot;
const Actor = @import("./Actor.zig");
const Range = @import("../language/Range.zig");
const Stack = @import("./stack.zig").Stack;
const Value = @import("./value.zig").Value;
const Object = @import("./Object.zig");
const Script = @import("../language/script.zig");
const AstGen = @import("./AstGen.zig");
const CodeGen = @import("./CodeGen.zig");
const ByteArray = @import("./ByteArray.zig");
const RegisterFile = @import("./lowcode/RegisterFile.zig");
const runtime_error = @import("./error.zig");
const RegisterLocation = @import("./lowcode/register_location.zig").RegisterLocation;

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

array_traits: Heap.Tracked = undefined,
block_traits: Heap.Tracked = undefined,
float_traits: Heap.Tracked = undefined,
string_traits: Heap.Tracked = undefined,
integer_traits: Heap.Tracked = undefined,

// --- Settings ---

/// Whether the interpreter should be silent when an error happens.
silent_errors: bool = false,

// --- Current execution state ---

register_file: RegisterFile = .{},
argument_stack: Stack(Value, "Argument stack", ValueSentinel) = .{},
slot_stack: Stack(Slot, "Slot stack", SlotSentinel) = .{},
saved_register_stack: Stack(SavedRegister, "Saved register stack", null) = .{},
/// Whether the next created method is going to be an inline method.
next_method_is_inline: bool = false,
/// The currently active source range. This is updated by the source_range
/// instruction.
range: Range = .{ .start = 0, .end = 0 },

const Self = @This();

// Sentinel values for the stacks
// FIXME: These should not be pub. Make heap visit each value by calling vm.visitValues()
//        instead of explicitly reaching into the VM.
pub const ValueSentinel = Value{ .data = 0xCCCCCCCCCCCCCCCC };
pub const SlotSentinel = Slot{ .name = ValueSentinel, .properties = .{ .properties = ValueSentinel }, .value = ValueSentinel };

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
    pub fn bumpArgumentHeight(self: *StackSnapshot, vm: *Self) void {
        self.argument_height = vm.argument_stack.height();
    }
};

/// A saved register, which is restored at activation exit.
pub const SavedRegister = struct {
    /// The register to restore the value to.
    register: RegisterLocation,
    /// The value which should be restored.
    value: Value,
};

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

    const empty_map = try Object.Map.Slots.create(heap, 0);

    self.lobby_object = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());

    self.global_nil = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.global_true = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.global_false = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());

    self.array_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.block_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.float_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.string_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());
    self.integer_traits = try heap.track((try Object.Slots.create(heap, empty_map, &.{})).asValue());

    self.register_file.init();

    return self;
}

pub fn destroy(self: *Self) void {
    self.lobby_object.untrack(self.heap);
    self.global_nil.untrack(self.heap);
    self.global_true.untrack(self.heap);
    self.global_false.untrack(self.heap);
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

    self.argument_stack.deinit(self.allocator);
    self.slot_stack.deinit(self.allocator);
    self.saved_register_stack.deinit(self.allocator);

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

/// Return a block message name with the given argument count, creating it
/// if it does not exist.
///
/// A block message name looks like: `value:With:With:With:...`.
pub fn getOrCreateBlockMessageName(self: *Self, argument_count: u8) !Heap.Tracked {
    const result = try self.block_message_names.getOrPut(self.allocator, argument_count);
    if (result.found_existing) {
        return result.value_ptr.*;
    } else {
        const byte_array = try ByteArray.createUninitialized(self.heap, requiredSizeForBlockMessageName(argument_count));
        writeBlockMessageName(byte_array.getValues(), argument_count);

        const tracked_value = try self.heap.track(byte_array.asValue());
        result.value_ptr.* = tracked_value;
        return tracked_value;
    }
}

fn writeBlockMessageName(name: []u8, argument_count: u8) void {
    std.debug.assert(name.len == requiredSizeForBlockMessageName(argument_count));
    std.mem.copy(u8, name, "value");

    if (argument_count > 0) {
        name[5] = ':';

        var remaining_buffer = name[6..];
        while (remaining_buffer.len > 0) {
            std.mem.copy(u8, remaining_buffer, "With:");
            remaining_buffer = remaining_buffer[5..];
        }
    }
}

fn requiredSizeForBlockMessageName(argument_count: u8) usize {
    var needed_space: usize = 5; // value
    if (argument_count > 0) {
        needed_space += 1; // :
        needed_space += 5 * (argument_count - 1); // Any other With:s needed
    }

    return needed_space;
}

pub fn executeEntrypointScript(self: *Self, script: Script.Ref) !?Value {
    var entrypoint_ast_executable = try AstGen.generateExecutableFromScript(self.allocator, script);
    defer entrypoint_ast_executable.destroy();

    var entrypoint_executable = try CodeGen.lowerExecutable(self.allocator, entrypoint_ast_executable);
    defer entrypoint_executable.unref();

    var actor = try Actor.create(self.allocator);
    defer actor.destroy(self.allocator);

    var completion = try actor.execute(self, entrypoint_executable);
    defer completion.deinit(self);

    switch (completion.data) {
        .Normal => |value| {
            return value;
        },
        .RuntimeError => |err| {
            if (!self.silent_errors) {
                std.debug.print("Received error at top level: {s}\n", .{err.message});
                runtime_error.printTraceFromActivationStack(actor.activation_stack.getStack(), err.source_range);
            }
            return null;
        },
        else => unreachable,
    }
}

pub fn readRegister(self: Self, location: RegisterLocation) Value {
    return switch (location) {
        .zero => self.nil(),
        else => self.register_file.read(location),
    };
}

pub fn writeRegister(self: *Self, location: RegisterLocation, value: Value) void {
    self.register_file.write(location, value);
}

pub fn takeStackSnapshot(self: Self) StackSnapshot {
    return .{
        .argument_height = self.argument_stack.height(),
        .slot_height = self.slot_stack.height(),
        .saved_register_height = self.saved_register_stack.height(),
    };
}

pub fn restoreStackSnapshot(self: *Self, snapshot: StackSnapshot) void {
    self.argument_stack.restoreTo(snapshot.argument_height);
    self.slot_stack.restoreTo(snapshot.slot_height);
    self.saved_register_stack.restoreTo(snapshot.saved_register_height);
}
