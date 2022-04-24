// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Actor = @import("./Actor.zig");
const Object = @import("./object.zig");
const Completion = @import("./completion.zig");
const SourceRange = @import("./SourceRange.zig");
const runtime_error = @import("./error.zig");
const VirtualMachine = @import("./virtual_machine.zig");
const RegisterLocation = @import("./bytecode/register_location.zig").RegisterLocation;

const basic_primitives = @import("./primitives/basic.zig");
const byte_array_primitives = @import("./primitives/byte_array.zig");
const array_primitives = @import("./primitives/array.zig");
const number_primitives = @import("./primitives/number.zig");
const object_primitives = @import("./primitives/object.zig");
const system_call_primitives = @import("./primitives/system_call.zig");

/// The context passed to a primitive.
pub const PrimitiveContext = struct {
    /// The virtual machine of the interpreter.
    vm: *VirtualMachine,
    /// The actor that this primitive is executing within.
    actor: *Actor,
    /// The receiver of this primitive call. This is the object that the
    /// primitive is called on. If the primitive was called without an explicit
    /// receiver, the receiver will be equivalent to the current activation's
    /// self object (unless the self object is an activation object, in which
    /// case the receiver will be unwrapped before being passed to the
    /// primitive).
    receiver: Heap.Tracked,
    /// The arguments that were passed to this primitive. The amount is always
    /// equivalent to the amount of colons in the primitive name.
    arguments: []const Value,
    /// The location in the current activation to which the result of the activation should be written. It is fine for primitives to just return the result of the primitive as a normal completion directly, but if a new activation needs to be added, then this should be passed in.
    target_location: RegisterLocation,
    /// The source range which triggered this primitive call. This will be
    /// passed to runtime error completions in the case of errors.
    source_range: SourceRange,
};

/// A primitive specification. The `name` field specifies the exact selector the
/// primitive uses (i.e. `DoFoo:WithBar:`, or `StringPrint`), and the `function`
/// is the function which is called to execute the primitive. The `arity`
/// specifies how many arguments the primitive takes.
const PrimitiveSpec = struct {
    name: []const u8,
    arity: u8,
    function: fn (context: PrimitiveContext) Allocator.Error!?Completion,

    pub fn call(
        self: PrimitiveSpec,
        vm: *VirtualMachine,
        actor: *Actor,
        receiver: Heap.Tracked,
        arguments: []const Value,
        target_location: RegisterLocation,
        source_range: SourceRange,
    ) Allocator.Error!?Completion {
        return try self.function(PrimitiveContext{
            .vm = vm,
            .actor = actor,
            .receiver = receiver,
            .arguments = arguments,
            .target_location = target_location,
            .source_range = source_range,
        });
    }
};

const PrimitiveRegistry = &[_]PrimitiveSpec{
    // basic primitives
    .{ .name = "Nil", .arity = 0, .function = basic_primitives.Nil },
    .{ .name = "RunScript", .arity = 0, .function = basic_primitives.RunScript },
    .{ .name = "EvaluateStringIfFail:", .arity = 1, .function = basic_primitives.EvaluateStringIfFail },
    .{ .name = "Error:", .arity = 1, .function = basic_primitives.Error },
    .{ .name = "Restart", .arity = 0, .function = basic_primitives.Restart },
    // byte array primitives
    .{ .name = "ByteArraySize", .arity = 0, .function = byte_array_primitives.ByteArraySize },
    .{ .name = "ByteAt:", .arity = 1, .function = byte_array_primitives.ByteAt },
    .{ .name = "ByteAt:Put:", .arity = 2, .function = byte_array_primitives.ByteAt_Put },
    .{ .name = "ByteArrayCopySize:FillingExtrasWith:", .arity = 2, .function = byte_array_primitives.ByteArrayCopySize_FillingExtrasWith },
    .{ .name = "ByteArrayEq:", .arity = 1, .function = byte_array_primitives.ByteArrayEq },
    .{ .name = "ByteArrayConcatenate:", .arity = 1, .function = byte_array_primitives.ByteArrayConcatenate },
    // array primitives
    .{ .name = "ArrayCopySize:FillingExtrasWith:", .arity = 2, .function = array_primitives.ArrayCopySize_FillingExtrasWith },
    .{ .name = "ArraySize", .arity = 0, .function = array_primitives.ArraySize },
    .{ .name = "ArrayAt:", .arity = 1, .function = array_primitives.ArrayAt },
    .{ .name = "ArrayAt:Put:", .arity = 2, .function = array_primitives.ArrayAt_Put },
    // number primitives
    .{ .name = "IntAdd:", .arity = 1, .function = number_primitives.IntAdd },
    .{ .name = "IntSub:", .arity = 1, .function = number_primitives.IntSub },
    .{ .name = "IntMul:", .arity = 1, .function = number_primitives.IntMul },
    .{ .name = "IntDiv:", .arity = 1, .function = number_primitives.IntDiv },
    .{ .name = "IntMod:", .arity = 1, .function = number_primitives.IntMod },
    .{ .name = "IntShl:", .arity = 1, .function = number_primitives.IntShl },
    .{ .name = "IntShr:", .arity = 1, .function = number_primitives.IntShr },
    .{ .name = "IntXor:", .arity = 1, .function = number_primitives.IntXor },
    .{ .name = "IntAnd:", .arity = 1, .function = number_primitives.IntAnd },
    .{ .name = "IntLT:", .arity = 1, .function = number_primitives.IntLT },
    .{ .name = "IntEq:", .arity = 1, .function = number_primitives.IntEq },
    .{ .name = "IntGT:", .arity = 1, .function = number_primitives.IntGT },
    // object primitives
    .{ .name = "AddSlots:", .arity = 1, .function = object_primitives.AddSlots },
    // .{ .name = "_RemoveSlot:IfFail:", .function = object_primitives.RemoveSlot_IfFail },
    .{ .name = "Inspect", .arity = 0, .function = object_primitives.Inspect },
    .{ .name = "Clone", .arity = 0, .function = object_primitives.Clone },
    .{ .name = "Eq:", .arity = 1, .function = object_primitives.Eq },
    // System call primitives
    .{ .name = "Open:WithFlags:IfFail:", .arity = 3, .function = system_call_primitives.Open_WithFlags_IfFail },
    .{ .name = "Read:BytesInto:AtOffset:From:IfFail:", .arity = 5, .function = system_call_primitives.Read_BytesInto_AtOffset_From_IfFail },
    .{ .name = "Write:BytesFrom:AtOffset:Into:IfFail:", .arity = 5, .function = system_call_primitives.Write_BytesFrom_AtOffset_Into_IfFail },
    .{ .name = "Close:", .arity = 1, .function = system_call_primitives.Close },
    .{ .name = "Exit:", .arity = 1, .function = system_call_primitives.Exit },
};

// FIXME: This is very naive! We shouldn't need to linear search every single
//        time.
pub fn getPrimitive(selector: []const u8) ?PrimitiveSpec {
    for (PrimitiveRegistry) |primitive| {
        if (std.mem.eql(u8, primitive.name, selector)) {
            return primitive;
        }
    }

    return null;
}
