// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./heap.zig");
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig");
const Completion = @import("./completion.zig");
const SourceRange = @import("../language/source_range.zig");
const runtime_error = @import("./error.zig");
const interpreter = @import("./interpreter.zig");
const InterpreterContext = interpreter.InterpreterContext;

const basic_primitives = @import("./primitives/basic.zig");
const byte_array_primitives = @import("./primitives/byte_array.zig");
const array_primitives = @import("./primitives/array.zig");
const number_primitives = @import("./primitives/number.zig");
const object_primitives = @import("./primitives/object.zig");
const system_call_primitives = @import("./primitives/system_call.zig");

/// The context passed to a primitive.
pub const PrimitiveContext = struct {
    /// The current context of the interpreter.
    interpreter_context: *InterpreterContext,
    /// The receiver of this primitive call. This is the object that the
    /// primitive is called on. If the primitive was called without an explicit
    /// receiver, the receiver will be equivalent to context.self_object (unless
    /// context.self_object is an activation object, in which case the receiver
    /// will be unwrapped before being passed to the primitive).
    receiver: Heap.Tracked,
    /// The arguments that were passed to this primitive. The amount is always
    /// equivalent to the amount of colons in the primitive name.
    arguments: []const Heap.Tracked,
    /// The source range which triggered this primitive call. This will be
    /// passed to runtime error completions in the case of errors.
    source_range: SourceRange,
};

/// A primitive specification. The `name` field specifies the exact selector the
/// primitive uses (i.e. `_DoFoo:WithBar:`, or `_StringPrint`), and the
/// `function` is the function which is called to execute the primitive.
const PrimitiveSpec = struct {
    name: []const u8,
    function: fn (context: PrimitiveContext) interpreter.InterpreterError!Completion,

    pub fn call(
        self: PrimitiveSpec,
        context: *InterpreterContext,
        receiver: Heap.Tracked,
        arguments: []const Heap.Tracked,
        source_range: SourceRange,
    ) interpreter.InterpreterError!Completion {
        return try self.function(PrimitiveContext{
            .interpreter_context = context,
            .receiver = receiver,
            .arguments = arguments,
            .source_range = source_range,
        });
    }
};

const PrimitiveRegistry = &[_]PrimitiveSpec{
    // basic primitives
    .{ .name = "_Nil", .function = basic_primitives.Nil },
    .{ .name = "_Exit:", .function = basic_primitives.Exit },
    .{ .name = "_RunScript", .function = basic_primitives.RunScript },
    .{ .name = "_EvaluateString", .function = basic_primitives.EvaluateString },
    .{ .name = "_Error:", .function = basic_primitives.Error },
    .{ .name = "_Restart", .function = basic_primitives.Restart },
    // byte array primitives
    .{ .name = "_StringPrint", .function = byte_array_primitives.StringPrint },
    .{ .name = "_ByteArraySize", .function = byte_array_primitives.ByteArraySize },
    .{ .name = "_ByteAt:", .function = byte_array_primitives.ByteAt },
    .{ .name = "_ByteAt:Put:", .function = byte_array_primitives.ByteAt_Put },
    .{ .name = "_ByteArrayCopySize:FillingExtrasWith:", .function = byte_array_primitives.ByteArrayCopySize_FillingExtrasWith },
    .{ .name = "_ByteArrayEq:", .function = byte_array_primitives.ByteArrayEq },
    .{ .name = "_ByteArrayConcatenate:", .function = byte_array_primitives.ByteArrayConcatenate },
    // array primitives
    .{ .name = "_ArrayCopySize:FillingExtrasWith:", .function = array_primitives.ArrayCopySize_FillingExtrasWith },
    .{ .name = "_ArraySize", .function = array_primitives.ArraySize },
    .{ .name = "_ArrayAt:", .function = array_primitives.ArrayAt },
    .{ .name = "_ArrayAt:Put:", .function = array_primitives.ArrayAt_Put },
    // number primitives
    .{ .name = "_IntAdd:", .function = number_primitives.IntAdd },
    .{ .name = "_IntSub:", .function = number_primitives.IntSub },
    .{ .name = "_IntMul:", .function = number_primitives.IntMul },
    .{ .name = "_IntDiv:", .function = number_primitives.IntDiv },
    .{ .name = "_IntMod:", .function = number_primitives.IntMod },
    .{ .name = "_IntLT:", .function = number_primitives.IntLT },
    .{ .name = "_IntEq:", .function = number_primitives.IntEq },
    .{ .name = "_IntGT:", .function = number_primitives.IntGT },
    // object primitives
    .{ .name = "_AddSlots:", .function = object_primitives.AddSlots },
    // .{ .name = "_RemoveSlot:IfFail:", .function = object_primitives.RemoveSlot_IfFail },
    .{ .name = "_Inspect", .function = object_primitives.Inspect },
    .{ .name = "_Clone", .function = object_primitives.Clone },
    .{ .name = "_Eq:", .function = object_primitives.Eq },
    // System call primitives
    .{ .name = "_Open:WithFlags:IfFail:", .function = system_call_primitives.Open_WithFlags_IfFail },
    .{ .name = "_Read:BytesInto:AtOffset:From:IfFail:", .function = system_call_primitives.Read_BytesInto_AtOffset_From_IfFail },
    .{ .name = "_Write:BytesFrom:AtOffset:Into:IfFail:", .function = system_call_primitives.Write_BytesFrom_AtOffset_Into_IfFail },
    .{ .name = "_Close:", .function = system_call_primitives.Close },
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
