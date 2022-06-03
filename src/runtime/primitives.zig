// Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Heap = @import("./Heap.zig");
const Value = value_import.Value;
const Actor = @import("./Actor.zig");
const Object = @import("./Object.zig");
const Completion = @import("./Completion.zig");
const SourceRange = @import("./SourceRange.zig");
const value_import = @import("./value.zig");
const runtime_error = @import("./error.zig");
const VirtualMachine = @import("./VirtualMachine.zig");
const ExecutionResult = @import("./interpreter.zig").ExecutionResult;
const RegisterLocation = @import("./lowcode/register_location.zig").RegisterLocation;
const IntegerValueSignedness = value_import.IntegerValueSignedness;

const basic_primitives = @import("./primitives/basic.zig");
const byte_array_primitives = @import("./primitives/byte_array.zig");
const array_primitives = @import("./primitives/array.zig");
const number_primitives = @import("./primitives/number.zig");
const object_primitives = @import("./primitives/object.zig");
const system_call_primitives = @import("./primitives/system_call.zig");
const actor_primitives = @import("./primitives/actor.zig");

const PrimitiveError = Allocator.Error || error{GetArgumentFailure};

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
    /// The location in the current activation to which the result of the
    /// activation should be written. It is fine for primitives to just return
    /// the result of the primitive as a normal completion directly, but if a
    /// new activation needs to be added, then this should be passed in.
    target_location: RegisterLocation,
    /// The source range which triggered this primitive call. This will be
    /// passed to runtime error completions in the case of errors.
    source_range: SourceRange,
    /// The error we received when getting arguments. This is set by
    /// PrimitiveArguments when an error is received (who also returns a
    /// GetArgumentFailure error).
    get_argument_error: ?ExecutionResult = null,

    /// Send this value to PrimitiveArguments.get* functions to get the
    /// receiver.
    pub const Receiver: isize = -1;

    pub fn getArguments(
        self: *PrimitiveContext,
        comptime primitive_name: []const u8,
    ) PrimitiveArguments(primitive_name) {
        return PrimitiveArguments(primitive_name).init(self);
    }
};

/// Helper struct for returning typed values for primitives (and returning
/// errors when the arguments don't match the specified types).
fn PrimitiveArguments(comptime primitive_name: []const u8) type {
    return struct {
        /// The context lifeline.
        context: *PrimitiveContext,

        const Self = @This();

        pub fn init(context: *PrimitiveContext) Self {
            return .{ .context = context };
        }

        pub inline fn getValue(self: Self, comptime index: isize) Value {
            return switch (index) {
                PrimitiveContext.Receiver => self.context.receiver.getValue(),
                0...std.math.maxInt(isize) => self.context.arguments[index],
                else => unreachable,
            };
        }

        fn IntegerType(comptime signedness: IntegerValueSignedness) type {
            return switch (signedness) {
                .Signed => i64,
                .Unsigned => u64,
            };
        }

        fn getArgumentMessage(comptime index: isize) []const u8 {
            return switch (index) {
                PrimitiveContext.Receiver => " receiver of ",
                0...std.math.maxInt(isize) => std.fmt.comptimePrint(" argument {} of ", .{index + 1}),
                else => unreachable,
            };
        }

        pub inline fn getInteger(self: Self, comptime index: isize, comptime signedness: IntegerValueSignedness) !IntegerType(signedness) {
            const value = self.getValue(index);

            if (!value.isInteger()) {
                self.context.get_argument_error = ExecutionResult.completion(try Completion.initRuntimeError(
                    self.context.vm,
                    self.context.source_range,
                    "Expected integer for" ++ getArgumentMessage(index) ++ primitive_name,
                    .{},
                ));
                return error.GetArgumentFailure;
            }

            const value_as_integer = value.asInteger();
            if (signedness == .Signed) return value_as_integer;

            if (value_as_integer < 0) {
                self.context.get_argument_error = ExecutionResult.completion(try Completion.initRuntimeError(
                    self.context.vm,
                    self.context.source_range,
                    "Expected positive integer for" ++ getArgumentMessage(index) ++ primitive_name,
                    .{},
                ));
                return error.GetArgumentFailure;
            }

            return @intCast(u64, value_as_integer);
        }

        pub inline fn getObject(self: Self, comptime index: isize, comptime object_type: Object.ObjectType) !*Object.ObjectT(object_type) {
            const value = self.getValue(index);

            if (value.isObjectReference()) {
                if (value.asObject().asType(object_type)) |object|
                    return object;
            }

            self.context.get_argument_error = ExecutionResult.completion(try Completion.initRuntimeError(
                self.context.vm,
                self.context.source_range,
                "Expected " ++ Object.humanReadableNameFor(object_type) ++ " for" ++ getArgumentMessage(index) ++ primitive_name,
                .{},
            ));
            return error.GetArgumentFailure;
        }
    };
}

/// A primitive specification. The `name` field specifies the exact selector the
/// primitive uses (i.e. `DoFoo:WithBar:`, or `StringPrint`), and the `function`
/// is the function which is called to execute the primitive. The `arity`
/// specifies how many arguments the primitive takes.
const PrimitiveSpec = struct {
    name: []const u8,
    arity: u8,
    function: fn (context: *PrimitiveContext) PrimitiveError!ExecutionResult,

    pub fn call(
        self: PrimitiveSpec,
        vm: *VirtualMachine,
        actor: *Actor,
        receiver: Heap.Tracked,
        arguments: []const Value,
        target_location: RegisterLocation,
        source_range: SourceRange,
    ) PrimitiveError!ExecutionResult {
        var context = PrimitiveContext{
            .vm = vm,
            .actor = actor,
            .receiver = receiver,
            .arguments = arguments,
            .target_location = target_location,
            .source_range = source_range,
        };

        return self.function(&context) catch |err| switch (err) {
            error.GetArgumentFailure => context.get_argument_error.?,
            else => |e| e,
        };
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
    .{ .name = "IntOr:", .arity = 1, .function = number_primitives.IntOr },
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
    .{ .name = "PollFDs:Events:WaitingForMS:IfFail:", .arity = 4, .function = system_call_primitives.PollFDs_Events_WaitingForMS_IfFail },
    .{ .name = "GetAddrInfoForHost:Port:Family:SocketType:Protocol:Flags:IfFail:", .arity = 7, .function = system_call_primitives.GetAddrInfoForHost_Port_Family_SocketType_Protocol_Flags_IfFail },
    // Actor primitives
    .{ .name = "Genesis:", .arity = 1, .function = actor_primitives.Genesis },
    .{ .name = "ActorSpawn:", .arity = 1, .function = actor_primitives.ActorSpawn },
    .{ .name = "ActorSetEntrypoint:", .arity = 1, .function = actor_primitives.ActorSetEntrypoint },
    .{ .name = "ActorResume", .arity = 0, .function = actor_primitives.ActorResume },
    .{ .name = "ActorYieldReason", .arity = 0, .function = actor_primitives.ActorYieldReason },
    .{ .name = "ActorYield", .arity = 0, .function = actor_primitives.ActorYield },
    .{ .name = "ActorSender", .arity = 0, .function = actor_primitives.ActorSender },
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
