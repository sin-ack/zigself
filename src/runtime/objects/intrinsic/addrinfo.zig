// Copyright (c) 2023-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");
const Allocator = std.mem.Allocator;

const Map = @import("../../map.zig").Map;
const Actor = @import("../../Actor.zig");
const Value = @import("../../value.zig").Value;
const heap_import = @import("../../Heap.zig");
const IntrinsicMap = @import("../../intrinsic_map.zig").IntrinsicMap;
const VirtualMachine = @import("../../VirtualMachine.zig");
const ByteArrayObject = @import("../byte_array.zig").ByteArray;

/// The addrinfo object is used by the _GetAddrInfoForHost:... primitive to
/// return results.
pub const AddrInfo = AddrInfoMap.ObjectType;
pub const AddrInfoMap = extern struct {
    map: Map align(@alignOf(u64)),
    family: Value align(@alignOf(u64)),
    socketType: Value align(@alignOf(u64)),
    protocol: Value align(@alignOf(u64)),
    flags: Value align(@alignOf(u64)),
    sockaddrBytes: Value align(@alignOf(u64)),

    // TODO: Verify that we actually unpacked all fields from IntrinsicMap.
    const intrinsic_map = IntrinsicMap(AddrInfoMap, .AddrInfo);
    pub const Ptr = intrinsic_map.Ptr;
    pub const ObjectType = intrinsic_map.ObjectType;
    pub const asAddress = intrinsic_map.asAddress;
    pub const asValue = intrinsic_map.asValue;
    pub const getSizeInMemory = intrinsic_map.getSizeInMemory;
    pub const getSizeForCloning = intrinsic_map.getSizeForCloning;
    pub const finalize = intrinsic_map.finalize;
    pub const visitEdges = intrinsic_map.visitEdges;
    pub const requiredSizeForAllocation = intrinsic_map.requiredSizeForAllocation;
    pub const createObject = intrinsic_map.createObject;

    pub fn createFromAddrinfo(allocator: Allocator, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken, actor_id: Actor.ActorID, addrinfo: *std.posix.addrinfo) !AddrInfoMap.Ptr {
        const sockaddr_memory: [*]u8 = @ptrCast(addrinfo.addr.?);
        const sockaddr_bytes_object = try ByteArrayObject.createWithValues(allocator, heap, token, actor_id, sockaddr_memory[0..addrinfo.addrlen]);

        const self = create(token);
        self.family = Value.fromInteger(addrinfo.family);
        self.socketType = Value.fromInteger(addrinfo.socktype);
        self.protocol = Value.fromInteger(addrinfo.protocol);
        self.flags = Value.fromInteger(@as(u32, @bitCast(addrinfo.flags)));
        self.sockaddrBytes = sockaddr_bytes_object.asValue();

        return self;
    }

    fn create(token: *heap_import.AllocationToken) AddrInfoMap.Ptr {
        const memory = token.allocate(AddrInfoMap.requiredSizeForAllocation());
        const self: AddrInfoMap.Ptr = @ptrCast(memory);

        self.init();
        return self;
    }

    fn init(self: AddrInfoMap.Ptr) void {
        self.map.init(.AddrInfo);
    }

    pub fn clone(self: AddrInfoMap.Ptr, heap: *VirtualMachine.Heap, token: *heap_import.AllocationToken) AddrInfoMap.Ptr {
        _ = heap;

        const new_map = create(token);
        new_map.flags = self.flags;
        new_map.family = self.family;
        new_map.socketType = self.socketType;
        new_map.protocol = self.protocol;
        new_map.sockaddrBytes = self.sockaddrBytes;

        return new_map;
    }

    pub fn requiredSizeToCreateFromAddrinfo() usize {
        return AddrInfoMap.requiredSizeForAllocation() + ByteArrayObject.requiredSizeForAllocation();
    }
};
