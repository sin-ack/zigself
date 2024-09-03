// Copyright (c) 2023-2024, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Map = @import("../../map.zig").Map;
const Heap = @import("../../Heap.zig");
const Actor = @import("../../Actor.zig");
const Value = @import("../../value.zig").Value;
const IntrinsicMap = @import("../../intrinsic_map.zig").IntrinsicMap;
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

    pub usingnamespace IntrinsicMap(AddrInfoMap, .AddrInfo);

    pub fn createFromAddrinfo(token: *Heap.AllocationToken, actor_id: Actor.ActorID, addrinfo: *std.posix.addrinfo) AddrInfoMap.Ptr {
        const sockaddr_memory: [*]u8 = @ptrCast(addrinfo.addr.?);
        const sockaddr_bytes_object = ByteArrayObject.createWithValues(token, actor_id, sockaddr_memory[0..addrinfo.addrlen]);

        const self = create(token);
        self.family = Value.fromInteger(addrinfo.family);
        self.socketType = Value.fromInteger(addrinfo.socktype);
        self.protocol = Value.fromInteger(addrinfo.protocol);
        self.flags = Value.fromInteger(addrinfo.flags);
        self.sockaddrBytes = sockaddr_bytes_object.asValue();

        return self;
    }

    fn create(token: *Heap.AllocationToken) AddrInfoMap.Ptr {
        const memory = token.allocate(.Object, AddrInfoMap.requiredSizeForAllocation());
        const self: AddrInfoMap.Ptr = @ptrCast(memory);

        self.init();
        return self;
    }

    fn init(self: AddrInfoMap.Ptr) void {
        self.map.init(.AddrInfo);
    }

    pub fn clone(self: AddrInfoMap.Ptr, token: *Heap.AllocationToken) AddrInfoMap.Ptr {
        const new_map = create(token);
        new_map.flags = self.flags;
        new_map.family = self.family;
        new_map.socketType = self.socketType;
        new_map.protocol = self.protocol;
        new_map.sockaddrBytes = self.sockaddrBytes;

        return new_map;
    }

    pub fn requiredSizeToCreateFromAddrinfo(addrinfo: *std.posix.addrinfo) usize {
        return AddrInfoMap.requiredSizeForAllocation() + ByteArrayObject.requiredSizeForAllocation(addrinfo.addrlen);
    }
};
