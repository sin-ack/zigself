// Copyright (c) 2023, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

const Map = @import("../map.zig").Map;
const Heap = @import("../../Heap.zig");
const Value = @import("../../value.zig").Value;
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

    pub usingnamespace IntrinsicMap(AddrInfoMap, .AddrInfo);

    pub fn createFromAddrinfo(map_map: Map.Ptr, token: *Heap.AllocationToken, actor_id: u31, addrinfo: *std.os.addrinfo) AddrInfoMap.Ptr {
        const sockaddr_memory: [*]u8 = @ptrCast(addrinfo.addr.?);
        const sockaddr_bytes_object = ByteArrayObject.createWithValues(map_map, token, actor_id, sockaddr_memory[0..addrinfo.addrlen]);

        const self = create(map_map, token);
        self.family = Value.fromInteger(addrinfo.family);
        self.socketType = Value.fromInteger(addrinfo.socktype);
        self.protocol = Value.fromInteger(addrinfo.protocol);
        self.flags = Value.fromInteger(addrinfo.flags);
        self.sockaddrBytes = sockaddr_bytes_object.asValue();

        return self;
    }

    fn create(map_map: Map.Ptr, token: *Heap.AllocationToken) AddrInfoMap.Ptr {
        const memory = token.allocate(.Object, AddrInfoMap.requiredSizeForAllocation());
        const self: AddrInfoMap.Ptr = @ptrCast(memory);

        self.init(map_map);
        return self;
    }

    fn init(self: AddrInfoMap.Ptr, map_map: Map.Ptr) void {
        self.map.init(.AddrInfo, map_map);
    }

    pub fn clone(self: AddrInfoMap.Ptr, vm: *VirtualMachine, token: *Heap.AllocationToken) AddrInfoMap.Ptr {
        const new_map = create(vm.getMapMap(), token);
        new_map.flags = self.flags;
        new_map.family = self.family;
        new_map.socketType = self.socketType;
        new_map.protocol = self.protocol;
        new_map.sockaddrBytes = self.sockaddrBytes;

        return new_map;
    }

    pub fn requiredSizeToCreateFromAddrinfo(addrinfo: *std.os.addrinfo) usize {
        return AddrInfoMap.requiredSizeForAllocation() + ByteArrayObject.requiredSizeForAllocation(addrinfo.addrlen);
    }
};
