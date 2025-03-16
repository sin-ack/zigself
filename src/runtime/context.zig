// Copyright (c) 2023-2025, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! This file holds the "current context" of a VM thread. It holds the VirtualMachine object
//! and the current actor for now.

const builtin = @import("builtin");
const std = @import("std");

const Actor = @import("Actor.zig");
const VirtualMachine = @import("VirtualMachine.zig");

threadlocal var vm: ?*VirtualMachine = null;
threadlocal var actor: ?*Actor = null;

pub fn getVM() *VirtualMachine {
    if (builtin.mode == .Debug) {
        if (vm) |v| return v;
        @panic("!!! Attempting to access current thread's VM, but it is not set!");
    } else {
        return vm.?;
    }
}

pub fn getHeap() *VirtualMachine.Heap {
    return &getVM().heap;
}

pub fn getActor() *Actor {
    if (builtin.mode == .Debug) {
        if (actor) |a| return a;
        @panic("!!! Attempting to access current thread's Actor, but it is not set!");
    } else {
        return actor.?;
    }
}

// TODO: These functions should actually "push onto" and "pop from" a stack of
//       values instead of setting the current value. This will be required once
//       we have more than one actor (or VM!) running.

pub fn pushVM(new_vm: *VirtualMachine) void {
    // The least we can do is make sure we're not overwriting an existing VM.
    std.debug.assert(vm == null);
    vm = new_vm;
}

pub fn popVM() *VirtualMachine {
    std.debug.assert(vm != null);

    const old_vm = vm.?;
    vm = null;
    return old_vm;
}

pub fn pushActor(new_actor: *Actor) void {
    std.debug.assert(actor == null);
    actor = new_actor;
}

pub fn popActor() *Actor {
    std.debug.assert(actor != null);

    const old_actor = actor.?;
    actor = null;
    return old_actor;
}
