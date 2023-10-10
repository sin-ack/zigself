"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    actor = (|
        parent* = std traits clonable.

        "NOTE: Keep in sync with src/runtime/Actor.zig!"
        yieldReason = (|
            "This actor hasn't yielded."
            none = 0.
            "The actor has received a runtime error."
            runtimeError = 1.
            "The actor has blocked on a primitive."
            blocked = 2.
            "The _ActorYield primitive was sent."
            yielded = 3.
            "The actor has finished its execution normally."
            dead = 4.
            "The actor has sent _ActorSpawn: which has spawned another actor.
             The return value of _ActorResume will be the newly spawned actor
             object."
            actorSpawned = 5.
        |).

        "Override this in your object in order to change the entrypoint."
        entrypoint = 'main'.

        "Send this message to spawn a new actor."
        spawn = (
            _ActorSpawn: 'spawnEntrypoint'.
        ).

        spawnEntrypoint = (
            _ActorSetEntrypoint: entrypoint.
            copy
        ).

        main = (
            _Error: 'The actor is missing an entrypoint!'.
        ).

        yield = (_ActorYield).
        sender = (_ActorSender).
    |).
|).

std _AddSlots: (|
    actor = (|
        parent* = std traits actor.
    |).
|).
