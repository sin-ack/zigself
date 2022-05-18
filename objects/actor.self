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
    |).
|).

std _AddSlots: (|
    actor = (|
        parent* = std traits actor.
    |).

    "
    Meant to be used with new actors. In order to create a new actor, use an
    inherited slot like this:

        base< = std actorBase.
    "
    actorBase = (std actor copy).
|).
