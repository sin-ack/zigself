"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    scheduler = (|
        parent* = std traits clonable.

        blockedActor = (|
            parent* = std traits clonable.
            fd. actor.
        |).

        copy = (
            clone
            ; readyQueue: readyQueue copy
            ; blockedQueue: blockedQueue copy
        ).

        startWith: obj = (startBySending: 'spawnEntrypoint' To: obj).
        startBySending: selector To: obj = (
            copy
            ; initialObject: obj
            ; initialSelector: selector
            ; _Genesis: 'main'.
        ).

        "Look at each blocked actor's file descriptor, and poll them."
        pollBlockedFDs = (| fds. events. newBlockedQueue. newEvents. |
            blockedQueue isEmpty ifTrue: [ ^ nil ].

            fds: std array copySize: blockedQueue size FillingExtrasWith: 0.
            events: fds copy.
            blockedQueue each: [| :blocked. :index |
                fds at: index Put: blocked fd.
                "FIXME: Switch to std os pollIn || std os pollOut"
                events at: index Put: 5.
            ].

            std os tryWhileInterrupted: [| :failBlock |
                newEvents: std os pollFDs: fds
                                  Events: events
                                  WaitingForMS: 1 negate
                                  IfFail: failBlock.
            ] IfFail: raiseError.

            newEvents = 0 ifTrue: [ ^ nil ].

            newBlockedQueue: blockedQueue copyRemoveAll.
            events each: [| :event. :index |
                event != 0 ifTrue: [
                    readyQueue add: (blockedQueue at: index; actor).
                ] False: [
                    newBlockedQueue add: blockedQueue at: index.
                ].
            ].

            blockedQueue: newBlockedQueue.
        ).

        main = (
            "At this point we're in actor mode and ready to schedule things."
            readyQueue add: initialObject _ActorSpawn: initialSelector.

            [| nextAvailableActor. result. yieldReason |
                readyQueue isEmpty ifFalse: [
                    nextAvailableActor: readyQueue shift.
                ].

                nil == nextAvailableActor ifTrue: [
                    pollBlockedFDs.
                    readyQueue isEmpty ifFalse: [
                        nextAvailableActor: readyQueue shift.
                    ].
                ].

                nil == nextAvailableActor ifTrue: [ ^ nil ].

                result: nextAvailableActor _ActorResume.
                yieldReason: nextAvailableActor _ActorYieldReason.

                yieldReason = std actor yieldReason blocked ifTrue: [| blocked |
                    blocked: blockedActor copy
                             ; fd: nextAvailableActor _ActorBlockedFD
                             ; actor: nextAvailableActor.
                    blockedQueue add: blocked.
                ] False: [yieldReason = std actor yieldReason yielded ifTrue: [
                    readyQueue add: nextAvailableActor.
                ] False: [yieldReason = std actor yieldReason actorSpawned ifTrue: [
                    readyQueue add: nextAvailableActor.
                    readyQueue add: result.
                ] False: [
                    (yieldReason != std actor yieldReason runtimeError) ||
                    [yieldReason != std actor yieldReason dead]
                    ifFalse: [
                        _Error: 'Unknown actor yield reason ', yieldReason asString, ' reached!'.
                    ].
                ]]].
            ] loop.
        ).
    |).
|).

std _AddSlots: (|
    scheduler = (|
        parent* = std traits scheduler.

        initialObject.
        initialSelector.

        "Queue storing actors ready to resume."
        readyQueue <- std vector copyRemoveAll.
        "Queue storing actors that are blocked on a file descriptor. Stores
         std scheduler blockedActor objects."
        blockedQueue <- std vector copyRemoveAll.
    |).
|).
