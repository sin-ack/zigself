(|
    parent* = std testing test.

    firstActor = (|
        parent* = std testing test.

        secondActor = (|
            parent* = std testing test.

            receivedMessage <- false.

            spawn = (
                _ActorSetEntrypoint: 'main'.
                copy
            ).

            main = (
                _ActorYield.
                expectToBeTrue: receivedMessage.
            ).

            hello: msg = (
                expect: msg ToBe: 'world'.
                receivedMessage: true.
            ).
        |).

        spawn = (
            _ActorSetEntrypoint: 'main'.
            copy
        ).

        main = (| proxy |
            proxy: secondActor _ActorSpawn: 'spawn'.
            proxy hello: 'world'.
        ).
    |).

    run = (| a. b |
        a: firstActor _ActorSpawn: 'spawn'.

        b: a _ActorResume.
        expect: a _ActorYieldReason ToBe: std actor yieldReason actorSpawned.

        b _ActorResume.
        expect: b _ActorYieldReason ToBe: std actor yieldReason yielded.

        a _ActorResume.
        expect: a _ActorYieldReason ToBe: std actor yieldReason dead.

        b _ActorResume.
        expect: b _ActorYieldReason ToBe: std actor yieldReason dead.
    ).
|) _Genesis: 'run'.
