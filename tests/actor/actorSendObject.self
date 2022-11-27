(|
    parent* = std testing test.

    testWithObject: objectToSend = (
        (|
            parent* = std testing test.

            firstActor = (|
                parent* = std testing test.

                objectToSend = objectToSend.

                secondActor = (|
                    parent* = std testing test.

                    gotMessage <- false.

                    spawn = (
                        _ActorSetEntrypoint: 'main'.
                        copy
                    ).

                    main = (
                        expectToBeTrue: gotMessage.
                    ).

                    message: object = (
                        "Accessing this object requires it to be owned by this actor."
                        gotMessage: object inner value.
                    ).
                |).

                spawn = (
                    _ActorSetEntrypoint: 'main'.
                    copy
                ).

                main = (| second |
                    second: secondActor _ActorSpawn: 'spawn'.
                    second message: objectToSend.
                ).
            |).

            run = (| first. second |
                first: firstActor _ActorSpawn: 'spawn'.

                second: first _ActorResume.
                expect: first _ActorYieldReason ToBe: std actor yieldReason actorSpawned.

                first _ActorResume.
                expect: first _ActorYieldReason ToBe: std actor yieldReason dead.

                second _ActorResume.
                expect: second _ActorYieldReason ToBe: std actor yieldReason dead.
            ).
        |) _Genesis: 'run'.
    ).

    run = (
        testWithObject: (| inner = (| value = true |) |).
    )
|) run.
