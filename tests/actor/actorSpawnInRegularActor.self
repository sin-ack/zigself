(|
    parent* = std testing test.

    run = (| firstActor. secondActor |
        firstActor: (|
            parent* = std traits clonable.

            other = (|
                parent* = std traits clonable.

                spawn = (
                    _ActorSetEntrypoint: 'main'.
                    copy
                ).

                main = ('main').
            |).

            spawn = (
                _ActorSetEntrypoint: 'main'.
                copy
            ).

            main = (| otherProxy |
                otherProxy: other _ActorSpawn: 'spawn'.
            ).
        |) _ActorSpawn: 'spawn'.

        secondActor: firstActor _ActorResume.
        expect: firstActor _ActorYieldReason ToBe: std actor yieldReason actorSpawned.

        firstActor _ActorResume.
        expect: firstActor _ActorYieldReason ToBe: std actor yieldReason dead.

        secondActor _ActorResume.
        expect: secondActor _ActorYieldReason ToBe: std actor yieldReason dead.
    ).
|) _Genesis: 'run'.
