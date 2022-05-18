(|
    parent* = std testing test.

    run = (| actor |
        actor: (|
            parent* = std testing test.

            spawn = (
                _ActorSetEntrypoint: 'main'.
                copy
            ).

            main = (
                expect: 1 + 1 ToBe: 2.
            ).
        |) _ActorSpawn: 'spawn'.

        actor _ActorResume.
        expect: actor _ActorYieldReason ToBe: std actor yieldReason dead.
    ).
|) _Genesis: 'run'.
