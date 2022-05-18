(|
    parent* = std testing test.

    run = (
        (|
            parent* = std traits clonable.

            spawn = (
                _ActorSetEntrypoint: 'shouldNotReachHere'.
                copy
            ).

            shouldNotReachHere = ('should not reach here!').
        |) _ActorSpawn: 'spawn'.
    ).
|) run.
