(|
    parent* = std traits singleton.

    run = (| actor |
        actor: (|
            spawn = (
                _Error: 'Some random error'.
            ).
        |) _ActorSpawn: 'spawn'.

        "We use an expected-error test to hide the stack trace from being
         printed during test runs, so we need to throw an error here on success."
        actor _ActorYieldReason = std actor yieldReason runtimeError
        ifTrue: [ _Error: 'task failed successfully' ].
    ).
|) _Genesis: 'run'.
