(|
    parent* = std testing test.

    actorA = (|
        parent* = std traits actor.

        actorB = (|
            parent* = std traits actor.

            counter <- 0.

            incrementCounter = (
                counter: counter succ.
                sender incrementCounter.
            ).

            main = (
                [counter < 10] whileTrue: [yield].
            ).
        |).

        counter <- 0.

        incrementCounter = (
            counter: counter succ.
            sender incrementCounter.
        ).

        main = (
           actorB spawn incrementCounter.
           [counter < 10] whileTrue: [yield].
        ).
    |).

    run = (
        std scheduler startWith: actorA.
    ).
|) run.
