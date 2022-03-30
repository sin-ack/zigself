(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        v add: 1.

        "Should cause an error"
        v at: 1 Put: 123.
    ).
|) run.
