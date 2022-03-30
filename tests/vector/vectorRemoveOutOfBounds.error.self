(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        v add: 1.

        "Shouldn't fail"
        v remove: 0.
        "Should cause an error"
        v remove: 0.
    ).
|) run.
