(|
    parent* = std testing test.

    run = (
        expectToFail: [| :fail | std file open: 'nonexistent.txt' IfFail: fail ].
    ).
|) run.
