"FIXME: Convert these tests to use std file objects when those are implemented."
(|
    parent* = std testing test.

    run = (
        expectToFail: [| :fail | _Open: 'nonexistent.txt' WithFlags: 0 IfFail: fail ].
    ).
|) run.
