(|
    parent* = std testing test.

    run = (| result |
        result: (|
            main = (123).
        |) _Genesis: 'main'.

        expect: result ToBe: 123.
    ).
|) run.
