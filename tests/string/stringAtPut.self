(|
    parent* = std testing test.

    run = (| s |
        s: 'aaa'.
        expect: (s at: 1) ToBe: 97.

        s at: 1 Put: 98.
        expect: (s at: 1) ToBe: 98.
        expect: s ToBe: 'aba'.
    )
|) run.
