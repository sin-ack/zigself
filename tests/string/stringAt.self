(|
    parent* = std testing test.

    run = (| s |
        s: ''.
        expect: s size ToBe: 0.

        s: s, 'abc'.
        expect: s size ToBe: 3.

        expect: (s at: 0) ToBe: 97.
        expect: (s at: 1) ToBe: 98.
        expect: (s at: 2) ToBe: 99.
    )
|) run.
