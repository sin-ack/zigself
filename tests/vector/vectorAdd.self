(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        expect: v size ToBe: 0.
        expect: v capacity ToBe: 0.

        v add: 1.
        expect: v size ToBe: 1.
        expect: v capacity ToBeGreaterThan: 1.
        expect: (v at: 0) ToBe: 1.
    ).
|) run.
