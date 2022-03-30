(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        v add: 1.
        v add: 2.
        v add: 3.
        v add: 4.

        expect: (v at: 1) ToBe: 2.
        expect: v size ToBe: 4.

        v remove: 1.
        expect: (v at: 0) ToBe: 1.
        expect: (v at: 1) ToBe: 3.
        expect: v size ToBe: 3.
    ).
|) run.
