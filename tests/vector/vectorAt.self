(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        v add: 1.
        v add: 2.
        v add: 3.
        v add: 4.
        v add: 5.

        expect: (v at: 1) ToBe: 2.

        v add: 6.
        expect: (v at: 5) ToBe: 6.

        v remove: 1.
        expect: (v at: 1) ToBe: 3.
        expect: (v at: 4) ToBe: 6.
    ).
|) run.
