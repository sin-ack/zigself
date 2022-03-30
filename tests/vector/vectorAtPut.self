(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        v add: 1.
        v add: 2.
        v add: 3.

        expect: (v at: 1) ToBe: 2.

        v at: 1 Put: 5.
        expect: (v at: 1) ToBe: 5.
    ).
|) run.
