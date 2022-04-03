(|
    parent* = std testing test.

    run = (
        expect: (1 & 2; size) ToBe: 2.
        expect: (1 & 2 & 3; size) ToBe: 3.
        expect: (1 & (2 & 3) & 4 & (5 & (6 & 7)); size) ToBe: 7.
    ).
|) run.
