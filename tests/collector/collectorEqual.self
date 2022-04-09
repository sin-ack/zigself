(|
    parent* = std testing test.

    run = (
        expect: (1 & 2) ToBe: (1 & 2).
        expect: (1 & 2) ToNotBe: (1 & 2 & 3).
        expect: (1 & 2 & 3) ToNotBe: (1 & 2).
    ).
|) run.
