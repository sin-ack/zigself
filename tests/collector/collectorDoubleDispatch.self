(|
    parent* = std testing test.

    run = (| c |
        "Basic case"
        c: 1 & 2.
        expect: c value ToBe: 2.
        expect: c previous value ToBe: 1.

        "Nested collectors should be flattened via the use of double dispatch;
         meaning that the right hand side should be the one doing the appending
         (since it's the one with the knowledge of how to behave when being added
         to a collector)."
        c: 1 & (2 & 3).
        expect: c value ToBe: 3.
        expect: c previous value ToBe: 2.
        expect: c previous previous value ToBe: 1.

        c: (1 & 2) & 3.
        expect: c value ToBe: 3.
        expect: c previous value ToBe: 2.
        expect: c previous previous value ToBe: 1.

        "Complex case just to be sure."
        c: (1 & (2 & 3) & 4 & (5 & (6 & 7) & 8) & 9; asVector).
        c each: [| :value. :index | expect: value ToBe: index succ ].
    ).
|) run.
