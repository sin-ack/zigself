(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        v add: 1.

        expect: v size ToBe: 1.
        expect: v capacity ToBeGreaterThan: 1.

        v shrinkToFit.
        expect: v capacity ToBe: 1.
    ).
|) run.
