(|
    parent* = std testing test.

    run = (| v |
        v: std vector copy.
        expect: v size ToBe: 0.

        v add: 1.
        expect: v size ToBe: 1.

        v: v copy.
        expect: v size ToBe: 1.

        v: v copyRemoveAll.
        expect: v size ToBe: 0.
    ).
|) run.
