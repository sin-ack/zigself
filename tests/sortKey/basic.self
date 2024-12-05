(|
    parent* = std testing test.

    sortKey = (|
        parent* = std traits clonable.
        "Descending sort key"
        value: lhs With: rhs = (
            lhs < rhs ifTrue: [^ 1].
            lhs > rhs ifTrue: [^ 1 negate].
            0
        ).
    |) asSortKey.

    run = (| v |
        v: std vector copy.
        v add: 1.
        v add: 2.
        v add: 3.

        v sortKey: sortKey.

        expect: (v at: 0) ToBe: 3.
        expect: (v at: 1) ToBe: 2.
        expect: (v at: 2) ToBe: 1.
    ).
|) run.
