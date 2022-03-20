(|
    parent* = std testing test.

    firstBase = (| foo = 1 |).
    secondBase = (| foo = 2 |).

    run = (| obj |
        "Overriding slots from the first base with the second base"
        obj: (| base< = firstBase |).
        expect: obj foo ToBe: 1.

        obj _AddSlots: (| base2< = secondBase |).
        expect: obj foo ToBe: 2.

        "Overriding slots from bases with new slots"
        obj _AddSlots: (| foo = 3 |).
        expect: obj foo ToBe: 3.
    ).
|) run.
