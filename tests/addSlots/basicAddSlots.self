(|
    parent* = std testing test.

    run = (| obj |
        "Basic case: Adding nothing (empty object)"
        obj: (| foo = 1 |) _AddSlots: ().
        expect: obj foo ToBe: 1.

        "Adding a new constant slot to an object"
        obj: (| foo = 1 |) _AddSlots: (| bar = 2 |).
        expect: obj foo ToBe: 1.
        expect: obj bar ToBe: 2.

        "Updating the value of an existing slot"
        obj: (| foo = 1 |) _AddSlots: (| foo = 2 |).
        expect: obj foo ToBe: 2.

        "Adding a new assignable slot (changes the object itself)"
        obj: (| foo = 1 |) _AddSlots: (| bar <- 2 |).
        expect: obj foo ToBe: 1.
        expect: obj bar ToBe: 2.
        obj bar: 3.
        expect: obj bar ToBe: 3.

        "Replacing a constant slot with an assignable slot"
        obj: (| foo = 1 |) _AddSlots: (| foo <- 2 |).
        expect: obj foo ToBe: 2.
        obj foo: 3.
        expect: obj foo ToBe: 3.
    )
|) run.
