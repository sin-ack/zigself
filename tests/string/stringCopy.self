(|
    parent* = std testing test.

    original = 'abcde'.
    run = (| s |
        s: original copy.
        expect: s ToBe: 'abcde'.

        s: original copySize: 3.
        expect: s ToBe: 'abc'.

        s: original copyFrom: 1 Until: 3.
        expect: s ToBe: 'bc'.

        s: original copyFrom: 3 Until: 1.
        expect: s ToBe: 'bc'.

        s: original copyFrom: 1 Until: 1.
        expect: s ToBe: ''.

        s: original copyFrom: 0 Size: 2.
        expect: s ToBe: 'ab'.

        s: original copyFrom: 2 Size: 2.
        expect: s ToBe: 'cd'.
    )
|) run.
