(|
    parent* = std testing test.

    run = (| s1. s2 |
        s1: 'a'.
        s2: 'b'.
        expect: s1 ToNotBe: s2.

        s1: 'b'.
        expect: s1 ToBe: s2.
    )
|) run.
