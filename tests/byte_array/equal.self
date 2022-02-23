(|
    parent* = std testing test.

    run = (
        expect: 'foo' ToBe: 'foo'.
        expect: 'foo' ToNotBe: 'bar'.
        expect: 'baz' ToNotBe: 'quux'.
        expect: 'foo' ToNotBe: 'foob'.
    ).
|) run.
