(|
    parent* = std testing test.

    run = (
        expect: '1' asInteger ToBe: 1.
        expect: '10' asInteger ToBe: 10.

        expect: '10' asHexInteger ToBe: 16.
        expect: '1f' asHexInteger ToBe: 31.
        expect: '1F' asHexInteger ToBe: 31.

        expect: ('z' asIntegerBase: 36) ToBe: 35.
        expect: ('10' asIntegerBase: 36) ToBe: 36.
    )
|) run.
