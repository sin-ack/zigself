(|
    parent* = std testing test.

    run = (
        expect: 1 asString ToBe: '1'.
        expect: 16 asString ToBe: '16'.

        expect: 16 asHexString ToBe: '10'.
        expect: 15 asHexString ToBe: 'f'.

        expect: (35 asStringBase: 2) ToBe: '100011'.
        expect: (35 asStringBase: 36) ToBe: 'z'.
    )
|) run.
