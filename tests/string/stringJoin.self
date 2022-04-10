(|
    parent* = std testing test.

    run = (| strings |
        strings: ('ab' & 'cd' & 'ef'; asVector).
        expect: ('' join: strings) ToBe: 'abcdef'.
        expect: (' ' join: strings) ToBe: 'ab cd ef'.
    )
|) run.
