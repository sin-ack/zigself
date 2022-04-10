(|
    parent* = std testing test.

    run = (| strings |
        strings: 'a b c d' splitBy: ' '.
        expect: (strings at: 0) ToBe: 'a'.
        expect: (strings at: 1) ToBe: 'b'.
        expect: (strings at: 2) ToBe: 'c'.
        expect: (strings at: 3) ToBe: 'd'.

        strings: ' b c d' splitBy: ' '.
        expect: (strings at: 0) ToBe: ''.
        expect: (strings at: 1) ToBe: 'b'.
        expect: (strings at: 2) ToBe: 'c'.
        expect: (strings at: 3) ToBe: 'd'.

        strings: 'a b c ' splitBy: ' '.
        expect: (strings at: 0) ToBe: 'a'.
        expect: (strings at: 1) ToBe: 'b'.
        expect: (strings at: 2) ToBe: 'c'.
        expect: (strings at: 3) ToBe: ''.

        strings: ' a b c ' splitBy: ' a b'.
        expect: (strings at: 0) ToBe: ''.
        expect: (strings at: 1) ToBe: ' c '.

        "Splitting a string into a list"
        strings: 'abcd' splitBy: ''.
        expect: (strings at: 0) ToBe: 'a'.
        expect: (strings at: 1) ToBe: 'b'.
        expect: (strings at: 2) ToBe: 'c'.
        expect: (strings at: 3) ToBe: 'd'.

        expect: ('-' join: ('a b c d e f' splitBy: ' ')) ToBe: 'a-b-c-d-e-f'.
    )
|) run.
