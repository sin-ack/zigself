(|
    parent* = std testing test.

    run = (
        [| :break |
            'a b' splitBy: ' ' Do: [| :a | expect: a ToBe: 'a'. break value].
        ] break.

        'ab' splitBy: ' ' Do: [| :a | expect: a ToBe: 'ab'].
    ).
|) run.
