(|
    parent* = std testing test.
    expected = 'Well hello friends! :^)\n'.

    run = (| file. actual |
        expectToNotFail: [| :fail |
            file: std file open: 'tests/fixtures/hello_world.txt' IfFail: fail.
            actual: file readAllIfFail: fail.
        ].

        expect: actual ToBe: expected.
        file close.
    ).
|) run.
