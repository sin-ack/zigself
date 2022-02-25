"FIXME: Convert these tests to use std file objects when those are implemented."
(|
    parent* = std testing test.
    expected = 'Well hello friends! :^)\n'.

    run = (| fd. nread. buffer. actual. |
        expectToNotFail: [| :fail | fd: _Open: 'tests/fixtures/hello_world.txt' WithFlags: 0 IfFail: fail ].
        "FIXME: Implement creating an empty string with arbitrary size"
        buffer: '                                '.
        expectToNotFail: [| :fail | nread: _Read: buffer size BytesInto: buffer AtOffset: 0 From: fd IfFail: fail ].
        actual: buffer copySize: nread.

        expect: actual ToBe: expected.
    ).
|) run.
