"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std _AddSlots: (|
    "OS specific constants, system calls, etc."
    os = (|
        openFile: path WithFlags: flags IfFail: failBlock = (
            _Open: path WithFlags: flags IfFail: failBlock.
        ).

        read: count BytesInto: buffer AtOffset: offset From: fd IfFail: failBlock = (
            _Read: count BytesInto: buffer AtOffset: offset From: fd IfFail: failBlock.
        ).

        write: count BytesFrom: buffer AtOffset: offset Into: fd IfFail: failBlock = (
            _Write: count BytesFrom: buffer AtOffset: offset Into: fd IfFail: failBlock.
        ).

        pollFDs: fds Events: events WaitingForMS: ms IfFail: failBlock = (
            _PollFDs: fds Events: events WaitingForMS: ms IfFail: failBlock.
        ).

        tryWhileInterrupted: block IfFail: failBlock = (
            [| :restart |
                block value: [| :err |
                    "FIXME: This check is completely bogus and does not work. Use
                            the correct errno once we can pipe them through to
                            Self code."
                    (err = 'EINTR') ifFalse: [ ^ failBlock value: err ]
                                    True: restart.
                ].
            ] restart.
        ).
    |).
|).
