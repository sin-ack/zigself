"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

traits block _AddSlots: (|
    "FIXME: This is a terrible way of implementing loops! Either replace this
            with the original Self VM's _Restart primitive, or implement TCO."
    loop = (value. 'Performed one loop iteration' printLine. loop).

    whileTrue: blk = (| s |
        [
            value ifFalse: [ ^ nil ].
            blk value
        ] loop.
        'whileTrue: Exited' printLine.
    ).

    whileFalse: blk = (
        [ value not ] whileTrue: blk
    ).
|).
