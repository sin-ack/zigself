"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits block _AddSlots: (|
    parent* = std traits clonable.

    "Call the block with no arguments indefinitely (or until the block non-local
     returns)."
    loop = (value. _Restart).

    "Call the block with a block argument which exits from this method when
     called, ending the loop."
    loopBreak = (| break |
        break: [ ^ nil ].
        [ value: break ] loop.
    ).

    "While this block evaluates to true, execute blk with a block argument to
     break from the block."
    whileTrue: blk = (| s |
        [| :break |
            value ifFalse: break.
            blk value: break.
        ] loopBreak.
    ).

    "While this block evaluates to false, execute blk with a block argument to
     break from the block."
    whileFalse: blk = (
        [ value not ] whileTrue: blk
    ).
|).
