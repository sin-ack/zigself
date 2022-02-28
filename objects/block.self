"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits block _AddSlots: (|
    parent* = std traits clonable.

    "Call the block with no arguments indefinitely (or until the block non-local
     returns)."
    loop = (value. _Restart).

    "Call the block with a block arguments which exits from this method when
     called, halting the block's execution."
    break = (value: [ ^ nil ]).

    "Call the block repeatedly with a block argument which exits from this
     method when called, ending the loop."
    loopBreak = (| break |
        break: [ ^ nil ].
        [ value: break ] loop.
    ).

    "Call the block repeatedly with a block argument which restarts the block.
     This is useful when the loop should start from scratch without executing
     the statements after the statement which invokes continue."
    loopContinue = (
        [
            [| :continue |
                value: continue.
            ] break.
        ] loop.
    ).

    "Call the block with a block argument which restarts the block."
    restart = (
        [| :outerBreak |
            [| :innerBreak |
               value: innerBreak.
               outerBreak value.
            ] break.
        ] loopBreak.
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
