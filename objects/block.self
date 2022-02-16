"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits block _AddSlots: (|
    parent* = std traits clonable.

    "Call the block with no arguments indefinitely (or until the block non-local
     returns)."
    loop = (value. _Restart).

    "While this block evaluates to true, execute blk with no arguments."
    whileTrue: blk = (| s |
        [
            value ifFalse: [ ^ nil ].
            blk value
        ] loop.
    ).

    "While this block evaluates to false, execute blk with no arguments."
    whileFalse: blk = (
        [ value not ] whileTrue: blk
    ).
|).
