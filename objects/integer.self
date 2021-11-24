"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

traits integer _AddSlots: (|
    succ = (self + 1).
    prec = (self - 1).

    + n  = (_IntAdd: n).
    - n  = (_IntSub: n).
    < n  = (_IntLT: n).
    = n  = (_IntEq: n).
    != n = ((self = n) not).

    to: upper Do: block = (| i |
        (self < upper) ifFalse: [ ^ nil ].

        i: self.
        [ i < upper ] whileTrue: [
            block value: i.
            i: i succ.
        ].
    ).
|).
