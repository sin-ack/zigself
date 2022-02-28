"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits integer _AddSlots: (|
    parent* = std traits clonable.

    succ = (self + 1).
    prec = (self - 1).

    negate = (0 - self).

    + n  = (_IntAdd: n).
    - n  = (_IntSub: n).
    * n  = (_IntMul: n).
    / n  = (_IntDiv: n).
    % n  = (_IntMod: n).
    < n  = (_IntLT: n).
    > n  = (_IntGT: n).
    = n  = (_IntEq: n).
    >= n = ((self > n) or: [ self = n ]).
    <= n = ((self < n) or: [ self = n ]).
    != n = ((self = n) not).

    max: n = ((self < n) ifTrue: [ n ] False: [ self ]).

    kib = (self * 1024).
    mib = (kib * 1024).
    gib = (mib * 1024).

    to: upper Do: block = (| i |
        (self < upper) ifFalse: [ ^ nil ].

        i: self.
        [ i < upper ] whileTrue: [
            block value: i.
            i: i succ.
        ].
    ).

    "FIXME: This is a naive implementation. Allocate the byte array in one go
            by counting the digits."
    asString = (| output. value |
        output: ''.
        value: self.

        (value < 0) ifTrue: [
            value: value negate.
            output: '-'.
        ].

        [ value > 0 ] whileTrue: [| char <- ' '. digit |
            digit: value % 10.
            char at: 0 PutByte: 48 + digit.
            output: output, char.
            value: value / 10.
        ].

        output reverse
    ).
|).
