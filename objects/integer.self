"
Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits integer _AddSlots: (|
    parent* = std traits clonable.

    succ = (+1).
    prec = (-1).

    negate = (0 - self).

    + n  = (_IntAdd: n).
    - n  = (_IntSub: n).
    * n  = (_IntMul: n).
    / n  = (_IntDiv: n).
    % n  = (_IntMod: n).
    < n  = (_IntLT: n).
    > n  = (_IntGT: n).
    = n  = (_IntEq: n).
    >= n = ((> n) || [ self = n ]).
    <= n = ((< n) || [ self = n ]).
    != n = ((= n) not).
    << n = (_IntShl: n).
    >> n = (_IntShr: n).

    ** n = (| i |
        n = 0 ifTrue: [ ^ 1 ].
        n < 0 ifTrue: [ _Error: 'TODO: Implement negative power' ].

        i: self.
        0 to: n prec Do: [ i: i * self ].

        i
    ).

    bitLength = (| absolute. power. output |
        absolute: abs.
        power: 1.
        output: 0.

        [power <= self] whileTrue: [
            output: output succ.
            power: power * 2.
        ].

        output
    ).

    abs = (< 0 ifTrue: negate False: self).

    max: n = (< n ifTrue: [ n ] False: [ self ]).

    kib = (self * 1024).
    mib = (kib * 1024).
    gib = (mib * 1024).

    to: upper Do: block = (through: upper prec Do: block).
    through: upper Do: block = (| i |
        <= upper ifFalse: [ ^ nil ].

        i: self.
        [ i <= upper ] whileTrue: [
            block value: i.
            i: i succ.
        ].
    ).

    "FIXME: This is a naive implementation. Allocate the byte array in one go
            by counting the digits."
    asString = (| output. value. negative |
        = 0 ifTrue: [ ^ '0' ].

        output: ''.
        value: self.
        negative: false.

        value < 0 ifTrue: [
            value: value negate.
            negative: true.
        ].

        [ value > 0 ] whileTrue: [| char <- ' '. digit |
            digit: value % 10.
            char at: 0 Put: 48 + digit.
            output: output, char.
            value: value / 10.
        ].

        output: output reverse.
        negative ifTrue: [ '-', output ] False: output.
    ).
|).
