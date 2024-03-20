"
Copyright (c) 2021-2024, sin-ack <sin-ack@protonmail.com>

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
    bitXor: n = (_IntXor: n).
    bitAnd: n = (_IntAnd: n).
    bitOr: n = (_IntOr: n).

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

    asStringBase: base = (|
        characterValues = '0123456789abcdefghijklmnopqrstuvwxyz'.
        output. value. negative.
    |
        (base < 0) || [base > 36] ifTrue: [_Error: 'Base outside valid range'].

        = 0 ifTrue: [ ^ '0' ].

        output: ''.
        value: self.
        negative: false.

        value < 0 ifTrue: [
            value: value negate.
            negative: true.
        ].

        [ value > 0 ] whileTrue: [| char <- ' '. digit |
            digit: value % base.
            "FIXME: Instead of doing this calculate the logarithm of the integer."
            char at: 0 Put: characterValues at: digit.
            output: output, char.
            value: value / base.
        ].

        output: output reverse.
        negative ifTrue: [ '-', output ] False: output.
    ).


    asString = (asStringBase: 10).
    asHexString = (asStringBase: 16).

    asInteger = (self).

    hash = (self).
|).
