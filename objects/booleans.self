"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

(|
    parent* = self.

    addBooleanCapabilities = (| booleanParent. t. f. |
        "FIXME: Make this into traits boolean or something"
        booleanParent: (|
            ifTrue: tb           = (ifTrue: tb False: []).
            ifFalse: fb          = (ifTrue: [] False: fb).
            ifFalse: fb True: tb = (ifTrue: tb False: fb).
        |).

        t: 0 _IntLT: 1.
        f: 1 _IntLT: 0.

        globals _AddSlots: (| true = t. false = f |).

        true  _AddSlots: (| parent* = () |).
        false _AddSlots: (| parent* = () |).

        true parent _AddSlots: (|
            parent* = booleanParent.

            ifTrue: tb False: fb = (tb value).
            not = false.
            && b = (b value).
            "Short circuits its argument."
            "FIXME: allow | as operator argument"
            or: b = (self).
        |).

        false parent _AddSlots: (|
            parent* = booleanParent.

            ifTrue: tb False: fb = (fb value).
            not = true.

            "Short circuits its argument."
            && b = (self).
            "FIXME: allow | as operator argument"
            or: b = (b value).
        |).
    ).
|) addBooleanCapabilities.
