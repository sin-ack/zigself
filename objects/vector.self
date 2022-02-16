"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits vector _AddSlots: (|
    parent* = std traits clonable.

    at: i = (_VectorAt: i).
    at: i Put: value = (_VectorAt: i Put: value).
    size = (_VectorSize).

    do: block = (
        0 to: size Do: [| :i | block value: (at: i) With: i ].
    ).

    copySize: n FillingExtrasWith: value = (_VectorCopySize: n FillingExtrasWith: value).
    copySize: n   = (copySize: n FillingExtrasWith: nil).
|).

std _AddSlots: (|
    "NOTE: As a special case, _VectorCopySize:FillingExtrasWith: will not look
     at the receiver when the size is 0, and will just create a new vector."
    vector = _VectorCopySize: 0 FillingExtrasWith: nil.
|).
