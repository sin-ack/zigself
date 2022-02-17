"
Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits array _AddSlots: (|
    parent* = std traits clonable.

    at: i = (_ArrayAt: i).
    at: i Put: value = (_ArrayAt: i Put: value).
    size = (_ArraySize).

    do: block = (
        0 to: size Do: [| :i | block value: (at: i) With: i ].
    ).

    copySize: n FillingExtrasWith: value = (_ArrayCopySize: n FillingExtrasWith: value).
    copySize: n   = (copySize: n FillingExtrasWith: nil).
|).

std _AddSlots: (|
    "NOTE: As a special case, _ArrayCopySize:FillingExtrasWith: will not look
     at the receiver when the size is 0, and will just create a new array."
    array = _ArrayCopySize: 0 FillingExtrasWith: nil.
|).