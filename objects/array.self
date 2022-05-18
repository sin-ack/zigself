"
Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std _AddSlots: (|
    "NOTE: As a special case, _ArrayCopySize:FillingExtrasWith: will not look
     at the receiver when the size is 0, and will just create a new array."
    array = _ArrayCopySize: 0 FillingExtrasWith: nil.
|).

std traits _AddSlots: (| array = std array parent |).

std traits array _AddSlots: (|
    mutable* = std mixins mutableCollection.
    indexable* = std mixins indexableCollection.
    parent* = std traits collection.

    at: i = (_ArrayAt: i).
    at: i Put: value = (_ArrayAt: i Put: value).
    size = (_ArraySize).

    copySize: n FillingExtrasWith: value = (_ArrayCopySize: n FillingExtrasWith: value).
    copySize: n   = (copySize: n FillingExtrasWith: nil).
    copyRemoveAll = (std array).
|).
