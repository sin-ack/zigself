"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

"A collector object is meant to be the 'missing syntax sugar' for creating
collections from literal values. You can use it like so:

    (1 & 2 & 3; asVector)

which will give you a std vector with 1, 2 and 3 as its elements in that order.

If your favorite collector type is not natively supported by collector, don't
fret! You can simply use the handy 'collectInto:' message:

    (1 & 2 & 3; collectInto: yourCoolCollection copyRemoveAll)

The asFoo messages are implemented this way."

std traits _AddSlots: (|
    collector = (|
        "NOTE: collector is not a collection itself! It simply helps you create
               collections."
        parent* = std traits clonable.

        copy = (clone; previous: previous copy).
        copyFirst: value = (clone; previous: nil; value: value).
        add: value = (clone; previous: self; value: value).

        = other = (
            "FIXME: Return false if other doesn't have value or previous messages."
            value = other value ifFalse: [ ^ false ].
            previous = other previous
        ).

        & value = (value appendToCollector: self).
        appendToCollector: c = (
            nil == previous ifFalse: [ ^ previous appendToCollector: c; add: value ].
            c add: value.
        ).

        collectInto: collection = (
            nil == previous ifFalse: [ previous collectInto: collection ].
            collection add: value.
        ).

        "Common conversions"
        asVector = (collectInto: std vector copy).

        size = (| current |
            nil == cachedSize ifFalse: [ ^ cachedSize ].

            cachedSize: 1.
            current: previous.
            [nil == current] whileFalse: [
                cachedSize: cachedSize succ.
                current: current previous.
            ].
            cachedSize
        ).
    |).
|).

std _AddSlots: (|
    collector = (|
        parent* = std traits collector.
        previous. value. cachedSize.
    |).
|).

defaultBehavior _AddSlots: (|
    & other = (other appendToCollector: std collector copyFirst: self).
    appendToCollector: c = (c add: self).
|).
