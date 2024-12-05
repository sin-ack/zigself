"
Copyright (c) 2024, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    sortKeyFactory = (|
        parent* = std traits clonable.
        copyComparator: comparator = (copy; comparator: comparator).

        "Create a new sort key from this factory."
        value: value = (std sortKey copyComparator: comparator Value: value).
    |).

    sortKey = (|
        parent* = std traits clonable.
        copyComparator: comparator Value: value = (copy; comparator: comparator; value: value).

        "Binary messages for comparison."
        < other = ((comparator value: value With: other value) < 0).
        > other = ((comparator value: value With: other value) > 0).
        = other = ((comparator value: value With: other value) = 0).
        <= other = ((comparator value: value With: other value) <= 0).
        >= other = ((comparator value: value With: other value) >= 0).
    |).
|).

std _AddSlots: (|
    sortKeyFactory = (|
        parent* = std traits sortKeyFactory.
        "Default comparator for sorting."
        comparator <- (|
            value: value With: rhs = (
                value < rhs ifTrue: [^ 1 negate].
                value > rhs ifTrue: [^ 1].
                0.
            ).
        |).
    |).

    sortKey = (|
        parent* = std traits sortKey.
        comparator <- nil.
        value <- nil.
    |)
|).

defaultBehavior _AddSlots: (|
    "Convert an object that accepts the value:With: message and returns either
     -1, 0, or 1 based on the two arguments into a key that can be used for
     sorting."
    asSortKey = (
        std sortKeyFactory copyComparator: self.
    ).
|).
