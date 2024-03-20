"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    hashSet = (|
        mutable* = std mixins mutableCollection.
        removable* = std mixins removableCollection.
        implicitKeyInsertable* = std mixins implicitKeyInsertableCollection.
        parent* = std traits collection.

        copy = (clone; table: table copy).
        copyRemoveAll = (clone; table: table copyRemoveAll).

        "Trait & mixin requirements"
        size = (table size).
        capacity = (table capacity).
        at: key IfAbsent: absentBlock = (table at: key IfAbsent: [ ^ absentBlock value ]. self).
        each: blk = (table each: blk. self).
        at: key Put: item = (table at: key Put: item. self).
        add: item = (table add: item. self).
        remove: item IfAbsent: absentBlock = (table remove: item IfAbsent: absentBlock).
        contains: key = (table contains: key).

        asString = (| s. firstItem |
            s: '{'.
            firstItem: true.
            each: [| :item |
                firstItem ifFalse: [ s: s, ', ' ].
                s: s, item asString.
                firstItem: false.
            ].
            s, '}'
        ).

        "Performs a union operation. Creates a new set."
        + other = (| newSet |
            newSet: copyRemoveAll.
            each: [| :item | newSet add: item ].
            other each: [| :item | newSet add: item ].

            newSet
        ).

        "Performs an intersection operation. Creates a new set."
        && other = (| newSet |
            newSet: copyRemoveAll.
            each: [| :item |
                other contains: item; ifTrue: [ newSet add: item ]
            ].

            newSet
        ).

        "Performs an xor operation. Creates a new set."
        ^^ other = (| newSet |
            newSet: copyRemoveAll.
            each: [| :item |
                other contains: item; ifFalse: [ newSet add: item ]
            ].
            other each: [| :item |
                contains: item; ifFalse: [ newSet add: item ]
            ].

            newSet
        ).

        "Performs a subtraction operation. Creates a new set."
        - other = (| newSet |
            newSet: copyRemoveAll.
            each: [| :item |
                other contains: item; ifFalse: [ newSet add: item ]
            ].

            newSet
        ).
    |).
|).

std _AddSlots: (|
    hashSet = (|
        parent* = std traits hashSet.
        table <- std hashTable copyRemoveAll.
    |).
|).
