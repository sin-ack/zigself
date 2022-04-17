"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    hashMap = (|
        mutable* = std mixins mutableCollection.
        removable* = std mixins removableCollection.
        parent* = std traits collection.

        copy = (| c |
            c: (clone; table: table copy).
            c table wrapperUtility replaceEachValueWithCopy.
            c
        ).
        copyRemoveAll = (clone; table: table copyRemoveAll).

        capacity = (table capacity).

        "Trait & mixin requirements"
        size = (table size).
        at: key IfAbsent: absentBlock = (private atKey: key IfAbsent: absentBlock).
        each: blk = (table each: [| :entry | blk value: entry value With: entry key ]).
        at: key Put: item = (private atKey: key Put: item. self).
        remove: key IfAbsent: absentBlock = (private removeKey: key IfAbsent: absentBlock).
        contains: key = (private containsKey: key).

        private = (|
            prototype = (|
                receiver* <- nil.

                entry = (|
                    parent* = (|
                        parent* = std traits clonable.

                        copyKey: key = (copyKey: key Value: nil).
                        copyKey: key Value: value = (clone; key: key; value: value).

                        hash = (key hash).
                        = other = (key = other key).
                    |).

                    key. value.
                |).

                atKey: key IfAbsent: absentBlock = (| keyAsEntry |
                    keyAsEntry: entry copyKey: key.
                    table at: keyAsEntry IfAbsent: [ ^ absentBlock value ]
                    ; value
                ).

                atKey: key Put: item = (| entryToInsert |
                    entryToInsert: entry copyKey: key Value: item.
                    table add: entryToInsert.
                ).

                removeKey: key IfAbsent: absentBlock = (| keyAsEntry |
                    keyAsEntry: entry copyKey: key.
                    table remove: keyAsEntry IfAbsent: [ ^ absentBlock value ]
                    ; value
                ).

                containsKey: key = (table contains: entry copyKey: key).
            |).
        | prototype clone; receiver: self).
    |).
|).

std _AddSlots: (|
    hashMap = (|
        parent* = std traits hashMap.
        table <- std hashTable copyRemoveAll.
    |).
|).
