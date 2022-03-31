"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

"This file implements the collection hierarchy."

std mixins _AddSlots: (|
    "Adds behavior for a collection that can be modified.
     Requires the object to inherit from std traits collection."
    mutableCollection = (|
        "The slots required on inheritors of mutableCollection."
        interface = (|
            "Put the item at the given key."
            at: key Put: item = (childMustImplement).
        |).
    |).

    "Adds behavior for a collection which is indexable (has a first and last
     key, can be iterated on with a monotonically increasing key).
     Covers the at:IfAbsent: and each: requirements of std traits collection.
     Requires the object to inherit from std traits collection."
    indexableCollection = (|
        "The slots required on inheritors of indexableCollection."
        interface = (|
            "Return the item at the given index."
            at: index = (childMustImplement).
        |).

        firstKey = 0.
        lastKey = (size prec).

        at: index IfAbsent: blk = (
            (index >= firstKey) && [index <= lastKey] ifFalse: blk.
            at: index
        ).
        each: blk = (
            firstKey through: lastKey Do: [| :i | blk value: (at: i) With: i ].
        ).
    |).

    "Adds behavior for a collection that can have slots removed from it.
     Requires the object to inherit from std traits collection."
    removableCollection = (|
        "The slots required on inheritors of removableCollection."
        interface = (|
            "Remove the element addressed by key from the collection.
             Call the given block with no arguments if no such element exists."
            remove: key IfAbsent: blk = (childMustImplement).
        |).

        remove: key = (remove: key IfAbsent: [ _Error: 'key does not exist in collection' ]).
    |).

    "Adds behavior for a collection that can have an item inserted into it
     without a key.
     Requires the object to inherit from std traits collection."
    implicitKeyInsertableCollection = (|
        "The slots required on inheritors of implicitKeyInsertableCollection."
        interface = (|
            "Insert the given item into the collection with an appropriate
             implicit key. For unindexable collections, this usually means
             key = value, and for indexable collections the element is usually
             appended to the end."
            add: item = (childMustImplement).
        |).
    |).
|).

std traits _AddSlots: (|
    collection = (|
        parent* = std traits clonable.

        "The slots required on inheritors of collection."
        interface = (|
            "Return the item stored at the given key."
            at: key IfAbsent: blk = (childMustImplement).
            "For each item stored in the collection, call the given block with
             the item and its key."
            each: blk = (childMustImplement).
            "Return the size of the collection."
            size = (childMustImplement).
        |).
    |).
|).
