"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    hashTable = (|
        mutable* = std mixins mutableCollection.
        removable* = std mixins removableCollection.
        implicitKeyInsertable* = std mixins implicitKeyInsertableCollection.
        parent* = std traits collection.

        copy = (| c |
            c: clone
            ; slots: slots copy.

            c slots each: [| :slot. :i | c slots at: i Put: slot copy ].
            c
        ).
        copyRemoveAll = (
            clone
            ; slots: std array copyRemoveAll
            ; size: 0
        ).

        capacity = (slots size).

        "Trait & mixin requirements"
        at: key IfAbsent: absentBlock = (| slot |
            isEmpty ifTrue: [ ^ absentBlock value ].

            slot: private slotForItem: key.
            slot used ifFalse: [ ^ absentBlock value ].
            slot value
        ).

        each: blk = (
            slots each: [| :slot |
                slot used ifTrue: [ blk value: slot value With: slot value ].
            ].
        ).

        at: key Put: item = (
            key == item ifFalse: [ _Error: 'hashTable requires its keys to be equal to its items' ].
            add: item.
        ).

        add: item = (
            [| p = private |
                p growIfNecessary.
                p insertWithoutGrowing: item.
            ] value.
            self
        ).

        remove: item IfAbsent: absentBlock = (| value |
            [| p = private. slot |
                slot: p slotForItem: item.
                slot used ifFalse: [ ^ absentBlock value ].

                value: slot value.
                slot delete.

                deletedSlots: deletedSlots succ.
                size: size prec.

                p shrinkIfNecessary.
            ] value.
            value
        ).

        contains: key = (| slot |
            isEmpty ifTrue: [ ^ false ].
            slot: private slotForItem: key.
            slot used
        ).

        private = (|
            prototype = (|
                receiver* <- nil.

                slot = (|
                    parent* = (|
                        parent* = std traits clonable.

                        set: item = (
                            used: true.
                            deleted: false.
                            value: item.
                        ).

                        delete = (
                            used: false.
                            deleted: true.
                            value: nil.
                        ).
                    |).

                    "Whether this slot is used."
                    used <- false.
                    "Whether this slot is deleted. Note that this is different
                     from used, because an item might be after this one in the
                     probe chain."
                    deleted <- false.
                    value.
                |).

                growThresholdPercent = 60.
                shrinkThresholdPercent = 20.

                shouldGrow = (
                    ((size + deletedSlots) * 100) >= (capacity * growThresholdPercent).
                ).

                growIfNecessary = (
                    shouldGrow ifTrue: [ rehashTo: (capacity * 2) max: 16 ].
                ).

                "Performs a rehash operation."
                rehashTo: newCapacity = (| oldSlots |
                    oldSlots: slots.
                    slots: std array copySize: newCapacity.
                    size: 0.
                    deletedSlots: 0.

                    0 to: newCapacity Do: [| :i |
                        slots at: i Put: slot copy.
                    ].

                    oldSlots each: [| :slot |
                        slot used ifTrue: [ insertWithoutGrowing: slot value ].
                    ].
                ).

                shouldShrink = (
                    (size > 16) && [(size * 100) < (capacity * shrinkThresholdPercent)].
                ).

                shrinkIfNecessary = (
                    shouldShrink ifTrue: [ rehashTo: size * 2 ].
                ).

                insertWithoutGrowing: item = (| slot |
                    slot: slotForItem: item.

                    slot deleted ifTrue: [ deletedSlots: deletedSlots prec ].
                    slot used ifFalse: [ size: size succ ].

                    slot set: item.
                ).

                slotForItem: item = (| hash. foundSlot. firstEmptySlot |
                    hash: item hash.
                    [| slot |
                        "The algorithm is as follows:
                         1) If a used slot is found with an equal value, then
                            return it.
                         2) Otherwise, store the first unused slot, and if the
                            loop hits an unused and undeleted slot (meaning that
                            the probe chain terminates), return that first
                            unused slot."
                        slot: slots at: hash % capacity.
                        slot used && [slot value = item] ifTrue: [ ^ slot ].

                        slot used ifFalse: [
                            firstEmptySlot == nil ifTrue: [ firstEmptySlot: slot ].
                            slot deleted ifFalse: [ ^ firstEmptySlot ].
                        ].

                        hash: doubleHash: hash.
                    ] loop.
                ).

                doubleHash: hash = (| magic = 0xBA5EDB01. result |
                    "A port of SerenityOS' double hash:"
                    "https://github.com/SerenityOS/serenity/blob/daaa8a57f0d7e638c434b08b22a5f2bc107480a6/AK/HashFunctions.h#L22"
                    hash = magic ifTrue: [ ^ 0 ].

                    result: hash.
                    result = 0 ifTrue: [ result: magic ].

                    result: result ^^ (result << 13).
                    result: result ^^ (result >> 17).
                    result: result ^^ (result << 5).

                    result && 0xFFFFFFFF
                ).
            |).
        | prototype clone; receiver: self).
    |).
|).

std _AddSlots: (|
    hashTable = (|
        parent* = std traits hashTable.
        slots <- std array copyRemoveAll.
        size <- 0.
        deletedSlots <- 0.
    |).
|).
