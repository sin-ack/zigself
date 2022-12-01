"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    vector = (|
        mutable* = std mixins mutableCollection.
        indexable* = std mixins indexableCollection.
        removable* = std mixins removableCollection.
        implicitKeyInsertable* = std mixins implicitKeyInsertableCollection.
        parent* = std traits collection.

        copy = (
            clone
            ; items: items copy
        ).
        copyRemoveAll = (
            clone
            ; items: std array copy
            ; size: 0.
        ).

        "Trait & mixin requirements"
        at: key = (
            (private isWithinBounds: key) ifFalse: [ _Error: 'key out of bounds' ].
            items at: key
        ).
        at: key Put: item = (
            [| p = private |
                (p isWithinBounds: key) ifFalse: [ _Error: 'key out of bounds' ].
                p at: key PutWithoutBoundsCheck: item.
            ] value.
            self
        ).
        remove: index IfAbsent: blk = (
            [| p = private |
                (p isWithinBounds: index) ifFalse: blk.
                p removeIndex: index.
            ] value.
            self
        ).
        add: item = (
            [| p = private |
                p ensureSpaceForOneMore.
                p at: size PutWithoutBoundsCheck: item.
                size: size succ.
            ] value.
            self
        ).

        shift = (| value |
            value: at: 0.
            remove: 0.
            value
        ).

        capacity = (items size).

        shrinkToFit = (private resizeTo: size. self).

        asString = (| s |
            s: '['.
            each: [| :v. :i |
                i > 0 ifTrue: [ s: s, ', ' ].
                s: s, v asString.
            ].
            s, ']'.
        ).

        sort = (sortKey: [| :v | v]).
        "Naive quicksort implementation."
        sortKey: keyBlock = (| pivot |
            size < 2 ifTrue: [ ^ self ].
            private quickSortFrom: 0 Until: size KeyBlock: keyBlock.
            self
        ).

        reverse = (| p |
            p: private.
            firstKey to: (lastKey - firstKey) / 2 Do: [| :k |
                p swap: k With: lastKey - k.
            ].
            self
        ).

        private = (|
            prototype = (|
                receiver* <- nil.

                resizeTo: c = (
                    items: items copySize: c.
                ).
                grow = (resizeTo: (2 ** capacity succ bitLength; max: 4)).
                ensureSpaceForOneMore = (capacity = size ifTrue: [ grow ]).

                at: index PutWithoutBoundsCheck: value = (
                    items at: index Put: value.
                ).

                isWithinBounds: index = ((index >= firstKey) && [index <= lastKey]).

                removeIndex: index = (
                    index to: size prec Do: [| :i |
                        at: i PutWithoutBoundsCheck: at: i + 1.
                    ].
                    size: size prec.
                    at: size PutWithoutBoundsCheck: nil.
                ).

                quickSortFrom: start Until: end KeyBlock: keyBlock = (| pivot. pivotKey. pivotIndex. |
                    (start >= end) || [start < 0] ifTrue: [ ^ nil ].

                    pivot: at: end prec.
                    pivotKey: keyBlock value: pivot.

                    pivotIndex: start prec.

                    start to: end prec Do: [| :i |
                        (keyBlock value: at: i) <= pivotKey ifTrue: [
                            pivotIndex: pivotIndex succ.
                            swap: pivotIndex With: i.
                        ]
                    ].

                    pivotIndex: pivotIndex succ.
                    swap: pivotIndex With: end prec.

                    quickSortFrom: start Until: pivotIndex KeyBlock: keyBlock.
                    quickSortFrom: pivotIndex succ Until: end KeyBlock: keyBlock.
                ).

                swap: a With: b = (| temp |
                    temp: at: a.
                    at: a Put: at: b.
                    at: b Put: temp.
                ).
            |).
        | prototype clone; receiver: self).
    |).
|).

std _AddSlots: (|
    vector = (|
        parent* = std traits vector.
        items <- std array.
        size <- 0.
    |).
|)
