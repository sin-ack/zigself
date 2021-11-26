"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

traits _AddSlots: (|
    "A node of a linked list."
    link = (|
        parent* = traits clonable.

        "Remove all items by disconnecting them."
        removeAll = (
            prev: self.
            next: self.
            self
        ).

        "Append the given node as the previous value."
        appendAsPrev: node = (
            node prev: prev.
            node next: self.
            prev next: node.
            prev: node.
            self
        ).

        "Iterate over all nodes except this one, calling the block in the process."
        do: block = (| head. current. |
            head: self.
            current: next.

            "Note that we need to save the next value here because the block
             might remove the current node when called."
            [ head == current ] whileFalse: [| savedNextItem |
                savedNextItem: current next.
                block value: current.
                current: savedNextItem.
            ].

            self
        ).

        "Iterate over all nodes except this one in reverse order, calling the
         block in the process."
        reverseDo: block = (| head. current. |
            head: self.
            current: prev.

            "Note that we need to save the previous value here because the block
             might remove the current node when called."
            [ head == current ] whileFalse: [| savedPrevItem |
                savedPrevItem: current prev.
                block value: current.
                current: savedPrevItem.
            ].

            self
        ).
    |).

    "A linked list object."
    list = (|
        parent* = traits clonable.

        "Remove all nodes."
        removeAll = (
            size: 0.
            nodes removeAll.
            self
        ).

        "Copy and remove all nodes. Return the copy."
        copyRemoveAll = (| c |
            c: copy.
            c nodes: nodes copy.
            c removeAll
        ).

        "Append a new item."
        append: item = (| n |
            n: nodes copy.
            n value: item.
            nodes appendAsPrev: n.

            size: size succ.
            self
        ).

        "Iterate over all the items in the list."
        do: block = (| i |
            i: 0.
            nodes do: [| :node | block value: node With: i. i: i succ ].
            self
        ).

        "Iterate over all the items in the list in reverse order."
        reverseDo: block = (| i |
            i: size prec.
            nodes reverseDo: [| :node | block value: node With: i. i: i prec ].
            self
        ).

        "Return the item at the given position."
        at: index = (| b |
            b: [| :node. :i | (i = index) ifTrue: [ ^ node value ] ].
            ((index * 2) > size) ifFalse: [ do: b ] True: [ reverseDo: b ].
            _Error: 'Could not find the item at the given index'.
        ).
    |).
|).

globals _AddSlots: (|
    link = (|
        parent* = traits link.
        value <- 'First object, no value'.
        prev.
        next.
    |).
|).

globals _AddSlots: (|
    list = (|
        parent* = traits list.
        size <- 0.
        nodes <- link.
    |).
|).

"Make the link reference itself."
"Note that this probably causes a reference cycle and isn't destructible
 at exit due to zigSelf's current reference counting-only mechanism."
link prev: link.
link next: link.
