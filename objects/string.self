"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits string _AddSlots: (|
    parent* = std traits clonable.

    splitOn: substring = (| indicesList. substrings. head. index |
        indicesList: list copyRemoveAll.
        index: 0.

        [ index < size ] whileTrue: [
            findSubstring: substring
                FromIndex: index
                IfPresent: [| :substringIndex |
                    indicesList append: substringIndex.
                    index: substringIndex succ.
                ]
                IfAbsent: [ index: size ].
        ].

        indicesList append: size.
        substrings: std array copySize: indicesList size prec.

        "The first substring must be appended outside because we do not want to add the substring length."
        substrings at: 0 Put: (copyFrom: 0 Until: (indicesList at: 0)).

        "Find and append all the substrings."
        index: 0.
        head: indicesList nodes.
        indicesList nodes do: [| :node |
            (node next == head) ifTrue: [ ^ substrings ].
            substrings at: index Put: (copyFrom: (node value + substring size) Until: (node next value)).
            index: index succ.
        ].

        substrings
    ).

    print = (_StringPrint).
    printLine = ( print. '\n' print ).

    findSubstring: substring
        FromIndex: index
        IfPresent: presentBlock
         IfAbsent: absentBlock = (
        index to: size - substring size Do: [| :i |
            substring findFirst: [| :c. :j | c != (at: i + j) ]
                      IfPresent: []
                       IfAbsent: [ ^ presentBlock value: i ].
        ].
        absentBlock value.
    ).

    size = (_ByteArraySize).
    isEmpty = (size = 0).
    at: index = ( _ByteAt: index ).
    at: index PutByte: value = ( _ByteAt: index Put: value ).
    do: block = (
        0 to: size Do: [| :i | block value: (at: i) With: i ].
    ).

    findFirst: block IfPresent: presentBlock IfAbsent: absentBlock = (
        do: [| :item. :i | (block value: item With: i) ifTrue: [ ^ presentBlock value ] ].
        absentBlock value.
    ).

    copyFrom: start Until: end = (
        (start = end) ifTrue: [ ^ '' ].
        (start < end) ifFalse: [ ^ copyFrom: end Until: start ].

        copyFrom: start Size: (end - start).
    ).

    copyFrom: start Size: size = (| bytesToCopy. targetBuffer |
        bytesToCopy: size max: (self size - start).
        targetBuffer: copySize: size.

        "If start is 0 then we're already done, no need to copy."
        (start = 0) ifTrue: [ ^ targetBuffer ].

        0 to: size Do: [| :i |
            targetBuffer at: i PutByte: at: start + i.
        ].

        targetBuffer
    ).

    copySize: size = ( _ByteArrayCopySize: size ).
|).

std traits string _AddSlots: (|
    toInteger = (| value. zeroByte = '0' at: 0. nineByte = '9' at: 0 |
        isEmpty ifTrue: [ _Error: 'empty string cannot be converted to integer' ].

        value: 0.
        do: [| :byte |
            ((byte >= zeroByte) && [byte <= nineByte]) ifTrue: [
                value: (value * 10) + (byte - zeroByte).
            ] False: [
                _Error: 'string with non-digit characters cannot be converted to integer'
            ].
        ].

        value
    ).
|).

std _AddSlots: (|
    string = ''.
|).
