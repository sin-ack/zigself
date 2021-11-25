"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

traits string _AddSlots: (|
    parent* = traits clonable.

    splitOn: substring = (| indicesList. substringsList. index |
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
        substringsList: list copyRemoveAll.

        "The first substring must be appended outside because we do not want to add the substring length."
        substringsList append: (copyFrom: 0 Until: (indicesList at: 0)).

        "Find and append all the substrings."
        index: 0.
        [ index < (indicesList size prec) ] whileTrue: [
            substringsList append: (copyFrom: ((indicesList at: index) + substring size) Until: (indicesList at: index succ)).
            index: index succ.
        ].

        substringsList
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

    size = (_ByteVectorSize).
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

    copySize: size = ( _ByteVectorCopySize: size ).
|).

traits string _AddSlots: (|
    toInteger = (| value. zeroByte = '0' at: 0. nineByte = '9' at: 0 |
        (size = 0) ifTrue: [ _Error: 'empty string cannot be converted to integer' ].

        value: 0.
        do: [| :byte |
            ((byte >= zeroByte) && [ byte <= nineByte ]) ifTrue: [
                   value: (value * 10) + (byte - zeroByte).
            ] False: [ _Error: 'string with non-digit characters cannot be converted to integer' ].
        ].

        value
    ).
|)
