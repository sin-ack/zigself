"
Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits string _AddSlots: (|
    mutable* = std mixins mutableCollection.
    indexable* = std mixins indexableCollection.
    parent* = std traits collection.

    "Trait & mixin requirements"
    size = (_ByteArraySize).
    at: index = (_ByteAt: index).
    at: index Put: value = (_ByteAt: index Put: value).

    "Writing strings to stdout"
    "NOTE: nil to prevent writing out the result in the REPL"
    print = (std out write: self. nil).
    printLine = (print. '\n' print).

    "Binary messages"
    = v = (_ByteArrayEq: v).
    , s = (_ByteArrayConcatenate: s).

    asString = (self).

    filler = ' '.
    "FIXME: Byte array objects should copy their values when cloned."
    copy = (copySize: size).
    copySize: size = (copySize: size FillingExtrasWith: filler).
    copySize: size FillingExtrasWith: filler = (_ByteArrayCopySize: size FillingExtrasWith: filler).

    "Join the given collection of strings into a single string, using the
     receiver as the delimiter."
    join: strings = (|
        totalLength.
        joined.
        offset.
        wroteOne.

        write: str = (
            0 to: str size Do: [| :strOffset |
                joined at: offset + strOffset
                       Put: str at: strOffset.
            ].
            offset: offset + str size.
        ).
    |
        totalLength: strings size prec * size.
        strings each: [| :s | totalLength: totalLength + s size ].

        joined: std string copySize: totalLength.
        offset: 0.
        wroteOne: false.
        strings each: [| :value. :i |
            wroteOne ifTrue: [ write: self ].
            write: value.
            wroteOne: true.
        ].

        joined
    ).

    "Reverse the receiver into a new string."
    reverse = (| newString |
        newString: copy.
        each: [| :byte. :i | newString at: size prec - i Put: byte].
        newString
    ).

    "FIXME: Convert this to return a collector!"
    splitOn: substring = (| indicesList. substrings. head. index |
        indicesList: std list copyRemoveAll.
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
        substrings: std array copySize: indicesList size.

        "The first substring must be appended outside because we do not want to add the substring length."
        substrings at: 0 Put: (copyFrom: 0 Until: (indicesList at: 0)).

        "Find and append all the substrings."
        index: 1.
        head: indicesList nodes.
        indicesList nodes do: [| :node |
            (node next == head) ifTrue: [ ^ substrings ].
            substrings at: index Put: (copyFrom: (node value + substring size) Until: (node next value)).
            index: index succ.
        ].

        substrings
    ).

    "Find the given substring, starting from the given index. Call presentBlock
     with the first index at which the substring is found if present, and call
     absentBlock with no arguments otherwise."
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

    copyFrom: start Until: end = (
        start = end ifTrue: [ ^ '' ].
        start < end ifFalse: [ ^ copyFrom: end Until: start ].
        copyFrom: start Size: (end - start).
    ).

    copyFrom: start Size: size = (| targetBuffer |
        targetBuffer: copySize: size.

        "If start is 0 then we're already done, no need to copy."
        start = 0 ifTrue: [ ^ targetBuffer ].

        0 to: size Do: [| :i |
            targetBuffer at: i Put: (at: start + i).
        ].

        targetBuffer
    ).
|).

std traits string _AddSlots: (|
    asInteger = (| value. zeroByte = '0' at: 0. nineByte = '9' at: 0 |
        isEmpty ifTrue: [ _Error: 'empty string cannot be converted to integer' ].

        value: 0.
        each: [| :byte |
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
