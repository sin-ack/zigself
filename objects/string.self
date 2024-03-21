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

    "Split the current string with the given delimiter, and put the results in the
     given implicitKeyInsertable collection. If a collection is not given, a std
     vector copy is used by default."
    splitBy: substring = (splitBy: substring CollectingInto: std vector copyRemoveAll).
    splitBy: substring CollectingInto: collection = (| indices. index. previous |
        indices: std vector copyRemoveAll.
        index: 0.

        "Empty substring means we want to split this string into its characters.
         That will include the first (empty) character, so let's skip it."
        substring isEmpty ifTrue: [ index: index succ ].

        [ index < size ] whileTrue: [
            findSubstring: substring
                FromIndex: index
                IfPresent: [| :substringIndex |
                    indices add: substringIndex.
                    index: substringIndex succ.
                ]
                 IfAbsent: [ index: size ].
        ].

        indices add: size.

        "The first substring must be appended outside because we do not want to add the substring length."
        collection add: (copyFrom: 0 Until: (indices at: 0)).

        "Find and append all the rest of the substrings."
        previous: indices at: 0.
        1 to: indices size Do: [| :i. current |
            current: indices at: i.
            collection add: (copyFrom: (previous + substring size) Until: current).
            previous: current.
        ].

        collection
    ).

    "Find the given substring, starting from the given index. Call presentBlock
     with the first index at which the substring is found if present, and call
     absentBlock with no arguments otherwise."
    findSubstring: substring
        FromIndex: index
        IfPresent: presentBlock
         IfAbsent: absentBlock = (
        index through: size - substring size Do: [| :i |
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

    copyFrom: start = (copyFrom: start Size: size prec).
    copyFrom: start Size: size = (| targetBuffer |
        targetBuffer: copySize: size.

        "If start is 0 then we're already done, no need to copy."
        start = 0 ifTrue: [ ^ targetBuffer ].

        0 to: size Do: [| :i |
            targetBuffer at: i Put: (at: start + i).
        ].

        targetBuffer
    ).

    "Return the 32-bit hash of this string."
    hash = (|
        hashMask = 0xFFFFFFFF.
        result
    |
        "A port of SerenityOS' string hash:"
        "https://github.com/SerenityOS/serenity/blob/daaa8a57f0d7e638c434b08b22a5f2bc107480a6/AK/StringHash.h#L13"
        result: 0.
        each: [| :byte |
            result: result + byte.
            result: result + (result << 10).
            result: result bitXor: (result >> 6).
        ].

        result: result + (result << 3).
        result: result bitXor: (result << 11).
        result: result + (result << 15).
        result bitAnd: hashMask
    ).
|).

std traits string _AddSlots: (|
    "Convert this string to an integer, interpreted as decimal."
    asInteger = (asIntegerBase: 10).

    "Convert this string to an integer, interpreted as hexadecimal."
    asHexInteger = (asIntegerBase: 16).

    asIntegerBase: base = (| value. zeroByte = '0' at: 0. nineByte = '9' at: 0. aByte = 'A' at: 0. zByte = 'Z' at: 0. |
        isEmpty ifTrue: [ _Error: 'empty string cannot be converted to integer' ].
        base: base asInteger.
        (base < 0) || [base > 36] ifTrue: [
            _Error: 'base can at most be 36'
        ].

        value: 0.
        "NOTE: This isn't Unicode-aware but we don't allow Unicode characters anyway."
        each: [| :byte. index |
            [| :break |
                (byte >= zeroByte) && [byte <= nineByte] ifTrue: [
                    value: (value * base) + (byte - zeroByte).
                    break value.
                ].

                "NOTE: The AND operation is used to convert lowercase letters to uppercase."
                byte: (byte bitAnd: 0b11011111).
                (byte >= aByte) && [byte <= zByte] ifTrue: [
                    value: (value * base) + (byte + (10 - aByte)).
                    break value.
                ].

                _Error: 'Invalid character in string'.
            ] break.
        ].

        value.
    ).
|).

std _AddSlots: (|
    string = ''.
|).
