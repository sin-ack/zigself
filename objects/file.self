"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    file = (|
        parent* = std traits singleton.

        defaultReadMaximum = 8 kib.

        "FIXME: Add O_* flags."
        open: path = (open: path IfFail: raiseError).
        open: path WithFlags: flags = (open: path WithFlags: flags IfFail: raiseError).
        open: path IfFail: failBlock = (open: path WithFlags: 0 IfFail: failBlock).
        open: path WithFlags: flags IfFail: failBlock = (| newFile |
            (fd = nil) ifFalse: [ _Error: 'Attempted to call std file open:WithFlags: on an open file' ].
            newFile: clone.
            newFile fd: std os openFile: path WithFlags: flags IfFail: failBlock.
            newFile atEOF: false.
            newFile.
        ).

        readAll = (readAllIfFail: raiseError).
        readAllIfFail: failBlock = (| chunks |
            chunks: std vector copyRemoveAll.

            [ atEOF ] whileFalse: [
                chunks add: readIfFail: failBlock.
            ].

            '' join: chunks.
        ).

        read = (readMin: 1).
        readIfFail: failBlock = (readMin: 1 IfFail: failBlock).

        readMin: min = (readMin: min IfFail: raiseError).
        readMin: min IfFail: failBlock = (readMin: min Max: defaultReadMaximum IfFail: failBlock).

        readMax: max = (readMax: max IfFail: raiseError).
        readMax: max IfFail: failBlock = (readMin: 1 Max: max IfFail: failBlock).

        readMin: min Max: max = (readMin: min Max: max IfFail: raiseError).

        readMin: min Max: max IfFail: failBlock = (| buffer. bytesRead |
            atEOF ifTrue: [ ^ '' ].

            buffer: std string copySize: max.
            bytesRead: 0.

            [ bytesRead < min ] whileTrue: [| :break |
                std os tryWhileInterrupted: [| :failBlock. nread |
                    nread: std os read: buffer size - bytesRead
                                  BytesInto: buffer
                                  AtOffset: bytesRead
                                  From: fd
                                  IfFail: failBlock.
                    atEOF: nread = 0.
                    atEOF ifTrue: break.

                    bytesRead: bytesRead + nread.
                ] IfFail: failBlock.
            ].

            (bytesRead = max) ifTrue: [ buffer ] False: [ buffer copySize: bytesRead ].
        ).

        readLine = (readLineIfFail: raiseError).
        "FIXME: This is a pretty bad implementation. Implement some kind of
                buffering."
        readLineIfFail: failBlock = (| buffer. offset |
            buffer: ''.

            [| :break. byte |
                byte: readMax: 1 IfFail: failBlock.
                atEOF ifTrue: break.
                ((byte at: 0) = 10) ifTrue: break.

                buffer: buffer, byte.
            ] loopBreak.

            buffer.
        ).

        write: data = (write: data IfFail: raiseError).
        write: data IfFail: failBlock = (| bytesWritten |
            bytesWritten: 0.

            [ bytesWritten < data size ] whileTrue: [
                std os tryWhileInterrupted: [| :failBlock. nwritten |
                    nwritten: std os write: data size
                                     BytesFrom: data
                                     AtOffset: 0
                                     Into: fd
                                     IfFail: failBlock.

                    bytesWritten: bytesWritten + nwritten.
                ] IfFail: failBlock.
            ].

            bytesWritten
        ).

        close = (
            _Close: fd.
            fd: nil.
        ).
    |).
|).

std _AddSlots: (|
    file = (|
        parent* = std traits file.
        fd.
        atEOF <- false.
    |).
|).

std _AddSlots: (|
    in = std file open: '/dev/stdin'.
    out = std file open: '/dev/stdout' WithFlags: 1.
|).
