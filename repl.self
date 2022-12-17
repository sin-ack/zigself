#!/usr/bin/env self
'objects/everything.self' _RunScript.

std scheduler startBySending: 'spawnEntrypoint' To: (|
    parent* = std traits actor.

    main = (| stdin |
        'Welcome to the ZigSelf REPL!' printLine.
        'The standard library is in std.' printLine.

        stdin: std file open: '/dev/stdin'.

        [| :continue. data. result |
           stdin atEOF ifTrue: [ _Exit: 0 ].

           std out write: '> '.
           data: stdin readLine.
           result: data _EvaluateStringContext: (| parent* = lobby |) IfFail: continue.

           (true == result) ifTrue: [ 'true' printLine. ] False: [
           (false == result) ifTrue: [ 'false' printLine. ] False: [
           (nil == result) ifFalse: [ result _Inspect. '\n' print. ]]].
        ] loopContinue.
    ).
|).
