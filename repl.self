'objects/everything.self' _RunScript.

(|
    parent* = std traits singleton.

    repl = (
        'Welcome to the ZigSelf REPL!' printLine.
        'The standard library is in std.' printLine.

        [| data. result |
           std in atEOF ifTrue: [ _Exit: 0 ].

           std out write: '> '.
           data: std in readLine.
           result: data _EvaluateString.

           (true == result) ifTrue: [ 'true' printLine. ] False: [
           (false == result) ifTrue: [ 'false' printLine. ] False: [
           (nil == result) ifFalse: [ result _Inspect. '\n' print. ]]].
        ] loop.
    ).
|) repl.
