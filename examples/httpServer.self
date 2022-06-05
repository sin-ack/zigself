'../objects/everything.self' _RunScript.

_AddSlots: (|
    httpRequestHandler = (|
        parent* = std traits actor.

        clientSocket <- nil.
        setClientSocket: socket = (clientSocket: socket).

        writeToClient: s = (
            _Write: s size BytesFrom: s AtOffset: 0 Into: clientSocket IfFail: [| :err |
                err = 32 ifFalse: [^raiseError value: err].
                'Client closed the connection' printLine.
                ^ false
            ].
            true
        ).

        readFromClientInto: s = (
            _Read: s size BytesInto: s AtOffset: 0 From: clientSocket IfFail: raiseError.
        ).

        main = (| buffer |
            [nil == clientSocket] whileTrue: [yield].

            buffer: std string copySize: 4096.
            [| :break. bytesRead |
                bytesRead: readFromClientInto: buffer.
                bytesRead < 4096 ifTrue: break.
            ] loopBreak.

            [| :break |
                writeToClient: 'HTTP/1.1 200 OK\r\n'; ifFalse: break.
                writeToClient: 'Content-Type: text/html; charset=utf-8\r\n'; ifFalse: break.
                writeToClient: '\r\n'; ifFalse: break.
                writeToClient: '<!doctype html>\n'; ifFalse: break.
                writeToClient: '<html><body><h1>Hello world!</h1><p>This is a test page using a HTTP server built in zigSelf.</p></body></html>'; ifFalse: break.
            ] break.

            _Close: clientSocket.
        ).
    |).

    serverActor = (|
        parent* = std traits actor.

        createServerSocket = (| results |
            results: _GetAddrInfoForHost: nil
                                    Port: 8000
                                  Family: 0
                              SocketType: 1
                                Protocol: 0
                                   Flags: 0
                                  IfFail: raiseError.

            results each: [| :result. socket |
                [| :break |
                    socket: _SocketWithFamily: result family
                                         Type: result socketType
                                     Protocol: result protocol
                                       IfFail: break.
                    _BindFD: socket ToSockaddrBytes: result sockaddrBytes IfFail: break.
                    _ListenOnFD: socket WithBacklog: 512 IfFail: break.
                    ^ socket.
                ] break.
            ].

            _Error: 'Could not bind to any available socket!'.
        ).

        main = (| socket |
            socket: createServerSocket.
            '[Server actor] Server bound at port 8000!' printLine.
            [| clientSocket. client |
                clientSocket: _AcceptFromFD: socket IfFail: raiseError.
                client: httpRequestHandler spawn.
                client setClientSocket: clientSocket.
            ] loop.
        ).
    |).
|).

std scheduler startWith: serverActor.
