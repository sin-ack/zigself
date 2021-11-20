(|
    lobby* = self.
    "FIXME: Remove these _Nil calls by automatically evaluating to nil for
            nil identifier slots. Alternatively have a specialized NilNode
            which executes into globalNil."
    basicEnvironment* <- _Nil.

    createBasicEnvironment = (| t <- _Nil. f <- _Nil |
        t: 0 _IntLT: 1.
        f: 1 _IntLT: 0.

        basicEnvironment: ().
        basicEnvironment _AddSlots: (| true = t. false = f. nil = _Nil. |).

        true _AddSlots: (| parent* = () |).
        false _AddSlots: (| parent* = () |).

        true parent _AddSlots: (| ifTrue: tb False: fb = ( tb value ) |).
        false parent _AddSlots: (| ifTrue: tb False: fb = ( fb value ) |).

        '' parent _AddSlots: (| printLine = ( _StringPrint. '\n' _StringPrint ) |).
        0 parent _AddSlots: (|
            + n = ( _IntAdd: n ).
            < n = ( _IntLT: n ).
        |).
    ).

    cleanupBasicEnvironment = (| fb <- _Nil |
        fb: [ 'Failed removing slots!\n' _StringPrint. _Exit: 1 ].

        '' parent _RemoveSlot: 'print'     IfFail: fb.
        '' parent _RemoveSlot: 'printLine' IfFail: fb.
        0 parent  _RemoveSlot: '+'         IfFail: fb.
        0 parent  _RemoveSlot: '<'         IfFail: fb.
        true      _RemoveSlot: 'parent'    IfFail: fb.
        false     _RemoveSlot: 'parent'    IfFail: fb.
    ).

    checkIfWorldWorks = (
        ((1 + 1) < 3) ifTrue: [
            ^ 'The world is working!'
        ] False: [].
        'The world is broken!'
    ).

    main = (
        createBasicEnvironment.
        checkIfWorldWorks printLine.
        cleanupBasicEnvironment.
    )
|) main.
