(|
    lobby* = self.
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

        traits string _AddSlots: (| printLine = ( _PrintLine ) |).
        traits number _AddSlots: (|
            + n = ( _IntAdd: n ).
            < n = ( _IntLT: n ).
        |).
    ).

    checkIfWorldWorks = (
        ((1 + 1) < 3) ifTrue: [
            ^ 'The world is working!'
        ] False: [].
        'The world is broken!'
    ).

    main = (
        createBasicEnvironment.
        checkIfWorldWorks.
    )
|) main _PrintLine.
