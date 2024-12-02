(|
    parent* = std testing test.

    run = (| v. didCallAbsent |
        v: std vector copy.
        v add: 1.

        didCallAbsent: false.
        v remove: 2 IfAbsent: [didCallAbsent: true].
        expectToBeTrue: didCallAbsent.

        didCallAbsent: false.
        v remove: 0 IfAbsent: [didCallAbsent: true].
        expectToBeFalse: didCallAbsent.
    ).
|) run.
