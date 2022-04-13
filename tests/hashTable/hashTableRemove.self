(|
    parent* = std testing test.

    run = (| ht |
        ht: std hashTable copy.
        ht add: 1.

        expectToBeTrue: ht contains: 1.
        expectToNotFail: [| :failBlock | ht remove: 1 IfAbsent: failBlock ].
        expectToBeFalse: ht contains: 1.
        expectToFail: [| :failBlock | ht remove: 1 IfAbsent: failBlock value ].
    ).
|) run.
