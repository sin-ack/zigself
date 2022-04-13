(|
    parent* = std testing test.

    run = (| ht1. ht2 |
        ht1: std hashTable copy.
        ht1 add: 1.
        expectToBeTrue: ht1 contains: 1.

        ht2: ht1 copy.
        expectToBeTrue: ht2 contains: 1.
        ht2 add: 2.
        expectToBeTrue: ht2 contains: 2.
        expectToBeFalse: ht1 contains: 2.

        ht2: ht1 copyRemoveAll.
        expectToBeFalse: ht2 contains: 1.
        expectToBeFalse: ht2 contains: 2.
    ).
|) run.
