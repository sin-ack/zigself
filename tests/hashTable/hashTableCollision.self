(|
    parent* = std testing test.

    someObject = (|
        parent* = std traits singleton.
        hash = 1.

        isTheRightOne = true.
    |).

    someOtherObject = (|
        parent* = std traits singleton.
        hash = 1.

        isTheRightOne = false.
    |).

    run = (| ht |
        ht: std hashTable copy.
        ht add: someObject.
        ht add: someOtherObject.

        expect: (ht at: someObject) ToBe: someObject.
        expectToBeTrue: (ht at: someObject; isTheRightOne).

        expect: (ht at: someOtherObject) ToBe: someOtherObject.
        expectToBeFalse: (ht at: someOtherObject; isTheRightOne).
    ).
|) run.
