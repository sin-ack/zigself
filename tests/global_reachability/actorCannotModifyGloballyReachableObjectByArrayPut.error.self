_AddSlots: (| globallyReachableArrayObject = std array copySize: 1 |).
globallyReachableArrayObject at: 0 Put: (| flag |).

(|
    parent* = self.
    main = (
        globallyReachableArrayObject at: 0; flag: 'This should fail'.
    ).
|) _Genesis: 'main'.
