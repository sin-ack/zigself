_AddSlots: (| globallyReachableAddSlotsObject = (| flag |) |).

(|
    parent* = self.
    main = (
        globallyReachableAddSlotsObject flag: 'This should fail'.
    ).
|) _Genesis: 'main'.
