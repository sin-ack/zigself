_AddSlots: (| globallyReachableAssignmentObject |).
globallyReachableAssignmentObject: (| flag |).

(|
    parent* = self.
    main = (
        globallyReachableAssignmentObject flag: 'This should fail'.
    ).
|) _Genesis: 'main'.
