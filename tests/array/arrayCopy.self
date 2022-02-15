(|
    parent* = std testing test.

    run = (| arr |
        arr: std array copySize: 3.
        expect: (arr at: 0) ToBeIdenticalTo: nil.
        expect: (arr at: 1) ToBeIdenticalTo: nil.
        expect: (arr at: 2) ToBeIdenticalTo: nil.

        arr: std array copySize: 3 FillingExtrasWith: 9.
        expect: (arr at: 0) ToBeIdenticalTo: 9.
        expect: (arr at: 1) ToBeIdenticalTo: 9.
        expect: (arr at: 2) ToBeIdenticalTo: 9.

        arr: arr copySize: 5 FillingExtrasWith: 5.
        expect: (arr at: 2) ToBeIdenticalTo: 9.
        expect: (arr at: 3) ToBeIdenticalTo: 5.
        expect: (arr at: 4) ToBeIdenticalTo: 5.
    ).
|) run.
