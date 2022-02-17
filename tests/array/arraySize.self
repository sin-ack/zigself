(|
    parent* = std testing test.

    run = (| arr |
        arr: std array copySize: 1.
        expect: arr size ToBe: 1.

        arr: arr copySize: 3.
        expect: arr size ToBe: 3.

        expect: std array size ToBe: 0.
    ).
|) run.
