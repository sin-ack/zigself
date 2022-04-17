(|
    parent* = std testing test.

    foo = (| parent* = std traits singleton. hash = 1. = v = (true). value = 1 |).
    bar = (| parent* = std traits singleton. hash = 1. = v = (true). value = 2 |).

    run = (| map |
        map: std hashTable copy.

        map add: foo.
        expect: (map at: foo) ToBe: foo.
        expect: (map at: foo) value ToBe: 1.
        expect: map size ToBe: 1.

        map add: bar.
        expect: (map at: bar) ToBe: bar.
        expect: (map at: bar) value ToBe: 2.
        "Make sure map doesn't grow when replacing existing object."
        expect: map size ToBe: 1.
    ).
|) run.
