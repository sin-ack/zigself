'../objects/everything.self' _RunScript.

(|
    fib: i = (
        (2 < i) ifTrue: [ ^ (fib: i - 1) + (fib: i - 2) ].
        1
    ).

    main = (
        1 to: 20 + 1 Do: [| :i |
            'Fibonacci(' print. i _StringPrint. ') is: ' print.
            (fib: i) _StringPrint.
            '' printLine.
        ].
    ).
|) main.
