'../objects/everything.self' _RunScript.

(|
    fib: i = (
        (2 < i) ifTrue: [ ^ (fib: i - 1) + (fib: i - 2) ].
        1
    ).

    main = (
        1 to: 25 + 1 Do: [| :i |
            'Fibonacci(', i asString, ') is: ', (fib: i) asString; printLine.
        ].
    ).
|) main.
