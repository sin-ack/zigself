"Hello world"
'Hello world!' printLine.

"Adding a slot to lobby"
_AddSlot: (|
  point = (|
    parent* = traits clonable.
    x <- 0.
    y <- 0.

    copy = (| c |
      c: self clone.
      c
    ).

    distanceFromOrigin = ( ((x square) add: (y square)) squareRoot ).

    copyX: x Y: y = (| c |
      c: copy.
      c x: x.
      c y: y.
      c
    ).

    + p = (| c |
      c: copy. c x: x + p x. c y: y + p y.
      c
    )
  |).
|).

"Fizzbuzz"
1 to: 100 Do: [| :i. str <- '' |
  ((i % 3) = 0) ifTrue: [ str: str, 'Fizz' ].
  ((i % 5) = 0) ifTrue: [ str: str, 'Buzz' ].
  str isEmpty ifTrue: [ i printString printLine ] False: [ str printLine ].
].

"Fibonacci numbers"
1 to: 10 Do: [|
  :i.
  ctx = (| fib: n = ( (n lessThan: 1) ifTrue: [ 1 ] False: [ (fib: i prec) add: fib: i prec prec ] ) |)
|
  (ctx fib: i) printLine
].
