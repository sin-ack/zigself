'Hello world!' printLine.

"A test comment!"
_AddSlot: (|
  point = (|
    parent* = traits clonable.
    x <- 0.
    y <- 0.

    copy = (| c |
      c: self clone.
      c
    ).

    distanceFromOrigin = ( ((x square) + (y square)) squareRoot ).
  |).
|).
