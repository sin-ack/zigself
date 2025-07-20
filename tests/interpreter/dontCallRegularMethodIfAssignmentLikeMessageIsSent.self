"Checks for a regression in the lookup code. Suppose we had this object:
(|
    foo = 1.
    foo: x = (""Do something."").
|)
We would accidentally send a message to `foo' when sending `foo:' to this
object."

(|
    parent* = std traits singleton.

    foo = (false assert: 'Lookup code broke!').
    foo: x = ("Hooray, no regression!").

    run = (
        foo: 'x'.
    ).
|) run.
