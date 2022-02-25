"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

_AddSlots: (|
    "The default behavior for all Self objects who inherit from lobby or some
     descendant (like std traits clonable)."
    defaultBehavior* = (|
        "These are convenience helpers. They allow for blocks that take less
         arguments than the caller expects to be called properly:

         list do: [| :item | item printLine ].

         Here, `do:` on `list` would expect the block it is passed to receive
         `value:With:` so it can pass the item and its index. However, since
         the block only accepts `value:`, that would fail. These methods allow
         `value:With:` to delegate to `value:` which makes for nicer behavior."
        value: a = (value).
        value: a With: b = (value: a).
        value: a With: b With: c = (value: a With: b).
        value: a With: b With: c With: d = (value: a With: b With: c).
        value: a With: b With: c With: d With: e = (value: a With: b With: c With: d).

        "Clone an object shallowly."
        clone = (_Clone).

        "Return whether the given object is identical to the receiver according
         to the VM."
        == v = (_Eq: v).
        "Return whether the two objects are equal. The objects do not have to
         be identical to be equal."
        = v = (self == v).

        "Convenient failure block for when you want the error to be fatal."
        raiseError = (|
            value = ( _Error: 'Error raised' ).
            value: err = ( _Error: 'Error raised (FIXME: Print the error)' ).
        |).
    |).
|)
