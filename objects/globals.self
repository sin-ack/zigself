"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

_AddSlots: (|
    "This is the place where libraries can place their objects.
     New objects should be namespaced in their respective objects.
     The standard library lives in std."
    globals* = (|
        std = (|
            traits = traits.
        |).
    |).
|).
