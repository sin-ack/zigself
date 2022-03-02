"
Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

_AddSlots: (|
    "This is the place where libraries can place their objects.
     New objects should be namespaced in their respective objects.
     The standard library lives in std."
    globals* = (|
        std = (|
            traits = (|
                float = 0.0 parent.
                integer = 0 parent.
                string = '' parent.
            |).
        |).
    |).
|).

"FIXME: Blocks created at top level crash the virtual machine."
(|
    parent* = self.

    addTraitsBlock = (| traitsBlock |
        traitsBlock: [] parent.
        std traits _AddSlots: (| block = traitsBlock |).
    ).
|) addTraitsBlock.
