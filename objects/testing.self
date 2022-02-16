"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std _AddSlots: (|
    testing = (|
        "The base object for a test. Provides assertion functions for tests."
        test = (|
            parent* = std traits singleton.

            expect: expected ToBeIdenticalTo: actual = (
                expected == actual ifFalse: [ _Error: 'Expected identical values' ].
            ).
        |).
    |).
|).
