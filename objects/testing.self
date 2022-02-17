"
Copyright (c) 2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std _AddSlots: (|
    testing = (|
        "The base object for a test. Provides assertion functions for tests."
        test = (|
            parent* = std traits singleton.

            "Checks whether the expected value is identical to the actual value."
            expect: actual ToBeIdenticalTo: expected = (
                (expected == actual) ifFalse: [
                    'Expected: ' print. expected _Inspect. '' printLine.
                    'Actual: ' print. actual _Inspect. '' printLine.
                    _Error: 'Expected identical values'
                ].
            ).

            "Checks whether the expected value is equal to the actual value."
            expect: actual ToBe: expected = (
                (expected = actual) ifFalse: [
                    'Expected: ' print. expected _Inspect. '' printLine.
                    'Actual: ' print. actual _Inspect. '' printLine.
                    _Error: 'Expected equal values'
                ].
            ).
        |).
    |).
|).
