"
Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

std traits _AddSlots: (|
    "Every object that can be copied should be inheriting from this."
    clonable = (|
        parent* = self.
        copy = (clone).
    |).

    "Objects that should have a single instance should be inheriting from this."
    singleton = (|
        parent* = self.
        copy = (self).
    |).
|).
