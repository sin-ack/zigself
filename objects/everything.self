"
Copyright (c) 2021-2022, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

"Required by default slots"
_AddSlots: (| nil = _Nil |).

"Basic Self operations and objects."
'globals.self' _RunScript.
'basicTraits.self' _RunScript.
'defaultBehavior.self' _RunScript.
'booleans.self' _RunScript.
'integer.self' _RunScript.
'block.self' _RunScript.

"The collection traits family and its users."
'collection.self' _RunScript.

'array.self' _RunScript.
'vector.self' _RunScript.
'string.self' _RunScript.
'list.self' _RunScript.
'hashTable.self' _RunScript.
'hashSet.self' _RunScript.
'hashMap.self' _RunScript.

"Collector for easy collecting."
'collector.self' _RunScript.

"A basic testing framework."
'testing.self' _RunScript.

"Interacting with the OS."
'os.self' _RunScript.
'file.self' _RunScript.

"Actors and everything to do with them."
'actor.self' _RunScript.
'scheduler.self' _RunScript.
