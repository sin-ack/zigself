"
Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>

SPDX-License-Identifier: GPL-3.0-only
"

"Required by default slots"
_AddSlots: (| nil = _Nil |).

'globals.self' _RunScript.
'basicTraits.self' _RunScript.

'defaultBehavior.self' _RunScript.
'booleans.self' _RunScript.
'string.self' _RunScript.
'integer.self' _RunScript.
'block.self' _RunScript.
'vector.self' _RunScript.

'list.self' _RunScript.
'testing.self' _RunScript.
