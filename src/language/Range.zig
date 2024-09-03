// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

//! This struct contains start and end positions for a given AST node. It does
//! not contain line and column information; that information should be obtained
//! when necessary from the script.

start: usize,
end: usize,
