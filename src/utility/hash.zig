// Copyright (c) 2021, sin-ack <sin-ack@protonmail.com>
//
// SPDX-License-Identifier: GPL-3.0-only

const std = @import("std");

// https://cp-algorithms.com/string/string-hashing.html
// 26 lowercase + 26 uppercase + 10 digit + underscore + 21 operators + colon
// = 85 characters.
// We use the closest prime higher than it, 89 as the p value.
// We use the prime 2979709441 for the m value, since that's what I found as
// a random prime. :^)

const P = 89;
const M = 2979709441;

// The first 64 powers of P.
const powers = blk: {
    var powers_array = [_]u64{0} ** 64;
    powers_array[0] = 1;

    for (powers_array[1..], 0..) |*p, i| {
        p.* = (powers_array[i] * P) % M;
    }

    break :blk powers_array;
};

// Translation map for characters to their hash values. 'a' starts with 1 so that
// a, aa, aaa, ... don't all hash to 0.
const character_translation_map = blk: {
    var map = [_]u64{0} ** 128;
    var i = 1;

    var c: u8 = 'A';
    while (c <= 'Z') : (c += 1) {
        map[c] = i;
        i += 1;
    }

    c = 'a';
    while (c <= 'z') : (c += 1) {
        map[c] = i;
        i += 1;
    }

    c = '0';
    while (c <= '9') : (c += 1) {
        map[c] = i;
        i += 1;
    }

    map['_'] = i;
    i += 1;
    map['!'] = i;
    i += 1;
    map['@'] = i;
    i += 1;
    map['#'] = i;
    i += 1;
    map['$'] = i;
    i += 1;
    map['%'] = i;
    i += 1;
    map['^'] = i;
    i += 1;
    map['&'] = i;
    i += 1;
    map['*'] = i;
    i += 1;
    map[','] = i;
    i += 1;
    map[';'] = i;
    i += 1;
    map['/'] = i;
    i += 1;
    map['\\'] = i;
    i += 1;
    map['<'] = i;
    i += 1;
    map['>'] = i;
    i += 1;
    map['='] = i;
    i += 1;
    map['+'] = i;
    i += 1;
    map['-'] = i;
    i += 1;
    map['?'] = i;
    i += 1;
    map['`'] = i;
    i += 1;
    map['~'] = i;
    i += 1;
    map[':'] = i;
    i += 1;
    map['|'] = i;

    break :blk map;
};

/// Creates a 32-bit hash of the given string.
/// The allowed characters are upper and lowercase characters, digits, the underscore,
/// all the operator characters (!@#$%^&*,;/\<>=+-?`~) and the colon.
pub fn stringHash(string: []const u8) u32 {
    var hash_value: u64 = 0;
    var power: u64 = powers[0];

    for (string, 0..) |c, i| {
        const mapped_character = character_translation_map[c];
        std.debug.assert(mapped_character != 0);

        hash_value = (hash_value + (mapped_character * power)) % M;
        power = if (i < powers.len - 1) powers[i + 1] else ((power * P) % M);
    }

    return @intCast(u32, hash_value);
}
