(|
    parent* = std testing test.

    run = (
        "Basic operations"
        expect: 1 + 1 ToBe: 2.
        expect: 1 - 1 ToBe: 0.
        expect: 2 * 3 ToBe: 6.
        expect: 6 / 2 ToBe: 3.
        expect: 5 / 2 ToBe: 2.

        expect: 5 % 2 ToBe: 1.
        expect: 5negate % 2 ToBe: 1.

        expect: 2 >> 1 ToBe: 1.
        expect: 2 << 1 ToBe: 4.

        "Bitwise operations, k-map style"
        expect: (0b1010 bitAnd: 0b1100) ToBe: 0b1000.
        expect: (0b1010 bitOr: 0b1100) ToBe: 0b1110.
        expect: (0b1010 bitXor: 0b1100) ToBe: 0b0110.

        expect: 2 ** 3 ToBe: 8.

        "Unary operations"
        expect: 1succ ToBe: 2.
        expect: 2prec ToBe: 1.

        expect: 1negate + 1 ToBe: 0.

        expect: 0b10000 bitLength ToBe: 5.
        expect: 0b111 bitLength ToBe: 3.

        expect: 1negate abs ToBe: 1.
        expect: 1abs ToBe: 1.

        "Comparison"
        expectToBeTrue: 1 < 2.
        expectToBeFalse: 1 > 2.
        expectToBeTrue: 1 <= 2.
        expectToBeFalse: 1 >= 2.
        expectToBeTrue: 1 <= 1.
        expectToBeTrue: 1 = 1.
        expectToBeFalse: 1 = 2.

        expectToBeTrue: 2 > 1.
        expectToBeTrue: 2 >= 1.

        expect: (1 max: 2) ToBe: 2.
        expect: (2 max: 1) ToBe: 2.

        expect: (1 min: 2) ToBe: 1.
        expect: (2 min: 1) ToBe: 1.
    )
|) run.
