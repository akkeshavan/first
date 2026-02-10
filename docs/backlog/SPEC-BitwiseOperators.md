# Backlog: Bitwise operators

**Status:** Not implemented. Spec for future implementation.

## Summary

Add bitwise operators for integer operands: and (`&`), or (`|`), xor (`^`), not (`~`), left shift (`<<`), right shift (`>>`).

## Operators

| Operator | Name      | Type           | Notes                    |
|----------|-----------|----------------|--------------------------|
| `&`      | and       | `(Int, Int) -> Int` | Bitwise AND              |
| `\|`     | or        | `(Int, Int) -> Int` | Bitwise OR               |
| `^`      | xor       | `(Int, Int) -> Int` | Bitwise XOR              |
| `~`      | not       | `Int -> Int`   | Bitwise NOT (unary)      |
| `<<`     | shl       | `(Int, Int) -> Int` | Left shift               |
| `>>`     | shr       | `(Int, Int) -> Int` | Right shift (define arithmetic vs logical) |

## Required changes

### Parser / AST

- Lexer: tokens for `&`, `|`, `^`, `~`, `<<`, `>>` (watch for conflict with existing `|` in type/pattern syntax).
- AST: binary expr nodes for `&`, `|`, `^`, `<<`, `>>`; unary expr for `~`.
- Precedence: define relative to arithmetic and comparison (e.g. `&` tighter than `^` tighter than `|`; shifts in between).

### Type checker

- Operands must be `Int`. Result type `Int`. Reject Float, Bool, String.
- For `>>`: document whether arithmetic (sign-extend) or logical (zero-fill) and stick to it for signed Int.

### IR generator

- Map to LLVM: `and`, `or`, `xor`, `shl`, `ashr` (arithmetic) or `lshr` (logical). For `~`, use `xor x, -1` or LLVM `not`.
- No runtime calls; all primitive instructions.

### Documentation

- Add to language specification (expressions / operators).
- Optional: short note in first-reference (e.g. Prelude or a “Built-in operators” section).

## Design choices to fix before implementation

- **Right shift:** Arithmetic (`>>` on signed = sign-extend) vs logical (zero-fill). For signed `Int`, arithmetic is common.
- **Shift amount:** Out-of-range (e.g. `n << 64`) — mask to word size or undefined; document.
