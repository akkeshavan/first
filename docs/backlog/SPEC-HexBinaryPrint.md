# Backlog: Print in hex or binary (format helpers)

**Status:** Not implemented. Spec for future implementation.

## Summary

Provide a way to “print anything in hex or binary” by adding stdlib functions that **format** values as hex or binary strings; the user then prints those strings (e.g. `println(intToHex(x))`). No change to `print`/`println` signatures.

## Recommended approach

- **Int:** Add `intToHex(n: Int) -> String` and `intToBinary(n: Int) -> String`. “Print in hex” = `println(intToHex(x))`; “print in binary” = `println(intToBinary(x))`.
- **Optional:** `hexToInt(s: String) -> Int`, `binaryToInt(s: String) -> Int` for parsing.
- **Optional:** `printHex(x: Int)`, `printBinary(x: Int)` as shorthands that print the formatted string (e.g. newline or not).
- **ArrayBuf (optional):** `arrayBufToHex(buf: ArrayBuf) -> String` (and maybe `arrayBufToBinary`) for a hex/binary dump string, then `println(arrayBufToHex(buf))`.
- **Other types:** v1 can be Int-only (and optionally ArrayBuf); Float/String/Bool either “not supported” or a documented rule.

## API (First)

| Function | Type | Description |
|----------|------|-------------|
| `intToHex(n)` | `Int -> String` | Format integer as hex string (e.g. for negative: two’s-complement or unsigned; document once). |
| `intToBinary(n)` | `Int -> String` | Format integer as binary string (same convention for negative). |
| `hexToInt(s)` (optional) | `String -> Int` | Parse hex string to Int. |
| `binaryToInt(s)` (optional) | `String -> Int` | Parse binary string to Int. |
| `arrayBufToHex(buf)` (optional) | `ArrayBuf -> String` | Hex dump of buffer (e.g. "48692100"). |
| `arrayBufToBinary(buf)` (optional) | `ArrayBuf -> String` | Binary representation of buffer. |

## Required changes

### Runtime (C++)

- Implement e.g. `first_int_to_hex(int64_t) -> char*`, `first_int_to_binary(int64_t) -> char*` (heap-allocated string; same ownership as other stdlib string returns).
- Define and document behavior for negative Int (e.g. two’s-complement hex like C’s `%x`, or unsigned view).
- Optional: `first_hex_to_int`, `first_binary_to_int`; `first_arraybuf_to_hex`, etc.

### Compiler

- **Type checker:** Stub/stdlib cases for `intToHex`, `intToBinary` (and any optional helpers): argument and return types.
- **IR / codegen:** Map these names to the C symbols (e.g. in getStdlibSig / stdlib dispatch).

### Documentation

- first-reference: document new functions (e.g. in Stdio.md or a small “Number formatting” section). Mention that “print in hex” is `println(intToHex(x))`.

## Dependencies

- None for Int-only. ArrayBuf formatters require ArrayBuf (implemented; see first-reference ArrayBuf.md).

## Design choices to fix before implementation

- Negative Int: two’s-complement hex (e.g. `-1` → `"ffffffffffffffff"` in 64-bit) vs unsigned view vs error. Same for binary.
- Hex casing: lowercase (`a-f`) vs uppercase (`A-F`); or add a parameter.
- Leading zeros / width: fixed width (e.g. 16 hex digits) vs no leading zeros; optional width parameter or separate function.
