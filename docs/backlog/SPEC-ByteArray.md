# Backlog: ByteArray built-in type

**Status:** Implemented (type named **ArrayBuf** in First). See first-reference [ArrayBuf.md](../../first-reference/ArrayBuf.md). Includes `arrayBufCreate`, `arrayBufLength`, `arrayBufGet`, `arrayBufSet`, Base64 (`base64Encode`, `base64Decode`), and binary file I/O (see SPEC-BinaryFileIO).

## Summary

Add a built-in type `ByteArray` (similar to JS `Uint8Array` / `ArrayBuffer`) representing a contiguous sequence of bytes. Used for binary data and binary file I/O.

## Language

- **Type name:** `ByteArray` (built-in, like `Int`, `Float`, `String`).
- **Semantics:** Immutable or mutable buffer of bytes (0–255). Length fixed at creation.

## Required changes

### Parser / AST

- Lexer: no new token (identifier `ByteArray` or keyword).
- AST: add `ByteArray` to primitive/built-in type kind (e.g. `PrimitiveType::ByteArray` or dedicated node).

### Type checker

- Treat `ByteArray` as a first-class type in signatures, assignments, and inference.
- Decide: `Eq`, `ToString` (e.g. hex dump or length), no `Ord` unless specified.

### IR / codegen

- **Representation:** Pointer + length (e.g. `i8*` + `i64`), or single pointer to struct `{ i8* data, i64 length }`. Use consistent ABI for function args/returns.
- **Allocation:** Via runtime (e.g. `first_bytearray_alloc(len)`) or inline alloc + store length.

### Runtime (C++)

- `first_bytearray_alloc(int64_t len)` → returns pointer to zero-filled buffer (or struct); caller/GC frees.
- Optionally: `first_bytearray_get(ptr, i64 index) -> int64_t` (byte 0–255), `first_bytearray_set(ptr, index, value)`; bounds-check or document UB.
- Or expose only via binary file read/write and literal construction (see SPEC-BinaryFileIO).

### Stdlib / API (First)

- `byteArrayCreate(length: Int) -> ByteArray` — create zero-filled.
- Optional: `byteArrayLength(b: ByteArray) -> Int`, indexing `b[i]` (get/set as Int 0–255) if language supports it.
- See SPEC-BinaryFileIO for `readFileBytes` / `writeFileBytes`.

### Documentation

- Add ByteArray to language spec and first-reference (new doc or String.md companion).

## Out of scope for this spec

- `Byte` as a separate primitive type and literals (e.g. `0xFFu8`).
- Unsigned integer types; bytes as 0–255 via Int is sufficient for v1.
