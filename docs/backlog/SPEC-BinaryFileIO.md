# Backlog: Binary file read/write

**Status:** Implemented. Uses **ArrayBuf** (see SPEC-ByteArray). Documented in first-reference [Stdio.md](../../first-reference/Stdio.md) (Binary files) and [ArrayBuf.md](../../first-reference/ArrayBuf.md).

## Summary

Add stdlib functions to read and write file contents as raw bytes, using the ByteArray type. Complements existing text `readFile` / `writeFile`.

## API (First)

| Function | Type | Description |
|----------|------|-------------|
| `readFileBytes(filename: String) -> ByteArray` | Read entire file as raw bytes. Returns empty ByteArray on error (file not found, etc.). |
| `writeFileBytes(filename: String, data: ByteArray) -> Unit` | Write entire ByteArray to file, overwriting if exists. |

No separate “open/seek/read chunk” API in v1; whole-file only.

## Required changes

### Runtime (C++)

- **Read:** e.g. `uint8_t* first_read_file_bytes(const char* path, int64_t* out_len)`. Open file in binary mode, read to buffer (e.g. vector or `first_alloc`), set length, return pointer. Caller/GC frees. Return `nullptr` and 0 on error.
- **Write:** e.g. `void first_write_file_bytes(const char* path, const uint8_t* data, int64_t len)`. Open in binary mode, write bytes, close.

### Compiler (type checker)

- Add built-in function cases: `readFileBytes(s: String) -> ByteArray`, `writeFileBytes(s: String, b: ByteArray) -> Unit`. Argument count and types enforced.

### Compiler (IR)

- In getStdlibSig (or equivalent): map `readFileBytes` to C symbol that returns pointer (and length); construct ByteArray value (ptr+len) from return. Map `writeFileBytes` to C symbol taking (path, ptr, len); pass ByteArray’s ptr and length as arguments.
- Ensure ByteArray ABI (how ptr+len are passed) is consistent with runtime.

### Stdlib / docs

- Document in first-reference Stdio.md: “Binary files” section with `readFileBytes` and `writeFileBytes`.

## Error handling

- **Read:** On error (no file, permission, etc.), return empty ByteArray (length 0) or a dedicated “error” value; document choice.
- **Write:** On error, either ignore, abort, or future: return `Bool`/`Option`; for v1, document “best effort” or abort.

## Dependencies

- ByteArray type and runtime representation (SPEC-ByteArray) must be implemented first.
