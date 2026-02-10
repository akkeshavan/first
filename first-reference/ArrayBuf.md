# ArrayBuf

**ArrayBuf** is a built-in type (like JS/TS `ArrayBuffer`): a mutable buffer of bytes (0–255). Used for binary data, file I/O, and Base64. These functions are always in scope; no import is required.

**Interactions only:** Because ArrayBuf is mutable, it may only be used in **interactions**, not in pure **functions**. You cannot have ArrayBuf as a function parameter or return type, and you cannot create or use ArrayBuf inside a function body (e.g. `arrayBufCreate`, `readFileBytes`, `base64Decode`, or variables of type ArrayBuf).

---

## Type and representation

- **Type:** `ArrayBuf` — pointer to a runtime block `[length, data...]`. Length is fixed at creation.
- **Bytes:** Values 0–255 (use **Int** when getting/setting).

---

## Create and length

| Function | Type | Description |
|----------|------|-------------|
| `arrayBufCreate(length)` | `Int -> ArrayBuf` | Allocate a new buffer of **length** bytes (zero-filled). |
| `arrayBufLength(buf)` | `ArrayBuf -> Int` | Return the length. Returns 0 if buf is null. |

---

## Get and set bytes

| Function | Type | Description |
|----------|------|-------------|
| `arrayBufGet(buf, index)` | `(ArrayBuf, Int) -> Int` | Byte at **index** (0–255). Returns 0 if out of range. |
| `arrayBufSet(buf, index, value)` | `(ArrayBuf, Int, Int) -> Unit` | Set byte at **index** to **value & 0xFF**. No-op if out of range. |

---

## Iteration (for-in)

You can iterate over the bytes of an ArrayBuf with **for-in**; the loop variable has type **Int** (each byte 0–255):

```first
for b in buf {
  println(intToString(b));
}
```

---

## Binary file I/O

| Function | Type | Description |
|----------|------|-------------|
| `readFileBytes(filename)` | `String -> ArrayBuf` | Read entire file as raw bytes. Returns empty ArrayBuf (length 0) on error. |
| `writeFileBytes(filename, data)` | `(String, ArrayBuf) -> Unit` | Write entire buffer to file (overwrites if exists). |

---

## Base64

| Function | Type | Description |
|----------|------|-------------|
| `base64Encode(buf)` | `ArrayBuf -> String` | Encode buffer to Base64 string. |
| `base64Decode(s)` | `String -> ArrayBuf` | Decode Base64 string to buffer. Returns empty ArrayBuf on error. |

---

## ToString

`ArrayBuf` implements **ToString** (e.g. for `toString(buf)`): returns a string like `"<ArrayBuf length=5>"`. The built-in helper is **`arrayBufToString(buf)`** → `String`.

---

## Example

```first
interaction main() -> Unit {
  let buf = arrayBufCreate(4);
  arrayBufSet(buf, 0, 72);
  arrayBufSet(buf, 1, 105);
  arrayBufSet(buf, 2, 33);
  println(intToString(arrayBufGet(buf, 0)));
  writeFileBytes("out.bin", buf);
  let read = readFileBytes("out.bin");
  println(toString(read));
  let b64 = base64Encode(buf);
  println(b64);
  let decoded = base64Decode(b64);
  println(intToString(arrayBufLength(decoded)));
}
```
