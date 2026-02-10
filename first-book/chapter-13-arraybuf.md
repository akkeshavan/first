# Chapter 13: ArrayBuf

This chapter introduces **ArrayBuf**: a mutable buffer of bytes (0–255) for binary data, file I/O, and Base64 encoding. ArrayBuf is a built-in type (similar to JavaScript’s `ArrayBuffer`). Because it is mutable, **ArrayBuf may only be used in interactions**, not in pure functions.

---

## 1. What is ArrayBuf?

**ArrayBuf** is a fixed-length sequence of bytes. Each byte is an **Int** in the range 0–255. Use it when you need:

- Raw binary data (e.g. file contents, network payloads).
- Binary file read/write.
- Base64 encoding and decoding.

**Interactions only:** You cannot have ArrayBuf as a function parameter or return type. Create and use ArrayBuf only inside **interactions** (e.g. `main`).

---

## 2. Creating a buffer and getting its length

| Function | Type | Description |
|----------|------|-------------|
| `arrayBufCreate(length)` | `Int -> ArrayBuf` | Allocate a new buffer of **length** bytes (zero-filled). |
| `arrayBufLength(buf)` | `ArrayBuf -> Int` | Return the number of bytes. Returns 0 if buf is null. |

### Example

```first
import "Prelude"

interaction main() -> Unit {
  let buf = arrayBufCreate(4);
  println("Length: " + intToString(arrayBufLength(buf)));
}
```

---

## 3. Getting and setting bytes

| Function | Type | Description |
|----------|------|-------------|
| `arrayBufGet(buf, index)` | `(ArrayBuf, Int) -> Int` | Byte at **index** (0–255). Returns 0 if out of range. |
| `arrayBufSet(buf, index, value)` | `(ArrayBuf, Int, Int) -> Unit` | Set byte at **index** to **value & 0xFF**. No-op if out of range. |

Indices are zero-based. Values are truncated to a single byte (0–255).

### Example

```first
interaction main() -> Unit {
  let buf = arrayBufCreate(4);
  arrayBufSet(buf, 0, 72);   // 'H'
  arrayBufSet(buf, 1, 105);  // 'i'
  arrayBufSet(buf, 2, 33);   // '!'
  println(intToString(arrayBufGet(buf, 0)));
  println(intToString(arrayBufGet(buf, 1)));
}
```

---

## 4. Iterating with for-in

You can iterate over the bytes of an ArrayBuf with **for-in**. The loop variable has type **Int** (each byte 0–255):

```first
interaction main() -> Unit {
  let buf = arrayBufCreate(4);
  arrayBufSet(buf, 0, 72);
  arrayBufSet(buf, 1, 105);
  arrayBufSet(buf, 2, 33);
  println("bytes in buf:");
  for b in buf {
    println("  " + intToString(b));
  }
}
```

---

## 5. Binary file I/O

| Function | Type | Description |
|----------|------|-------------|
| `readFileBytes(filename)` | `String -> ArrayBuf` | Read entire file as raw bytes. Returns empty ArrayBuf (length 0) on error. |
| `writeFileBytes(filename, data)` | `(String, ArrayBuf) -> Unit` | Write entire buffer to file (overwrites if exists). |

### Example

```first
interaction main() -> Unit {
  let buf = arrayBufCreate(4);
  arrayBufSet(buf, 0, 72);
  arrayBufSet(buf, 1, 105);
  arrayBufSet(buf, 2, 33);
  writeFileBytes("out.bin", buf);
  let read = readFileBytes("out.bin");
  println(toString(read));
}
```

**toString(buf)** (or the ToString implementation for ArrayBuf) yields a string like `"<ArrayBuf length=4>"`, useful for debugging.

---

## 6. Base64 encoding and decoding

| Function | Type | Description |
|----------|------|-------------|
| `base64Encode(buf)` | `ArrayBuf -> String` | Encode buffer to Base64 string. |
| `base64Decode(s)` | `String -> ArrayBuf` | Decode Base64 string to buffer. Returns empty ArrayBuf on error. |

### Example

```first
interaction main() -> Unit {
  let buf = arrayBufCreate(4);
  arrayBufSet(buf, 0, 72);
  arrayBufSet(buf, 1, 105);
  arrayBufSet(buf, 2, 33);
  let b64 = base64Encode(buf);
  println("base64 encoded: " + b64);
  let decoded = base64Decode(b64);
  println("decoded length: " + intToString(arrayBufLength(decoded)));
}
```

---

## 7. Putting it together

A complete example that creates a buffer, writes bytes, saves to a file, reads it back, and demonstrates Base64 and for-in:

```first
import "Prelude"

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
  println("base64 encoded : " + b64);
  let decoded = base64Decode(b64);
  println(intToString(arrayBufLength(decoded)));
  println("bytes in buf:");
  for b in buf {
    println("  " + intToString(b));
  }
}
```

---

## Summary

| Concept | Syntax / idea |
|--------|----------------|
| **ArrayBuf** | Mutable buffer of bytes (0–255); **interactions only**. |
| **Create / length** | `arrayBufCreate(n)`, `arrayBufLength(buf)`. |
| **Get / set** | `arrayBufGet(buf, index)`, `arrayBufSet(buf, index, value)`. |
| **For-in** | `for b in buf { ... }` — loop variable **Int** (byte). |
| **File I/O** | `readFileBytes(filename)`, `writeFileBytes(filename, data)`. |
| **Base64** | `base64Encode(buf)`, `base64Decode(s)`. |

---

## Runnable example: chapter-13-ArrayBuf

The project **examples/chapter-13-ArrayBuf** contains a **main** interaction that demonstrates create, get/set, file write/read, Base64, and for-in over the buffer.

From the repo root:

```bash
cd examples/chapter-13-ArrayBuf
fir run
```

You should see printed bytes, the file written and read back, and the Base64 string and decoded length.

---

## Try it

- Create a buffer of 10 bytes, fill it with values 0..9, and print each with **for-in**.
- Read a small binary file with **readFileBytes**, then encode it with **base64Encode** and print the string.
- Decode a known Base64 string (e.g. `"SGk="`) with **base64Decode** and print **arrayBufLength** and the first byte.

ArrayBuf gives you a simple, interaction-only way to work with raw bytes and files in First.
