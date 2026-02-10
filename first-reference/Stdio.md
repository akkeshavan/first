# Stdio

Standard input/output and file I/O. These functions are always in scope; no import is required.

---

## Console output

| Function | Type | Description |
|----------|------|--------------|
| `print(s)` | `String -> Unit` | Write string to stdout (no newline) |
| `println(s)` | `String -> Unit` | Write string to stdout followed by a newline |

---

## Console input

| Function | Type | Description |
|----------|------|--------------|
| `readLine()` | `() -> String` | Read one line from stdin. Returns the line (without trailing newline) or an empty string on EOF. Uses a thread-local buffer. |

---

## File I/O

| Function | Type | Description |
|----------|------|--------------|
| `readFile(filename)` | `String -> String` | Read entire file as a string. Returns contents or empty string on error (e.g. file not found). Uses a static buffer. |
| `writeFile(filename, content)` | `(String, String) -> Unit` | Write string to file, overwriting if it exists. Creates the file if it does not exist. |

### Binary files

| Function | Type | Description |
|----------|------|-------------|
| `readFileBytes(filename)` | `String -> ArrayBuf` | Read entire file as raw bytes. Returns empty ArrayBuf on error. See [ArrayBuf](ArrayBuf.md). |
| `writeFileBytes(filename, data)` | `(String, ArrayBuf) -> Unit` | Write ArrayBuf contents to file (overwrites if exists). |

Example:

```first
interaction main() -> Unit {
  println("Enter your name:");
  let name = readLine();
  println("Hello, " + name);
  writeFile("out.txt", "saved");
  println(readFile("out.txt"));
}
```
