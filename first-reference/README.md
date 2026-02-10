# First standard library reference

This directory holds the reference documentation for libraries supplied with First.

- **[Prelude](Prelude.md)** — Core types and interfaces: `Option<T>`, `ToString`, `Eq`, `Ord`, `Iterator`.
- **[Math](Math.md)** — Math library: trigonometry, log/exp, rounding, min/max, constants.
- **[Date](Date.md)** — Date library: current time, format, parse, getters, arithmetic.
- **[Array](Array.md)** — Array type and operations: length, insertAt, deleteAt, reduce, filter, for-in (built-in; no import).
- **[ArrayBuf](ArrayBuf.md)** — Binary buffer (JS/TS-style): arrayBufCreate, get/set, readFileBytes, writeFileBytes, Base64 (built-in; no import).
- **[String](String.md)** — String operations and regular expressions (built-in; no import).
- **[Stdio](Stdio.md)** — Standard I/O: print, println, readLine, readFile, writeFile (built-in; no import).

Libraries in `lib/` (Prelude, Math, Date) are found via the "lib next to binary" scheme (or `FIRST_LIB_PATH`). Import with `import "Prelude"`, `import "Math"`, or `import "Date"`. String and Stdio are built-in and always in scope.
