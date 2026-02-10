# Prelude

The Prelude is the core standard library module. It provides common interfaces and the `Option` type used across the language.

**Import:** `import "Prelude"` or `import { Option, ToString, Eq } "Prelude"`

---

## Option&lt;T&gt;

A type that represents an optional value: either `Some(value)` or `None`.

- Used by APIs that may fail or have no value (e.g. `insertAt` / `deleteAt` return `Option<Array<T>>`).
- Pattern match with `match`: `match opt { Some(x) -> ... ; None -> ... }`.

---

## Interfaces

### ToString&lt;T&gt;

- **`toString(x: T) -> String`** — Convert a value to a string.

Built-in types `Int`, `Float`, `Bool`, `String` implement `ToString`. Use `#derive(ToString)` on records/ADTs or implement the interface manually.

### Eq&lt;T&gt;

- **`eq(a: T, b: T) -> Bool`** — Equality.

Built-in types implement `Eq`. Use `#derive(Eq)` or implement manually for your types.

### Ord&lt;T&gt;

- **`compare(a: T, b: T) -> Int`** — Comparison (negative / zero / positive for &lt; / = / &gt;).

`Int`, `Float`, `String` implement `Ord`. Use `#derive(Ord)` or implement manually.

### Iterator&lt;T&gt;

- **`hasNext(it: Iterator<T>) -> Bool`**
- **`next(it: Iterator<T>) -> T`**

`Array<T>` implements `Iterator<T>`, so `for x in arr { ... }` works. Other collection types can implement this interface.

---

## Built-in behaviour

- **Primitives** implement `ToString`, `Eq`; `Int`, `Float`, `String` also implement `Ord`.
- **Arrays** implement `Iterator` so `for-in` over arrays is supported.
