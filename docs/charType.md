 

## Specification: The `Char` Primitive Type

### 1. Data Representation

* **Storage Size:** 32-bit (4-byte) fixed-width integer.
* **Encoding:** Internal representation must be **UTF-32**.
* **Range:** Valid Unicode Scalar Values only.
* Range 1:  to 
* Range 2:  to 


* **Constraint:** Values between  and  (surrogate pairs) are **invalid** as standalone characters and must throw a compile-time or runtime error if assigned.

### 2. Core Properties (Typeclasses)

To match Haskell's utility, your `Char` type should implement the following interfaces:

* **Equality (`Eq`):** Comparison based on the numerical Unicode code point.
* **Ordering (`Ord`):** Lexicographical order based on code point value ().
* **Enumeration (`Enum`):** * `succ(c)`: Returns the character at code point .
* `pred(c)`: Returns the character at code point .
* Support for ranges: `['a'..'z']`.



### 3. Required Primitive Functions

You should expose a core module (e.g., `Std.Char`) containing these low-level operations:

| Function | Logic |
| --- | --- |
| `to_int(Char)` | Returns the underlying 32-bit integer (Haskell's `ord`). |
| `from_int(Int)` | Validates the Int is within Unicode ranges and returns a `Char` (Haskell's `chr`). |
| `category(Char)` | Returns the Unicode General Category (e.g., Letter, Number, Punctuation). |

### 4. Categorization Predicates

Implement a suite of boolean checks using the Unicode Database (UCD):

* `is_alpha`: True if category is `L*` (Letter).
* `is_digit`: True if category is `Nd` (Decimal Number).
* `is_whitespace`: True if it matches Unicode space characters, tabs, or newlines.
* `is_upper` / `is_lower`: Check for casing properties.

### 5. String Integration

In Haskell, `String` is a linked list of characters: `[Char]`.

* **Recommendation:** If your language prioritizes performance, implement `String` as a **UTF-8 encoded byte array** (like Rust or Go), but provide an **Iterator** that yields your 32-bit `Char` type. This gives you Haskell's ease of use with modern memory efficiency.

---

### Implementation Note: Literal Syntax

To prevent confusion with strings, enforce the single-quote standard:

* `'A'` → `Char` (Literal)
* `'\u03BB'` → `Char` (Unicode escape for λ)
* `"A"` → `String` (Array/List of Chars)

---

### Implementation status (First)

**Done:**

- **Type:** `Char` primitive; 32-bit (i32) in IR; valid Unicode scalar only (0..0xD7FF, 0xE000..0x10FFFF); surrogates rejected at parse (literals) and in `fromInt` (runtime abort).
- **Literals:** `'A'`, `'\n'`, `'\t'`, `'\r'`, `'\\'`, `'\''`, `'"'`, `'\uXXXX'` (4 hex digits).
- **Eq / Ord:** Built-in (code-point comparison).
- **ToString:** Built-in; `toString(c)` → internal Char-to-string → UTF-8 string of one code point.
- **Primitive functions:**  
  - `toInt(c: Char) -> Int` (ord), `fromInt(i: Int) -> Char` (chr; invalid range aborts).  
  - `succ(c: Char) -> Char`, `pred(c: Char) -> Char` (Enum; skip surrogate range; abort at max/min).  
  - `category(c: Char) -> String` (returns "Letter", "Number", "Punctuation", "Space", "Other").  
  - `isAlpha`, `isDigit`, `isWhitespace`, `isUpper`, `isLower` (Char → Bool).  
  All exposed as stdlib (no separate Std.Char module).

**Not yet:**

- Range syntax `['a'..'z']` (language/expression feature).
- Dedicated `Std.Char` module (optional; current API is via Prelude/stdlib).

 