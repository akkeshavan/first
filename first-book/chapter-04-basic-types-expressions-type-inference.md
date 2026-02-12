# Chapter 4: Basic Types, Expressions, and Type Inference

First is statically typed: every expression has a type, and the compiler checks that types line up. This chapter covers the **basic types**, how **expressions** get their types, and **type inference**—when you can omit type annotations and let the compiler figure them out. Control flow (if expressions, for-in, ranges) is covered in Chapter 5.

---

## Basic types

First provides these built-in types for everyday use:

| Type    | Description              | Examples of values                    |
|--------|--------------------------|----------------------------------------|
| **Int**   | 64-bit signed integer    | `0`, `42`, `-7`                        |
| **Float** | 64-bit floating-point    | `0.0`, `3.14`, `-1.5e2`               |
| **Bool**  | Boolean                  | `true`, `false`                        |
| **Char**  | Single Unicode character (UTF-32 scalar) | `'A'`, `'\n'`, `'\u0041'`     |
| **String**| Text (UTF-8)             | `"hello"`, `"a\nb"`                    |
| **Unit**  | “No useful value”        | Result of `println(...)`; function returns nothing |

- **Int** and **Float** are used for numeric computation. Mixing them in an expression promotes **Int** to **Float** (e.g. `3 + 2.0` is **Float**).
- **Bool** is used in conditions and logical expressions.
- **Char** is a single Unicode code point (e.g. letters, digits, spaces). You can compare chars (**==**, **!=**, **<**, **<=**, **>**, **>=**) and convert to/from **Int** or **String** (see [The Char type](#the-char-type)).
- **String** is used for text and I/O (e.g. **print** / **println** take a **String**). You can concatenate with **+**.
- **Unit** is the type of expressions that only have side effects and no result you use (e.g. **println(...)** returns **Unit**).

---

## Literals and their types

**Literals** are values written directly in the source. Each literal has a fixed type:

```first
42        // Int
0         // Int
3.14      // Float
0.0       // Float
true      // Bool
false     // Bool
'A'       // Char (single quote)
'\n'      // Char (escape: \n \t \r \\ \' ")
"hello"   // String
"a\nb"    // String (escape sequences: \n \t \" \\)
```

- Integer literals (digits, optional minus) → **Int**.
- Float literals (digits, decimal point, optional exponent) → **Float**.
- **true** / **false** → **Bool**.
- Single-quoted **Char** literals: one character or escape (**\n**, **\t**, **\r**, **\\**, **\'**, **"**). **\uXXXX** (4 hex digits) gives a Unicode code point.
- Double-quoted strings → **String**; **\n**, **\t**, **\"**, **\\** are supported.

The compiler uses these to **infer** the type of an expression when you don’t write it explicitly.

---

## Expressions and operator types

Expressions are built from literals, variables, function calls, and operators. The type of an expression is determined by its parts.

### Arithmetic (Int, Float, String)

| Operator | Meaning   | Types (left, right)     | Result   |
|----------|-----------|--------------------------|----------|
| **+**    | add       | Int, Int                 | Int      |
| **+**    | add       | Float, Float (or Int+Float) | Float |
| **+**    | concatenate | String, String        | String   |
| **-** **\*** **/** | subtract, multiply, divide | Int or Float | Int or Float |
| **%**    | remainder | Int, Int                 | Int      |

Examples:

```first
1 + 2           // Int: 3
3.14 + 0.86     // Float: 4.0
1 + 2.0         // Float (Int promoted)
"Hello, " + name + "!"   // String concatenation
10 % 3          // Int: 1
```

### Comparison (any comparable types → Bool)

| Operator | Meaning        | Supported types        |
|----------|----------------|------------------------|
| **==**   | equal          | Int, Float, Char, String     |
| **!=**   | not equal      | Int, Float, Char, String     |
| **<**    | less than      | Int, Float, Char, String     |
| **<=**   | less or equal  | Int, Float, Char, String     |
| **>**    | greater than   | Int, Float, Char, String     |
| **>=**   | greater or equal | Int, Float, Char, String   |

Both operands must be the same (or compatible) type. The result is **Bool**.

**Numeric comparison** (Int / Float):

```first
x == 0       // Bool
n >= 1       // Bool
1.5 < 2.0    // Bool: true
```

**Char comparison**: For **Char** operands, comparison is by Unicode code point: `'A' < 'B'`, `'a' == 'a'`, etc.

**String comparison**: For **String** operands, all six operators perform **lexicographic** (dictionary) comparison. Characters are compared by their UTF-8 byte order; shorter strings are less than longer strings if one is a prefix of the other.

```first
"hello" == "hello"    // true
"hello" != "world"    // true
"apple" < "banana"   // true (a < b)
"apple" <= "apple"   // true
"zebra" > "apple"    // true
"zebra" >= "apple"   // true
```

### Logical (Bool → Bool)

| Operator | Meaning   |
|----------|-----------|
| **&&**   | and (short-circuit) |
| **\|\|** | or (short-circuit)  |

Operands must be **Bool**; the result is **Bool**:

```first
x > 0 && x < 10
done || error
```

### Unary

| Operator | Meaning   | Operand   | Result   |
|----------|-----------|-----------|----------|
| **-**    | negation  | Int or Float | Int or Float |
| **!**    | logical not | Bool   | Bool     |

```first
-x        // Int or Float
!flag     // Bool
```

---

## The Char type

**Char** represents a single Unicode character (one UTF-32 scalar value). Valid code points exclude surrogates (0xD800..0xDFFF); literals and **fromInt** enforce this.

### Char literals

Use single quotes. Escapes: **\n**, **\t**, **\r**, **\\**, **\'**, **"**. For a code point by value use **\uXXXX** (exactly 4 hex digits).

```first
'A'       // Char
'\n'      // newline
'\u0041'  // 'A' by code point
```

### Char conversion and utilities

These built-in functions are always available (no import):

| Function | Signature | Explanation |
|----------|-----------|-------------|
| **toInt** | `(c: Char) -> Int` | Code point of the character (e.g. `toInt('A')` → 65). |
| **fromInt** | `(i: Int) -> Char` | Character for a code point; aborts if out of valid Unicode scalar range. |
| **toString** | (built-in) | Converts **Char** to **String** (e.g. for **println** or concatenation). |
| **succ** | `(c: Char) -> Char` | Next character in code-point order; skips surrogate range; aborts at U+10FFFF. |
| **pred** | `(c: Char) -> Char` | Previous character; skips surrogates; aborts at U+0000. |
| **category** | `(c: Char) -> String` | General category: **"Letter"**, **"Number"**, **"Punctuation"**, **"Space"**, or **"Other"**. |
| **isAlpha** | `(c: Char) -> Bool` | True if the character is a letter. |
| **isDigit** | `(c: Char) -> Bool` | True if a decimal digit. |
| **isWhitespace** | `(c: Char) -> Bool` | True for space, tab, newline, and other Unicode whitespace. |
| **isUpper** | `(c: Char) -> Bool` | True if uppercase. |
| **isLower** | `(c: Char) -> Bool` | True if lowercase. |

Example:

```first
toInt('A');           // 65
fromInt(65);          // 'A'
toString('x');        // "x"
succ('a');            // 'b'
category('9');       // "Number"
isAlpha('K');        // true
isDigit('5');        // true
isWhitespace(' ');   // true
```

---

## String operators and built-in string functions

This section summarizes **string-specific** operators and the built-in functions that work on or produce strings.

### String operators

| Operator | Name           | Types        | Result   | Explanation |
|----------|----------------|-------------|----------|-------------|
| **+**    | concatenate    | String, String | String | Joins two strings: `"Hello, " + "World"` → `"Hello, World"`. |
| **==**   | equal          | String, String | Bool   | True only if both strings are identical. |
| **!=**   | not equal      | String, String | Bool   | True if the strings differ. |
| **<**    | less than      | String, String | Bool   | Lexicographic order: `"apple" < "banana"`. |
| **<=**   | less or equal  | String, String | Bool   | Less than or equal in lexicographic order. |
| **>**    | greater than   | String, String | Bool   | Lexicographic order: `"zebra" > "apple"`. |
| **>=**   | greater or equal | String, String | Bool | Greater than or equal in lexicographic order. |

**Concatenation** is the only string operator that *produces* a string; the others produce **Bool**. You can chain **+** to build longer strings: `"a" + "b" + "c"` is `"abc"`.

### Built-in string functions

These functions are always available (no import).

| Function | Signature | Explanation |
|----------|-----------|-------------|
| **intToString** | `(n: Int) -> String` | Converts an integer to its decimal string, e.g. `intToString(42)` → `"42"`. |
| **floatToString** | `(x: Float) -> String` | Converts a float to a string, e.g. `floatToString(3.14)` → `"3.140000"` (format may vary). |
| **strEquals** | `(s1: String, s2: String) -> Bool` | Same as `s1 == s2`; true if the two strings are equal. |
| **strCompare** | `(s1: String, s2: String) -> Int` | Lexicographic comparison: returns **-1** if `s1 < s2`, **0** if `s1 == s2`, **1** if `s1 > s2`. Useful for sorting or custom ordering. |

Example:

```first
let a = "apple";
let b = "banana";
strEquals(a, b);      // false
strCompare(a, b);     // -1
"Sum: " + intToString(10 + 20);   // "Sum: 30"
```

---

## Regular expression functions

First provides **regular expression** support via built-in functions. Patterns use the same syntax as C++ `std::regex` (ECMAScript-style). These functions are always available (no import).

### regexMatches

**Signature:** `(str: String, pattern: String) -> Int`

**Explanation:** Tests whether the **entire** string `str` matches the regex `pattern`.

- Returns **1** if the whole string matches.
- Returns **0** if it does not match.
- Returns **-1** if the pattern is invalid (regex error).

```first
regexMatches("hello123", "[a-z]+[0-9]+");   // 1 (match)
regexMatches("hello", "[0-9]+");            // 0 (no match)
```

### regexSearch

**Signature:** `(str: String, pattern: String) -> Int`

**Explanation:** Finds the **first** occurrence of `pattern` in `str`.

- Returns the **index** (0-based) where the match starts.
- Returns **-1** if no match is found or the pattern is invalid.

```first
regexSearch("The year is 2024", "[0-9]+");   // 12 (start of "2024")
regexSearch("no digits", "[0-9]+");           // -1
```

### regexReplace

**Signature:** `(str: String, pattern: String, replacement: String) -> String`

**Explanation:** Replaces the **first** occurrence of `pattern` in `str` with `replacement`. Returns a new string; the original is unchanged.

```first
regexReplace("hello world", "world", "First");   // "hello First"
```

### regexReplaceAll

**Signature:** `(str: String, pattern: String, replacement: String) -> String`

**Explanation:** Replaces **every** occurrence of `pattern` in `str` with `replacement`. Returns a new string.

```first
regexReplaceAll("foo bar foo", "foo", "baz");   // "baz bar baz"
```

### regexExtract

**Signature:** `(str: String, pattern: String, groupIndex: Int) -> String`

**Explanation:** Searches for the first match of `pattern` in `str` and returns the substring for the given **capturing group**.

- **groupIndex 0**: the whole match.
- **groupIndex 1, 2, ...**: the first, second, ... parenthesized group in the pattern.

Returns the matched substring as a new string, or an empty string if there is no match or the group index is invalid.

```first
regexExtract("Price: $42.50", "\\$([0-9.]+)", 1);   // "42.50"
regexExtract("Date: 2024-01-15", "([0-9]{4})-([0-9]{2})-([0-9]{2})", 2);   // "01"
```

---

## Type inference

The compiler **infers** the type of an expression from literals, operators, and function signatures. You don’t have to annotate every expression.

### Literals and operators

From a literal, the type is fixed (e.g. **42** is **Int**). From an operator, the type is derived from the operands (e.g. **Int + Int → Int**, **String + String → String**). So the type of **3 + 4** is inferred as **Int**, and **"a" + "b"** as **String**.

### Variables: optional type annotation

In **let** and **var** declarations you can omit the type; the compiler infers it from the initializer:

```first
let x = 42;           // x is Int
let pi = 3.14;        // pi is Float
let name = "First";   // name is String
let ok = true;        // ok is Bool
```

You can still write the type explicitly. The compiler then **checks** that the initializer matches:

```first
let x: Int = 42;      // ok
let n: Int = 7 * 6;   // ok: 42 is Int
let bad: Int = 3.14;  // error: type mismatch
```

If you omit both the type and the initializer, the compiler cannot infer the type and will report an error. So for inference you need an initializer.

### Function parameters and return type

Function and interaction **parameters** and **return types** are always annotated in First (e.g. **function add(x: Int, y: Int) -> Int**). The compiler uses those to type-check the body and any call sites. So “type inference” in this chapter mainly refers to **local variables** and **expressions**, not to inferring parameter or return types.

---

## Runnable example: chapter-04-basic-expressions-types

The project **examples/chapter-04-basic-expressions-types** defines one pure function per expression kind and **interaction main()** calls each and prints the result:

- **Arithmetic (Int)**: **addInt**, **subInt**, **mulInt**, **divInt**, **modInt**
- **Arithmetic (Float)**: **addFloat**, **mulFloat**
- **String**: **concatStrings** (uses **+**)
- **String comparison**: **eqString**, **neString**, **ltString**, **geString** (uses operators)
- **Comparison**: **eqInt**, **neInt**, **ltInt**, **geInt** (return **Bool**)
- **Char**: **toInt**, **fromInt**, **succ**, **pred**, **category**, **isAlpha** (and related predicates)
- **Logical**: **andBool**, **orBool**
- **Unary**: **negateInt**, **negateFloat**, **notBool**
- **Regex**: demonstrations of **regexMatches** and **regexSearch**

From the repo root:

```bash
cd examples/chapter-04-basic-expressions-types
fir run
```

You’ll see one line per expression (e.g. `addInt(10, 3) = 13`, `eqInt(5, 5) = true`).

---

## Putting it together

A short example using basic types, expressions, and inference:

```first
interaction main() -> Unit {
    let a = 10;
    let b = 20;
    let sum = a + b;                    // Int
    let msg = "Sum is " + intToString(sum);  // String
    println(msg);

    let x = 3.14;
    let y = 2.0;
    let product = x * y;                // Float
    println("Product: " + floatToString(product));

    let flag = true;
    let ok = flag && (sum > 0);         // Bool
    if (ok) {
        println("OK");
    }
}
```

- **a**, **b**, **sum** are inferred as **Int**; **msg** as **String**.
- **x**, **y**, **product** are inferred as **Float**.
- **flag**, **ok** are inferred as **Bool**.
- **intToString** / **floatToString** convert numbers to **String** for concatenation and **println**.

---

## Summary

1. **Basic types**: **Int**, **Float**, **Bool**, **Char**, **String**, **Unit**—used for numbers, booleans, single characters, text, and “no value.”
2. **Literals** have fixed types; the compiler uses them to infer expression types.
3. **Expressions**: arithmetic (**+** **-** **\*** **/** **%**), comparison (**==** **!=** **<** **<=** **>** **>=** on Int, Float, Char, and String), logical (**&&** **||**), unary (**-** **!**), and string **+** (concatenation); result types follow the rules above.
4. **Char**: single-quoted literals and **\uXXXX**; **toInt** / **fromInt** / **toString**; **succ** / **pred**; **category** and **isAlpha** / **isDigit** / **isWhitespace** / **isUpper** / **isLower**. Chars support **==**, **!=**, **<**, **<=**, **>**, **>=** by code point.
5. **String operators**: **+** concatenates strings; **==** **!=** **<** **<=** **>** **>=** compare strings lexicographically and return **Bool**. Built-in **strEquals** and **strCompare** give explicit comparison.
6. **Regular expressions**: **regexMatches**, **regexSearch**, **regexReplace**, **regexReplaceAll**, and **regexExtract** provide pattern matching, search, replace, and capturing-group extraction.
7. **Type inference**: in **let** / **var**, you can omit the type and the compiler infers it from the initializer; you can still add a type for checking.
8. Function parameters and return types are always annotated; inference applies to locals and expressions.

Control flow and the **for-in** loop over ranges are covered in **Chapter 5**.

---

## Try it

- Run **examples/chapter-04-basic-expressions-types** with `fir run` and edit **src/main.first** to add more calls (e.g. **divInt(20, 4)**, **notBool(false)**).
- In **examples/chapter-02-functions-and-interactions**, add **let** bindings with and without type annotations (e.g. **let n = 7;** and **let n: Int = 7;**), and use **+**, **\***, **==**, **&&** in expressions.
- Use **floatToString** with a **Float** variable and **println** to see Float inference and conversion.
