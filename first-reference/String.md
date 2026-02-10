# String

Built-in string operations and regular expressions. These functions are always in scope; no import is required.

---

## Length and slicing

| Function | Type | Description |
|----------|------|--------------|
| `stringLength(s)` | `String -> Int` | Number of characters |
| `stringConcat(s1, s2)` | `(String, String) -> String` | Concatenate two strings |
| `stringSlice(s, start, end)` | `(String, Int, Int) -> String` | Substring from index `start` (inclusive) to `end` (exclusive). Indices out of range are clamped. |

---

## Parsing

| Function | Type | Description |
|----------|------|--------------|
| `stringToInt(s)` | `String -> Int` | Parse decimal integer; returns 0 if invalid |
| `stringToFloat(s)` | `String -> Float` | Parse float; returns 0.0 if invalid |

---

## Conversion to String

| Function | Type | Description |
|----------|------|--------------|
| `intToString(n)` | `Int -> String` | Integer to decimal string |
| `floatToString(x)` | `Float -> String` | Float to string |
| `boolToString(b)` | `Bool -> String` | `"true"` or `"false"` |
| `unitToString(u)` | `Unit -> String` | `"()"` |

---

## Comparison

Strings also support operators `==`, `!=`, `<`, `<=`, `>`, `>=` (lexicographic order).

| Function | Type | Description |
|----------|------|--------------|
| `stringEquals(s1, s2)` | `(String, String) -> Bool` | Equality |
| `stringCompare(s1, s2)` | `(String, String) -> Int` | Returns `-1` if s1 < s2, `0` if equal, `1` if s1 > s2 |

---

## Regular expressions

Patterns use the C++ `std::regex` syntax.

| Function | Type | Description |
|----------|------|--------------|
| `regexMatches(str, pattern)` | `(String, String) -> Int` | Returns `1` if match, `0` if no match, `-1` on regex error |
| `regexSearch(str, pattern)` | `(String, String) -> Int` | Index of first match, or `-1` if not found |
| `regexReplace(str, pattern, replacement)` | `(String, String, String) -> String` | Replace first occurrence |
| `regexReplaceAll(str, pattern, replacement)` | `(String, String, String) -> String` | Replace all occurrences |
| `regexExtract(str, pattern, groupIndex)` | `(String, String, Int) -> String` | Extracts capturing group; `0` = whole match, `1` = first group, etc. |

Example:

```first
regexSearch("The year is 2024", "[0-9]+")        // 12
regexReplaceAll("foo bar foo", "foo", "baz")     // "baz bar baz"
regexExtract("Price: $42.50", "\\$([0-9.]+)", 1) // "42.50"
```
