# Chapter 18: A JSON parser

This chapter builds a **small JSON parser** in First using **algebraic data types (ADTs)**, **pattern matching**, **recursion**, and **Option** for failure. The parser is pure (no I/O inside parsing) and shows how to structure a recursive-descent parser in a functional style.

Runnable code lives in **examples/chapter-18-A-JSON-parser**. Run it with:

```bash
cd examples/chapter-18-A-JSON-parser
fir run
```

---

## 1. Why a JSON parser?

JSON is a simple, widely used data format. Parsing it in First gives you:

- A **Json** type that models JSON values (null, bool, number, string, array, object).
- **Pattern matching** to handle each kind of value and to pretty-print or transform the tree.
- **Option&lt;ParseResult&gt;** to represent “parsed a value and the remaining string” or “parse failed,” without throwing.
- **Recursion** for nested arrays and objects and for consuming characters one step at a time.

No external libraries are required: we use the standard string functions (**strTrim**, **strStartsWith**, **strSlice**, **strLength**, **strCharAt**, **strIndexOf**) and **Prelude** for **Option**, **some**, and **none**.

---

## 2. The Json type (ADT)

We represent a JSON value as a **sum type**: every value is exactly one of the following variants.

```first
type Json =
    JNull
  | JBool(Bool)
  | JInt(Int)
  | JFloat(Float)
  | JString(String)
  | JArray(Array<Json>)
  | JObject(Array<{ key: String, value: Json }>);
```

- **JNull** — JSON `null`.
- **JBool(Bool)** — `true` or `false`.
- **JInt(Int)** / **JFloat(Float)** — numbers (we parse integers when there is no decimal point).
- **JString(String)** — double-quoted string (we support a minimal set of escapes).
- **JArray(Array&lt;Json&gt;)** — ordered list of JSON values.
- **JObject(Array&lt;{ key: String, value: Json }&gt;)** — key–value pairs (order preserved).

This is the same idea as in **Chapter 16 (Type reference)**: one type, several **constructors**, each with a payload. The compiler will force us to handle every variant when we **match** on a **Json** value.

---

## 3. Parse result: value + remaining string

Parsing is **pure**: we don’t mutate a global string. Instead, each parser step takes a string and returns either:

- **Some({ value: Json, rest: String })** — we parsed one value and the **rest** is the unconsumed input, or  
- **None** — parse failed.

So we use a small record and **Option**:

```first
type ParseResult = { value: Json, rest: String };

// Internal parsers return Option<ParseResult>
// e.g. parseValue(s) -> Some({ value: JNull, rest: " ..." }) or None
```

The top-level **parse(s)** trims the input, calls the value parser, and checks that nothing is left (so the whole string is valid JSON).

---

## 4. Top-level parse and dispatching

**parse(s: String) -> Option&lt;Json&gt;**

1. Trim whitespace from **s**.
2. Call **parseValue(trimmed)**.
3. If the result is **None**, return **None**.
4. If the result is **Some({ value, rest })**, trim **rest**; if **rest** is empty, return **Some(value)**; otherwise fail (trailing junk).

**parseValue(s)** dispatches by looking at the start of **s** (using **strStartsWith** and **strCharAt**):

- `"null"` → **parseNull**
- `"true"` / `"false"` → **parseBool**
- `'"'` → **parseString**
- `'['` → **parseArray**
- `'{'` → **parseObject**
- `'-'` or a digit → **parseNumber**
- else → **None**

So the “shape” of the input (first character or literal) selects which sub-parser runs. That’s **pattern matching on the input shape**, expressed with **if** and **strStartsWith** / **strCharAt**.

---

## 5. Parsing atoms: null, bool, number

- **parseNull(s)**  
  If **s** starts with **"null"**, return **Some({ value: JNull, rest: strSlice(s, 4, strLength(s)) })**; else **None**.

- **parseBool(s)**  
  If **s** starts with **"true"**, return **Some({ value: JBool(true), rest: ... })**; if **"false"**, return **JBool(false)**; else **None**.

- **parseNumber(s)**  
  Find the longest span that forms a number (optional minus, digits, optional decimal part, optional exponent). Use **strSlice** to cut that substring and **strToFloat** (or **strToInt** if no decimal) to get the value. Return **JFloat** or **JInt** and the remaining string.

Numbers are the fiddliest: we use a small recursive helper (e.g. **skipDigits(s, i)**) to advance an index **i** while the character is a digit, then handle **.**, **e**/ **E**, etc. All of this is pure: we only use **strCharAt**, **strLength**, and **strSlice**.

---

## 6. Parsing strings (quoted)

**parseString(s)** requires **s** to start with **'"'**:

1. Find the closing **'"'**, respecting **\\"** (escaped quote): e.g. recursive **findClosingQuote(s, i)** that skips **\\** and the next character, or stops at **'"'**.
2. The content is **strSlice(s, 1, closeIndex)**.
3. Optionally **unescape** the content (replace **\\n**, **\\t**, **\\"**, **\\\\**, etc.); for a minimal parser we can leave content as-is or implement a simple unescape.
4. Return **Some({ value: JString(content), rest: strSlice(s, closeIndex + 1, strLength(s)) })**.

So string parsing is another recursive walk over the input, using **strCharAt** and **strSlice**, and returns **Option&lt;ParseResult&gt;** to chain with the rest of the parser.

---

## 7. Parsing arrays and objects (recursive structure)

**parseArray(s)** (when **s** starts with **'['**):

1. Consume **'['** and trim.
2. If the next character is **']'**, return **Some({ value: JArray([]), rest: ... })** (empty array).
3. Otherwise parse one value with **parseValue**, then:
   - If the next (trimmed) token is **','**, consume it and **recursively** parse the rest of the elements.
   - Build the **Array&lt;Json&gt;** by prepending each new value: **insertAt(restArr, first, 0)** (or append with **insertAt(restArr, first, arrayLength(restArr))** and adjust order). Arrays are immutable, so we build a new array at each step.
4. When we see **']'**, wrap the collected array in **JArray** and return it with the remaining string.

**parseObject(s)** (when **s** starts with **'{'**):

1. Consume **'{'** and trim.
2. If the next character is **'}'**, return **Some({ value: JObject([]), rest: ... })**.
3. Otherwise parse a **key** (a quoted string), then **':'**, then a **value** via **parseValue**, then optionally **','** and more pairs.
4. Collect pairs into **Array&lt;{ key: String, value: Json }&gt;** the same way we built **JArray** (e.g. **insertAt** to add each pair).
5. When we see **'}'**, return **JObject(pairs)** and the rest.

In both cases, **recursion** handles nesting: **parseValue** can call **parseArray** or **parseObject**, which call **parseValue** again. The **Option** type propagates failure: if any step returns **None**, we return **None** without throwing.

---

## 8. Pretty-printing with pattern matching

Once we have a **Json** value, we can **match** on it to produce a string (e.g. for debugging or output):

```first
function jsonToString(j: Json) -> String {
    return match j {
        JNull => "null",
        JBool(b) => if (b) { "true" } else { "false" },
        JInt(n) => intToString(n),
        JFloat(x) => floatToString(x),
        JString(s) => "\"" + s + "\"",
        JArray(arr) => "[" + arrayOfJsonToString(arr) + "]",
        JObject(pairs) => "{" + objectPairsToString(pairs) + "}"
    };
}
```

Helper functions **arrayOfJsonToString** and **objectPairsToString** can use **reduce** or **for-in** over the array to concatenate comma-separated strings. This is a **structural recursion** over the **Json** type: every constructor is handled, and nested arrays/objects are handled by calling the same helpers recursively.

---

## 9. Summary

| Idea | How we use it |
|------|----------------|
| **ADT** | **Json** = JNull \| JBool \| JInt \| JFloat \| JString \| JArray \| JObject — one type, many variants. |
| **Pattern matching** | **match j { ... }** to inspect and transform every kind of JSON value; exhaustiveness is checked. |
| **Option** | Parsers return **Option&lt;ParseResult&gt;** (or **Option&lt;Json&gt;** at the top level) instead of throwing. |
| **Recursion** | **parseValue** → **parseArray** / **parseObject** → **parseValue**; **findClosingQuote** and **skipDigits** advance by recursing. |
| **Immutability** | We never mutate the input string; we pass **rest** and use **strSlice** to get substrings. |
| **Building arrays** | **insertAt(acc, value, index)** to build **Array&lt;Json&gt;** and **Array&lt;{ key, value }&gt;** step by step. |

This chapter ties together **ADTs** (Ch. 6, 16), **pattern matching**, **recursion** (Ch. 8), **Option** (Prelude), and **immutable arrays** (Ch. 10) into one small but complete parser you can extend (e.g. better number/string handling or error messages).
