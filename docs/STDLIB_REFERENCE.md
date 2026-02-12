## Standard Library Functions (Runtime)

This document lists the **built‑in stdlib functions** that are available **without importing any module**. These are implemented in the C++ runtime and wired directly by the compiler.

Types used below:
- **Int**: 64‑bit integer
- **Float**: 64‑bit floating point
- **Bool**: boolean
- **String**: immutable UTF‑8 string
- **Unit**: `()` (no meaningful value)
- **Array<T>**: immutable array of `T`
- **ArrayBuf**: mutable byte buffer (interaction‑only)

> **Note**: Some operations are **restricted to interactions** (because they touch I/O, time, or mutable buffers). These are marked as **interaction‑only**.

---

## I/O and Files

- **print(s: String) -> Unit**  
  Prints `s` to stdout (no newline).

- **println(s: String) -> Unit**  
  Prints `s` to stdout followed by a newline.

- **sleep(ms: Int) -> Unit** *(interaction‑only)*  
  Blocks the **current thread** for `ms` milliseconds. Used to simulate latency or delay execution.

- **readLine() -> String**  
  Reads a single line from stdin (without the trailing newline).

- **readFile(filename: String) -> String**  
  Reads the entire contents of a text file into a `String`.  
  On failure it currently reports an error via the runtime (no `Result` type yet at this layer).

- **writeFile(filename: String, content: String) -> Unit**  
  Writes `content` to `filename`, overwriting any existing file.

---

## Math (Float)

All of these take and/or return **Float**:

- **sin(x: Float) -> Float**
- **cos(x: Float) -> Float**
- **tan(x: Float) -> Float**
- **sqrt(x: Float) -> Float**
- **abs(x: Float) -> Float**
- **floor(x: Float) -> Float**
- **ceil(x: Float) -> Float**
- **exp(x: Float) -> Float**
- **log(x: Float) -> Float**  (natural log)
- **log10(x: Float) -> Float**
- **round(x: Float) -> Float**
- **sign(x: Float) -> Float**  (`-1.0`, `0.0`, or `1.0`)

Binary float functions:

- **pow(base: Float, exp: Float) -> Float**
- **min(x: Float, y: Float) -> Float**
- **max(x: Float, y: Float) -> Float**

Integer helpers:

- **minInt(x: Int, y: Int) -> Int**
- **maxInt(x: Int, y: Int) -> Int**

Math constants:

- **pi() -> Float**
- **e() -> Float**

---

## Date / Time

Dates are represented as an **opaque Int handle**. You should treat the `Int` as a token; only the functions below should operate on it.

- **now() -> Int**  
  Returns the current date‑time as an opaque handle.

- **format(d: Int, fmt: String) -> String**  
  Formats `d` using a C‑style format string (e.g. `"%Y-%m-%d %H:%M:%S"`).

- **parse(s: String) -> Int**  
  Parses a date string into an opaque handle (same representation as `now()`).

- **getYear(d: Int) -> Int**  
- **getMonth(d: Int) -> Int**
- **getDay(d: Int) -> Int**
- **getHours(d: Int) -> Int**
- **getMinutes(d: Int) -> Int**
- **getSeconds(d: Int) -> Int**

- **addSeconds(d: Int, seconds: Int) -> Int**  
  Returns a new date handle offset by `seconds`.

---

## Binary I/O and ArrayBuf

`ArrayBuf` is a **mutable byte buffer**, only allowed inside **interactions**.

- **arrayBufCreate(length: Int) -> ArrayBuf** *(interaction‑only)*  
  Allocates a new buffer of `length` bytes.

- **arrayBufLength(buf: ArrayBuf) -> Int**  
  Returns the buffer length in bytes.

- **arrayBufGet(buf: ArrayBuf, index: Int) -> Int**  
  Reads the byte at `index` (0‑based) as an `Int` in range \[0, 255\].

- **arrayBufSet(buf: ArrayBuf, index: Int, value: Int) -> Unit**  
  Writes `value` (low 8 bits) into `buf[index]`.

- **readFileBytes(filename: String) -> ArrayBuf** *(interaction‑only)*  
  Reads an entire file as raw bytes into an `ArrayBuf`.

- **writeFileBytes(filename: String, data: ArrayBuf) -> Unit**  
  Writes the buffer contents to a file.

- **base64Encode(buf: ArrayBuf) -> String**  
  Returns a Base64 string for the given bytes.

- **base64Decode(s: String) -> ArrayBuf** *(interaction‑only)*  
  Decodes Base64 text into bytes.

- **arrayBufToString(buf: ArrayBuf) -> String**  
  Interprets the bytes as UTF‑8 and returns a `String`.

---

## String utilities and conversions

- **strLength(s: String) -> Int**
- **strConcat(s1: String, s2: String) -> String**
- **strSlice(s: String, start: Int, end: Int) -> String**

Character / code (index is 0‑based; returns **-1** if out of range):

- **strCharAt(s: String, index: Int) -> Int**  
  Returns the byte at `index` (0–255), or -1 if out of bounds.

- **strCodePointAt(s: String, index: Int) -> Int**  
  Returns the UTF‑8 code point starting at `index`, or -1 if out of bounds.

Search (use `0` for “from start” or “position” where not needed):

- **strIndexOf(s: String, search: String, from: Int) -> Int**  
  Index of first occurrence of `search` in `s` at or after `from`; **-1** if not found.

- **strLastIndexOf(s: String, search: String, from: Int) -> Int**  
  Index of last occurrence of `search` in `s` at or before `from`; **-1** if not found.

Boolean checks (use `0` for “from start” / “full string”):

- **strIncludes(s: String, search: String, position: Int) -> Bool**
- **strStartsWith(s: String, search: String, position: Int) -> Bool**
- **strEndsWith(s: String, search: String, endPosition: Int) -> Bool**  
  For `strEndsWith`, `endPosition` is the length to consider (e.g. use `strLength(s)` for full string).

Case and whitespace:

- **strToLower(s: String) -> String**
- **strToUpper(s: String) -> String**
- **strTrim(s: String) -> String**  
  Trims ASCII whitespace from both ends.
- **strTrimStart(s: String) -> String**
- **strTrimEnd(s: String) -> String**

Padding and repeat:

- **strPadStart(s: String, targetLength: Int, padString: String) -> String**  
  Pads from the start so the result length is at least `targetLength`; `padString` is repeated as needed (defaults to space if empty).
- **strPadEnd(s: String, targetLength: Int, padString: String) -> String**
- **strRepeat(s: String, count: Int) -> String**  
  Repeats `s` `count` times; returns empty string if `count` ≤ 0.

Unicode:

- **strNormalize(s: String, form: String) -> String**  
  Normalization form (`"NFC"`, `"NFD"`, `"NFKC"`, `"NFKD"`). Currently returns a copy of `s` (full Unicode normalization may be added later).

Parsing:

- **strToInt(s: String) -> Int**
- **strToFloat(s: String) -> Float**

Formatting / conversions:

- **intToString(x: Int) -> String**
- **floatToString(x: Float) -> String**
- **boolToString(x: Bool) -> String**
- **unitToString(x: Unit) -> String**

Interface‑based:

- **toString(x: T) -> String**  
  Works when `T` implements the `ToString` interface. The compiler will resolve the correct implementation.

---

## String comparison and Regular Expressions

Basic comparison:

- **strEquals(s1: String, s2: String) -> Bool**
- **strCompare(s1: String, s2: String) -> Int**  
  Returns `< 0`, `0`, or `> 0` depending on lexicographic ordering.

Regex functions (see `docs/STRING_COMPARISON_AND_REGEX.md` for details):

- **regexMatches(str: String, pattern: String) -> Int**  
  Returns `1` (match), `0` (no match), or `-1` (error).

- **regexSearch(str: String, pattern: String) -> Int**  
  Returns the first match index or `-1` if not found.

- **regexReplace(str: String, pattern: String, replacement: String) -> String**

- **regexReplaceAll(str: String, pattern: String, replacement: String) -> String**

- **regexExtract(str: String, pattern: String, groupIndex: Int) -> String**  
  Returns the given capture group or an empty string if no match.

---

## Arrays

Arrays are **immutable**. These functions are available in scope (Prelude or built-in); see **first-reference/Array.md** and **first-book/chapter-10-array-functions.md** for usage.

**Length and indexing:**

- **arrayLength(a: Array<T>) -> Int**  
  Returns the number of elements.

**Insert and delete** (return **Option&lt;Array&lt;T&gt;&gt;**; use **match** for `Some` / `None`):

- **insertAt<T>(a: Array<T>, value: T, position: Int) -> Option<Array<T>>**  
  Returns a new array with `value` inserted at `position`, or `None` if the index is out of range (position &lt; 0 or &gt; length).

- **deleteAt<T>(a: Array<T>, position: Int) -> Option<Array<T>>**  
  Returns a new array with the element at `position` removed, or `None` if the index is out of range.

**Reduce (fold):**

- **reduce<T, U>(a: Array<T>, init: U, f: function(U, T) -> U) -> U**  
  Fold left: start with `init`, then apply `f(acc, cur)` over elements left to right. Example: sum = `reduce(a, 0, function(acc: Int, cur: Int) -> Int { return acc + cur; })`.

- **reduceRight<T, U>(a: Array<T>, init: U, f: function(T, U) -> U) -> U**  
  Fold right: start with `init`, then apply `f(cur, acc)` from right to left.

**Filter:**

- **filter<T>(a: Array<T>, p: function(T) -> Bool) -> Array<T>**  
  Returns a new array containing only elements for which `p(item)` is true.

**Map and reverse:**

- **map<T, U>(a: Array<T>, f: function(T) -> U) -> Array<U>**  
  Returns a new array where each element is `f(x)` for `x` in `a`, in the same order.

- **reverse<T>(a: Array<T>) -> Array<T>**  
  Returns a new array with the elements of `a` in reverse order.

**Iteration:** `Array<T>` implements **Iterator&lt;T&gt;** (Prelude), so you can use **`for x in arr { ... }`**.

---

## Networking: Sockets

Low‑level TCP socket helpers:

- **socketConnect(hostPort: String, timeoutMs: Int) -> Int**  
  Opens a connection and returns a socket handle (Int) or a negative error code.

- **socketSend(sock: Int, data: String) -> Int**  
  Sends data, returns number of bytes written or an error code.

- **socketRecv(sock: Int) -> String**  
  Receives data from the socket as a `String`.

- **socketClose(sock: Int) -> Unit**

---

## HTTP Client and Server

HTTP client:

- **httpGet(url: String) -> String**
- **httpPost(url: String, body: String) -> String**

- **httpRequest(method: String, url: String, headersJson: String, queryJson: String, paramsJson: String, body: String) -> Int**  
  Lower‑level request helper returning an opaque handle/status code (internal details may evolve).

HTTP server:

- **httpServerCreate(host: String, port: Int) -> Int**  
  Creates a server and returns a server handle.

- **httpServerGet(server: Int, path: String, handlerId: Int) -> Unit**
- **httpServerPost(server: Int, path: String, handlerId: Int) -> Unit**

- **httpServerListen(server: Int) -> Unit**
- **httpServerClose(server: Int) -> Unit**

Request helpers (from within a handler):

- **httpReqMethod(req: Int) -> String**
- **httpReqPath(req: Int) -> String**
- **httpReqParamsJson(req: Int) -> String**
- **httpReqQueryJson(req: Int) -> String**
- **httpReqHeadersJson(req: Int) -> String**
- **httpReqBody(req: Int) -> String**

Response helpers:

- **httpResponseCreate(status: Int, headersJson: String, body: String) -> Int**
- **httpRespStatus(resp: Int) -> Int**
- **httpRespHeadersJson(resp: Int) -> String**
- **httpRespBody(resp: Int) -> String**

---

## JSON helpers

- **jsonPrettify(json: String) -> String**  
  Formats JSON with indentation and newlines.

- **jsonStringifyString(s: String) -> String**
- **jsonStringifyInt(x: Int) -> String**
- **jsonStringifyFloat(x: Float) -> String**  
  JSON‑encodes the given primitive value as a JSON string.

