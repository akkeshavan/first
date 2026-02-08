# Chapter 9: Interfaces

This folder explains **interfaces** in First: what they are, the **standard interfaces** (ToString, Eq, Ord, Iterator), how to **derive** them for your types, how to write **custom implementations**, and how to **call** interface methods (including when you have custom implementations).

The runnable source is **src/main.first** (uses **derive** and built-in ToString; custom implementation syntax is explained in the text).

---

## 1. What is an interface?

An **interface** is a contract: it names a type parameter and one or more **operations** (functions) that any type implementing the interface must provide. In First, interfaces are defined in the **Prelude** (and can be extended in your own code). They let you:

- Write **generic code** that works for any type that supports certain operations (e.g. “convert to string”, “compare for equality”).
- Use **constrained type parameters** like `T : ToString` or `T : Eq` so the type checker knows that `T` has those operations.

**Example idea:** “A function that prints any value” only needs a way to turn that value into a string. So we say: “for any type `T` that implements **ToString** (i.e. has a `toString` function), we can call `toString(x)` and then print the result.”

---

## 2. Standard interfaces (Prelude)

The **Prelude** module defines these interfaces. You get them when your program is compiled with the standard library (e.g. via `FIRST_LIB_PATH` pointing at `lib/`).

### ToString&lt;T&gt;

- **Method:** `toString: function(T) -> String`
- **Meaning:** “Turn a value of type `T` into a `String`.”
- **Built-in implementations:** `Int`, `Float`, `Bool`, `String`, and `Unit` already implement ToString. You can use `toString(42)`, `toString(3.14)`, `toString(true)`, `toString("hi")`, etc., without writing any implementation yourself.
- **Use:** Printing, logging, debugging, or any generic function that needs to show values of type `T`.

### Eq&lt;T&gt;

- **Method:** `eq: function(T, T) -> Bool`
- **Meaning:** “Compare two values of type `T` for equality.”
- **Relationship to operators:** The **`==`** and **`!=`** operators are defined in terms of Eq. For any type that implements Eq:
  - **`a == b`** means **`eq(a, b)`**
  - **`a != b`** means **`not eq(a, b)`**
- **Built-in implementations:** `Int`, `Float`, `Bool`, and `String` implement Eq. You can use `==` and `!=` for these types (and for any type with a custom `implementation Eq<MyType> { eq = ... }`).
- **Use:** Constrained generics like `function elem<T:Eq>(x: T, xs: List<T>) -> Bool`; the compiler requires `T : Eq` when you use `==` or `!=` on values of type `T`.

### Ord&lt;T&gt;

- **Method:** `compare: function(T, T) -> Int`
- **Meaning:** “Compare two values; returns negative, zero, or positive (like a comparator).”
- **Relationship to operators:** The **`<`**, **`<=`**, **`>`**, and **`>=`** operators are defined in terms of Ord. For any type that implements Ord:
  - **`a < b`** means **`compare(a, b) < 0`**
  - **`a <= b`** means **`compare(a, b) <= 0`**
  - **`a > b`** means **`compare(a, b) > 0`**
  - **`a >= b`** means **`compare(a, b) >= 0`**
- **Built-in implementations:** `Int`, `Float`, and `String` implement Ord (Bool does not).
- **Use:** Sorting, min/max, or any generic code that needs to order values of type `T`. (Custom derivation of Ord is reserved for future implementation.)

### Iterator&lt;T&gt;

- **Methods:** `hasNext: function(Iterator<T>) -> Bool`, `next: function(Iterator<T>) -> T`
- **Meaning:** “Sequence of values of type `T` that can be traversed.”
- **Use:** The `for`-in loop over arrays uses Iterator; `Array<T>` implements `Iterator<T>` so you can iterate over array elements.

---

## 3. How to derive an interface

For **record types** (single-constructor types with named fields), you can **derive** an implementation so you don’t have to write it by hand.

### Syntax

Place **`derive(InterfaceName)`** (or **`derive(InterfaceName1, InterfaceName2)`**) immediately before the type declaration:

```first
derive(ToString) type Point = { x: Float, y: Float };
```

- The compiler generates a **helper function** (e.g. `toString_Point`) that converts a `Point` to a string by converting each field with `toString` and joining them (e.g. `"{ x = 1.5, y = 2.5 }"`).
- It also adds an **implementation** `ToString<Point>` that binds `toString` to that helper.

So after the declaration above, you can call **`toString(p)`** for any `p : Point` without writing any implementation yourself.

### What can be derived today

- **ToString** — supported for **non-generic record types**. The derived implementation formats the record as `"{ field1 = value1, field2 = value2 }"`, where each value is converted with `toString` (so field types must implement ToString).
- **Eq** and **Ord** — listing them in `derive(ToString, Eq, Ord)` is accepted by the parser, but **only ToString is generated**; Eq and Ord derivation are reserved for future implementation.

### Example (from src/main.first)

```first
derive(ToString) type Point = { x: Float, y: Float };

interaction main() {
    let p: Point = { x: 1.5, y: 2.5 };
    println(toString(p));   // prints: { x = 1.500000, y = 2.500000 }
}
```

You can derive ToString for multiple record types in the same file; each gets its own generated implementation.

---

## 4. How to create custom implementations

When you need full control (or the type is not a simple record), you can write an **implementation** block by hand.

### Syntax

```first
implementation InterfaceName<TypeName> {
    memberName = value;
    ...
}
```

- **InterfaceName** — e.g. `ToString`, `Eq`, `Ord`.
- **TypeName** — the type that implements the interface (e.g. `Int`, `MyRecord`, or a type alias).
- **memberName** — must match a method of the interface (e.g. `toString` for ToString, `eq` for Eq).
- **value** — usually a **function** that matches the interface’s signature, or the name of a function that does.

### Custom ToString example

If you don’t use `derive(ToString)` and want a custom string format:

```first
type Point = { x: Float, y: Float };

function pointToString(p: Point) -> String {
    return "Point(" + floatToString(p.x) + ", " + floatToString(p.y) + ")";
}

implementation ToString<Point> {
    toString = pointToString;
}
```

Now **`toString(p)`** calls `pointToString(p)` and returns strings like `"Point(1.5, 2.5)"`.

### Custom implementation with an inline function

You can also give a function literal (if the language supports it in implementation blocks) or any expression that has the right type. The key is that the **member’s type** must match the interface method’s type (e.g. `function(Point) -> String` for `toString`).

*Note:* The parser and type checker accept custom `implementation ToString<MyType> { toString = myFunc; }`. The code generator currently resolves `toString(x)` for user types to a single generated/derived-style function per type; custom bindings to differently named functions may not be fully wired end-to-end yet. The runnable example in **src/main.first** uses **derive(ToString)** so it builds and runs reliably.

---

## 5. How to call interface methods (including custom implementations)

Once a type implements an interface (either by **built-in**, **derive**, or **custom implementation**), you call the interface method like any other function: **by name, with the appropriate arguments**.

### ToString

- **Call:** `toString(x)` or **`x.toString()`** where `x` has a type that implements ToString.
- **Result:** A `String`. You can pass it to **`print`** or **`println`**:
  - `println(toString(42));`        // built-in
  - `println(myPoint.toString());`  // or toString(myPoint) — derived or custom

So **how to call when you have a custom implementation:** you don’t call the implementation directly; you call **`toString(x)`**. The compiler resolves the call to the right implementation (built-in, derived, or your custom one) based on the type of `x`.

### Eq

- **Call:** For built-in types you usually use **`==`** and **`!=`** in expressions. For generic code, the type checker requires `T : Eq` when you use equality on `T`; the compiler then uses the `eq` implementation for that type.
- Custom implementations: **`implementation Eq<MyType> { eq = myEqFunc; }`** — then when you write `a == b` for `a,b : MyType`, the compiler uses `myEqFunc`.

### Ord

- **Call:** Typically used via comparison operators or sorting helpers that use `compare` under the hood. Custom: **`implementation Ord<MyType> { compare = myCompareFunc; }`** — then code that compares `MyType` uses your function.

### Eq, Ord and the comparison operators (summary)

| Interface | Method      | Operators that use it | Meaning in terms of the method        |
|-----------|-------------|------------------------|----------------------------------------|
| **Eq**    | `eq(a, b)`  | **`==`**, **`!=`**     | `a == b` → `eq(a,b)`; `a != b` → `not eq(a,b)` |
| **Ord**   | `compare(a, b)` | **`<`**, **`<=`**, **`>`**, **`>=`** | `a < b` → `compare(a,b) < 0`; `a <= b` → `compare(a,b) <= 0`; similarly for `>` and `>=` |

When you provide **`implementation Eq<MyType> { eq = myEq; }`** or **`implementation Ord<MyType> { compare = myCompare; }`**, the compiler uses your functions whenever code writes **`==`**, **`!=`**, or **`<`**, **`<=`**, **`>`**, **`>=`** on that type. You do not call `eq` or `compare` yourself unless you want to (e.g. for sorting by passing `compare` to a sort function).

### Custom Eq and Ord for Point

You can implement **Eq** and **Ord** for your own types so **`==`**, **`!=`**, and **`<`**, **`<=`**, **`>`**, **`>=`** use your logic. Example: **Point** with structural equality and lexicographic order:

```first
type Point = { x: Float, y: Float };

function pointEq(a: Point, b: Point) -> Bool {
    return a.x == b.x && a.y == b.y;
}

function pointCompare(a: Point, b: Point) -> Int {
    return if (a.x < b.x) { -1 } else if (a.x > b.x) { 1 }
           else if (a.y < b.y) { -1 } else if (a.y > b.y) { 1 } else { 0 };
}

implementation Eq<Point> { eq = pointEq; }
implementation Ord<Point> { compare = pointCompare; }
```

**Usage:** In code you write **`p == q`**, **`p != r`**, **`p <= q`**, **`p > r`**, **`r < p`**, etc. The compiler lowers these to calls to your **eq** and **compare** functions as described in the table above. You can also call **`eq(p, q)`** and **`compare(p, q)`** directly when needed (e.g. for sorting).

### Summary: “how to call”

| Interface  | You write              | Compiler uses                          |
|------------|------------------------|----------------------------------------|
| ToString   | `toString(x)` or `x.toString()` | Built-in, derived, or your implementation |
| Eq         | `a == b`, `a != b`, or `eq(a, b)` | Built-in or your `eq`                  |
| Ord        | `<=`, `<`, `>=`, `>`, or `compare(a, b)` | Built-in or your `compare`             |

So: **create custom implementations** with `implementation InterfaceName<TypeName> { ... }`, and **call** them by using the usual interface-based operations (`toString(x)`, `==`, etc.); the compiler picks the right implementation from the type of the arguments.

---

## 6. Constrained generics: using interfaces in type parameters

You can restrict a type parameter to types that implement an interface:

```first
function showTwice<T:ToString>(x: T) -> String {
    return toString(x) + " " + toString(x);
}
```

- **`T : ToString`** means: “T must implement ToString.”
- So **`toString(x)`** is allowed inside the function, and the compiler will use the appropriate ToString implementation for the concrete type (e.g. `Int`, `Point`) at each call site.

This is how you **reuse** interface implementations in generic code: constrain `T` to the interface and call the interface method; the compiler resolves it to the right implementation.

---

## 7. Summary

| Topic | Summary |
|-------|---------|
| **Interfaces** | Contracts: a type parameter and required methods (e.g. ToString, Eq, Ord). |
| **Standard interfaces** | ToString (toString), Eq (eq), Ord (compare), Iterator (hasNext, next). Built-in for Int, Float, Bool, String (and Unit for ToString). |
| **Derive** | `derive(ToString) type Point = { ... };` — compiler generates ToString for record types. Eq/Ord derive planned. |
| **Custom implementation** | `implementation ToString<Point> { toString = pointToString; }` — you supply the function. |
| **Calling** | Call the interface method by name: `toString(x)` or `x.toString()`, `==`/`!=` (or `eq(a,b)`), `<=`/`<`/etc. (or `compare(a,b)`). The compiler picks built-in, derived, or custom implementation from the type of the arguments. |

---

## Running the example

From the repository root or this directory:

```bash
cd examples/chapter-09-interfaces
../../tools/fir build
../../tools/fir run
```

Or with firstc:

```bash
FIRST_LIB_PATH=lib ./build/bin/firstc examples/chapter-09-interfaces/src/main.first -o build/ch09
./build/ch09
```

Expected output: printed strings for built-in types (Int, Float, Bool, String) and for derived types `Point` and `Person`, demonstrating how to call `toString` when implementations are built-in or derived.
