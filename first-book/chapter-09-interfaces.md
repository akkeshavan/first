# Chapter 9: Interfaces — ToString, Eq, Ord

This chapter explains **interfaces** in First: what they are, the **standard interfaces** (ToString, Eq, Ord), how to **derive** them for your types, how to write **custom implementations**, and how to **call** interface methods. The runnable examples are in **examples/chapter-09-interfaces**.

Run them with:

```bash
cd examples/chapter-09-interfaces
fir run
```

---

## 1. What is an interface?

An **interface** is a contract: it names a type parameter and one or more **operations** (functions) that any type implementing the interface must provide. In First, interfaces are defined in the **Prelude**. They let you:

- Write **generic code** that works for any type that supports certain operations (e.g. “convert to string”, “compare for equality”).
- Use **constrained type parameters** like `T : ToString` or `T : Eq` so the type checker knows that `T` has those operations.

**Example:** A function that prints any value only needs a way to turn that value into a string. So we say: “for any type `T` that implements **ToString**, we can call `toString(x)` and then print the result.”

---

## 2. Standard interfaces (Prelude)

Import the Prelude so you can use interfaces and Option:

```first
import * "Prelude";
```

### ToString&lt;T&gt;

- **Method:** `toString: function(T) -> String`
- **Meaning:** Turn a value of type `T` into a `String`.
- **Built-in:** `Int`, `Float`, `Bool`, `String`, and `Unit` already implement ToString. You can use `toString(42)`, `toString(3.14)`, `toString("hi")`, etc., without writing any implementation.
- **Use:** Printing, logging, or any generic function that needs to show values.

### Eq&lt;T&gt;

- **Method:** `eq: function(T, T) -> Bool`
- **Meaning:** Compare two values of type `T` for equality.
- **Operators:** **`==`** and **`!=`** are defined in terms of Eq. For any type that implements Eq, `a == b` means `eq(a, b)` and `a != b` means `not eq(a, b)`.
- **Built-in:** `Int`, `Float`, `Bool`, and `String` implement Eq.
- **Use:** Constrained generics like `function elem<T:Eq>(x: T, xs: List<T>) -> Bool`.

### Ord&lt;T&gt;

- **Method:** `compare: function(T, T) -> Int`
- **Meaning:** Compare two values; returns negative, zero, or positive (like a comparator).
- **Operators:** **`<`**, **`<=`**, **`>`**, **`>=`** are defined in terms of Ord. For example `a < b` means `compare(a, b) < 0`.
- **Built-in:** `Int`, `Float`, and `String` implement Ord (Bool does not).
- **Use:** Sorting, min/max, or any generic code that needs to order values.

---

## 3. Derive ToString for record types

For **record types**, you can **derive** an implementation so you don’t have to write it by hand.

**Syntax:** Place **`derive(ToString)`** immediately before the type declaration:

```first
derive(ToString)
type Point = { x: Float, y: Float };
```

The compiler generates a helper that converts each field with `toString` and joins them (e.g. `"{ x = 1.5, y = 2.5 }"`). You can then call **`toString(p)`** or **`p.toString()`** for any `p : Point`.

From **examples/chapter-09-interfaces/src/main.first**:

```first
derive(ToString)
type Point = { x: Float, y: Float };
// ...
let p: Point = { x: 1.5, y: 2.5 };
println(toString(p));   // or p.toString()
```

---

## 4. Custom implementations for Eq and Ord

When you need full control (or the type is not a simple record), write an **implementation** block by hand.

**Syntax:**

```first
implementation InterfaceName<TypeName> {
    memberName = value;
}
```

**Custom Eq and Ord for Point** (from the example):

```first
function pointEq(a: Point, b: Point) -> Bool {
    return a.x == b.x && a.y == b.y;
}

function pointCompare(a: Point, b: Point) -> Int {
    return if (a.x < b.x) {
        -1
    } else if (a.x > b.x) {
        1
    } else if (a.y < b.y) {
        -1
    } else if (a.y > b.y) {
        1
    } else {
        0
    };
}

implementation Eq<Point> {
    eq = pointEq;
}

implementation Ord<Point> {
    compare = pointCompare;
}
```

After that, you can use **`==`**, **`!=`**, **`<`**, **`<=`**, **`>`**, **`>=`** on `Point` values; the compiler uses your `eq` and `compare` functions.

---

## 5. Calling interface methods

Once a type implements an interface (built-in, derived, or custom), you call the method by name or via the operators:

| Interface | You write              | Meaning |
|-----------|------------------------|--------|
| ToString  | `toString(x)` or `x.toString()` | Convert to string |
| Eq        | `a == b`, `a != b`     | Equality / inequality |
| Ord       | `a < b`, `a <= b`, `a > b`, `a >= b` | Ordering |

From the example’s **main**:

```first
println(toString(42));
println(toString(p));
println(toString(p == q));   // true — same x,y
println(toString(p != r));   // true
println(toString(p <= q));   // true — equal
println(toString(p > r));    // true — p > r (lexicographic)
```

---

## 6. Summary

| Topic | Summary |
|-------|---------|
| **Interfaces** | Contracts: a type parameter and required methods (ToString, Eq, Ord). |
| **Standard interfaces** | ToString (toString), Eq (eq), Ord (compare). Built-in for Int, Float, Bool, String (and Unit for ToString). |
| **Derive** | `derive(ToString) type Point = { ... };` — compiler generates ToString for record types. |
| **Custom implementation** | `implementation Eq<Point> { eq = pointEq; }` — you supply the function. |
| **Calling** | `toString(x)` or `x.toString()`; `==`, `!=`, `<`, `<=`, `>`, `>=` use your implementation when defined. |

---

## Running the example

```bash
cd examples/chapter-09-interfaces
fir run
```

Expected output: printed strings for built-in types (Int, Float, Bool, String) and for Point, and comparisons (==, !=, <, <=, >, >=) using the custom Eq and Ord for Point.
