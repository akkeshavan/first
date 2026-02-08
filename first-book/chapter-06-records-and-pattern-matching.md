# Chapter 6: Custom Record Types and Pattern Matching

This chapter introduces **custom record types** (named or inline struct-like types with named fields) and **pattern matching**. You will define record types, build values with record literals, access fields, and use **match** to destructure values and branch on their shape.

First supports **two styles of pattern matching**:

1. **Pattern matching on records** — use a single record type with a tag field (e.g. `kind`) and match on **record patterns** that check field values: `{ kind: "Expression", value: expr, op: _ }`.
2. **Pattern matching on types (sum types)** — define a **sum type** with named variants and match on the **variant name**: `Expression(value)`, `Operator(op)`. The compiler checks that all variants are covered.

Both styles are useful: records are good for ad-hoc shapes and when you don’t need a closed set of variants; sum types give you exhaustiveness and a single place to extend variants. This chapter explains both.

---

## 1. Record types

A **record type** is a fixed set of named fields, each with a type. In First you can use record types in two ways:

- **Inline** in signatures: `{ x: Int, y: Int }`
- **Named** via a type alias (when supported): `type Point = { x: Float, y: Float };`

### Inline record type

Use an inline record type in function parameters or return types:

```first
function getX(rec: { x: Int, y: Int }) -> Int {
    return rec.x;
}

function makePoint(x: Int, y: Int) -> { x: Int, y: Int } {
    return { x: x, y: y };
}
```

- **Syntax:** `{ fieldName: Type, ... }`. Each field has a name and a type.
- **Field access:** `recordExpr.fieldName` (e.g. `rec.x`).

### Record literals

A **record literal** has the same shape as the type, but with expressions as values:

```first
let p = { x: 10, y: 20 };
let nameAndAge = { name: "Alice", age: 30 };
```

- **Syntax:** `{ fieldName: expr, ... }`.
- The type of the literal is inferred from the field types (or from context when required).

### Field access

Use a dot to read a field:

```first
let p = { x: 1, y: 2 };
let a = p.x;   // 1
let b = p.y;   // 2
```

---

## 2. Pattern matching on records (record literals)

**First style:** pattern matching with **record literals**. You use a single record type and **record patterns** that match on field names and (optionally) literal values (e.g. a tag like `kind: "Expression"`). Exhaustiveness is not enforced.

### Match expression

```first
match expr {
    pattern1 => result1,
    pattern2 => result2,
    ...
}
```

- The **matched expression** is evaluated once.
- **Cases** are tried in order; the first **matching** case’s right-hand side is evaluated and that value is the result of the whole `match`.
- All case bodies must have the **same type** (the type of the match expression).

### Record patterns

A **record pattern** looks like a record type or literal, but with **patterns** instead of types or expressions in each field:

| Pattern form        | Meaning |
|---------------------|--------|
| `{ shape: s, radius: r, width: w, height: h }` | Match a record with those fields; bind to `s`, `r`, `w`, `h`. |
| `{ shape: "Circle", radius: r, width: _, height: _ }` | Match when `shape` is `"Circle"`; bind `radius` to `r`. |
| `{ shape: "Rectangle", radius: _, width: w, height: h }` | Match when `shape` is `"Rectangle"`; bind `width` and `height`. |
| `r`                 | Variable pattern: match anything and bind to `r`. |

You can mix literal patterns (e.g. string `"Circle"`), variable patterns, and wildcards in the same record pattern.

### Example: Shapes (record literals)

We represent **shapes** with one record type: a tag field **shape** plus fields for shape-specific data. Use **record literals** to build values and **record patterns** to match on the tag.

**One record type** (all shapes in one struct; unused fields can be zero):

```first
function describe(s: { shape: String, radius: Float, width: Float, height: Float }) -> String {
    return match s {
        { shape: "Circle", radius: r, width: _, height: _ } =>
            "Circle with radius " + floatToString(r),
        { shape: "Rectangle", radius: _, width: w, height: h } =>
            "Rectangle " + floatToString(w) + " x " + floatToString(h),
        other => "unknown shape"
    };
}
```

**Building values** with record literals:

```first
function makeCircle(radius: Float) -> { shape: String, radius: Float, width: Float, height: Float } {
    return { shape: "Circle", radius: radius, width: 0.0, height: 0.0 };
}

function makeRectangle(w: Float, h: Float) -> { shape: String, radius: Float, width: Float, height: Float } {
    return { shape: "Rectangle", radius: 0.0, width: w, height: h };
}
```

- **Circle** case: when `shape` is `"Circle"`, we use the `radius` field.
- **Rectangle** case: when `shape` is `"Rectangle"`, we use `width` and `height`.
- **Catch-all** `other` handles any other tag; exhaustiveness is not enforced. Order matters: put more specific patterns before the catch-all.

### Wildcard in patterns

Use **`_`** when you don’t care about a value:

```first
match s {
    { shape: "Circle", radius: r, width: _, height: _ } => floatToString(r),
    { shape: "Rectangle", radius: _, width: w, height: h } => floatToString(w) + " x " + floatToString(h)
}
```

### Summary: pattern matching with record literals

- **Type:** one record type with a tag field, e.g. `{ shape: String, radius: Float, width: Float, height: Float }`.
- **Construction:** record literals, e.g. `{ shape: "Circle", radius: 5.0, width: 0.0, height: 0.0 }`.
- **Pattern:** record pattern with field names and (optionally) literal tags: `{ shape: "Circle", radius: r, width: _, height: _ }`.
- **Exhaustiveness:** not enforced; add a catch-all (e.g. variable pattern) if you want to cover “anything else”.

---

## 3. Named types and sum types (pattern matching on types)

Beyond inline records, you can give types **names** and define **sum types** (one of several variants), then **match on the variant name** instead of on a string tag. The compiler ensures every variant is covered.

### Named record types

Declare a type name for a record shape so you can reuse it:

```first
type Point = { x: Float, y: Float };

function distance(p: Point, q: Point) -> Float {
    let dx = p.x - q.x;
    let dy = p.y - q.y;
    return sqrt(dx * dx + dy * dy);
}
```

- **Syntax:** `type TypeName = { fieldName: Type, ... };`
- Use **TypeName** in parameters, return types, and variable annotations instead of writing the inline record repeatedly.

### Sum types (algebraic data types)

A **sum type** is a type whose values are exactly one of several **variants**. Each variant has a **name** and carries a **payload** (one or more types in a fixed order).

**Example: lexer items**

- Some tokens are **expressions** (e.g. a number).
- Some are **operators** (e.g. `"+"`, `"-"`).

Define a sum type with **positional** payloads:

```first
// Sum type: a value is either Expression(Int) or Operator(String)
// Parameter names (value:, op:) are optional and document the payload.
type LexItem = Expression(value: Int) | Operator(op: String);
```

- **Expression(Int)** — variant named **Expression** with one payload of type **Int**.
- **Operator(String)** — variant named **Operator** with one payload of type **String**.

**Creating values**

Use the **variant name** as a constructor with arguments in the same order as in the declaration:

```first
let e1 = Expression(42);      // Expression variant with payload 42
let plus = Operator("+");     // Operator variant with payload "+"
```

**Pattern matching on types (variants)**

Match on the **variant name** and bind the payload to variables. The compiler requires that **every variant** is covered (exhaustiveness).

```first
function showItem(v: LexItem) -> String {
    return match v {
        Expression(value) => "expr: " + intToString(value),
        Operator(op)      => "op: " + op
    };
}
```

- **Expression(value)** — matches the **Expression** variant and binds its payload (the **Int**) to **value**.
- **Operator(op)** — matches the **Operator** variant and binds its payload (the **String**) to **op**.

If you don’t need the payload, use a wildcard: `Expression(_) => ...`, `Operator(_) => ...`.

**Contrast with record patterns**

| | Record literals (§2) | Sum types (§3) |
|--|----------------------|----------------|
| **Type** | One record, e.g. `{ kind: String, value: Int, op: String }` | Sum, e.g. `Expression(Int) \| Operator(String)` |
| **Construction** | `{ kind: "Expression", value: 42, op: "" }` | `Expression(42)` |
| **Pattern** | `{ kind: "Expression", value: expr, op: _ }` | `Expression(value)` |
| **Exhaustiveness** | Not checked | Required (all variants must have a case) |

**Summary of the form**

| Concept | Syntax |
|--------|--------|
| Sum type | `type LexItem = Expression(value: Int) \| Operator(op: String);` (names optional) |
| Construct value | `Expression(42)`, `Operator("+")` |
| Match on variant | `Expression(value) => ...`, `Operator(op) => ...` |

### Sum-of-records (Shape = Circle | Rectangle)

You can define a sum type whose variants each carry a **named record type**. Use **record patterns** inside the constructor pattern and a **default case** (exhaustiveness is not required; a catch-all is).

```first
type Circle = { radius: Float };
type Rectangle = { len: Float, width: Float };
type Shape = Circle | Rectangle;   // sugar for Circle(Circle) | Rectangle(Rectangle)

function describe(s: Shape) -> String {
    return match s {
        Circle({ radius }) => "Circle with radius " + floatToString(radius),
        Rectangle({ len, width }) => "Rectangle " + floatToString(len) + " x " + floatToString(width),
        other => "unknown shape"
    };
}

let c = Circle({ radius: 5.0 });
let r = Rectangle({ len: 10.0, width: 20.0 });
```

- **Construction:** `Circle({ radius: 5.0 })`, `Rectangle({ len: 10.0, width: 20.0 })`.
- **Pattern:** `Circle({ radius })` or `Circle({ radius: r })`; shorthand `{ radius }` binds the field to a variable of the same name.
- **Default case:** A match on a sum-of-records type must include a default (variable or `_`) case.

---

## 4. Record update (optional)

Some dialects support **record update** to build a new record from an existing one with some fields changed:

```first
let updated = rec.{ x = 42 };
// New record: same as rec but with x = 42
```

Use this when the language supports it; otherwise build a new literal with the fields you want.

---

## 5. Putting it together: two complete examples

This section shows one full example for **sum types** (no record literal) and one for **record literals** (shapes). Each style is self-contained.

### Example A: Sum types — Expression / Operator (LexItem)

**No record literal.** Declare a sum type, construct with the variant name, and match on the variant. The compiler enforces exhaustiveness.

```first
type LexItem = Expression(value: Int) | Operator(op: String);

function showItem(v: LexItem) -> String {
    return match v {
        Expression(value) => "expr: " + intToString(value),
        Operator(op)      => "op: " + op
    };
}

interaction main() -> Unit {
    let e1 = Expression(42);
    let e2 = Expression(100);
    let plus = Operator("+");
    let minus = Operator("-");
    println(showItem(e1));
    println(showItem(plus));
    println(showItem(e2));
    println(showItem(minus));
}
```

Running this prints: `expr: 42`, `op: +`, `expr: 100`, `op: -`. See runnable example **chapter-06-records-pattern-matching** (§ Runnable examples).

### Example B: Record literals — Shapes (Circle / Rectangle)

**Record literals only.** One record type with a **shape** tag; build values with record literals and match with record patterns. Exhaustiveness is not enforced.

```first
function describe(s: { shape: String, radius: Float, width: Float, height: Float }) -> String {
    return match s {
        { shape: "Circle", radius: r, width: _, height: _ } =>
            "Circle with radius " + floatToString(r),
        { shape: "Rectangle", radius: _, width: w, height: h } =>
            "Rectangle " + floatToString(w) + " x " + floatToString(h),
        other => "unknown shape"
    };
}

function makeCircle(radius: Float) -> { shape: String, radius: Float, width: Float, height: Float } {
    return { shape: "Circle", radius: radius, width: 0.0, height: 0.0 };
}

function makeRectangle(w: Float, h: Float) -> { shape: String, radius: Float, width: Float, height: Float } {
    return { shape: "Rectangle", radius: 0.0, width: w, height: h };
}

interaction main() -> Unit {
    let c = makeCircle(5.0);
    let r = makeRectangle(10.0, 20.0);
    println(describe(c));
    println(describe(r));
}
```

Running this prints: `Circle with radius 5`, `Rectangle 10 x 20`. See runnable example **chapter-06-shapes-record-literals** (§ Runnable examples).

---

## Summary

| Concept | Syntax / idea |
|--------|----------------|
| **Two pattern styles** | **(1)** Record patterns: match on field names/literals, e.g. `{ kind: "Expression", value: expr, op: _ }`. **(2)** Variant patterns: match on sum-type constructors, e.g. `Expression(value)`, `Operator(op)`; exhaustiveness required. |
| **Inline record type** | `{ fieldName: Type, ... }` in parameters or return types. |
| **Named record type** | `type Point = { x: Float, y: Float };` — use **Point** in annotations. |
| **Sum type** | `type LexItem = Expression(value: Int) \| Operator(op: String);` — one of several variants (param names optional). |
| **Variant pattern** | `Expression(value) => ...`, `Operator(op) => ...` — match by variant name and bind payload. |
| **Record pattern** | `{ shape: "Circle", radius: r, width: _, height: _ }`, `{ shape: "Rectangle", ... }`, or variable `r`. |
| **Record literal** | `{ fieldName: expr, ... }`; type inferred or from context. |
| **Field access** | `recordExpr.fieldName`. |
| **Match expression** | `match expr { pat1 => e1, pat2 => e2, ... }`; first matching case wins. |
| **Variable pattern** | A single name (e.g. `r`) matches any value and binds it. |
| **Wildcard** | `_` in a pattern matches without binding. |

---

## Runnable examples

Chapter 6 has **two** runnable examples: one for **sum types** (Expression/Operator) and one for **record literals** (Shapes).

### 1. Sum types: chapter-06-records-pattern-matching

**examples/chapter-06-records-pattern-matching** — pattern matching on **types** (no record literal):

- **type LexItem** — sum type with variants **Expression(Int)** and **Operator(String)**.
- **showItem(v)** — match on **Expression(value)** and **Operator(op)**; exhaustiveness is enforced.
- **main** — builds values with **Expression(42)** and **Operator("+")**, then prints via **showItem**.

```bash
cd examples/chapter-06-records-pattern-matching
../../tools/fir build
../../tools/fir run
```

Expected output: `expr: 42`, `op: +`, `expr: 100`, `op: -`.

### 2. Record literals: chapter-06-shapes-record-literals

**examples/chapter-06-shapes-record-literals** — pattern matching on **record literals** (Shapes):

- One record type with **shape**, **radius**, **width**, **height**.
- **describe(s)** — match on **shape: "Circle"** and **shape: "Rectangle"** with record patterns; catch-all for unknown.
- **makeCircle** / **makeRectangle** — build values with record literals.
- **main** — builds a circle and a rectangle, then prints **describe** of each.

```bash
cd examples/chapter-06-shapes-record-literals
../../tools/fir build
../../tools/fir run
```

Expected output: `Circle with radius 5`, `Rectangle 10 x 20`.

---

## Try it

- **Sum types (LexItem):** Add a variant with no payload (e.g. **Paren**) to **LexItem** and a match case **Paren => "paren"**; the compiler will require it.
- **Record literals (Shapes):** Add a third shape (e.g. **"Square"** with one side field) and a record pattern for it; the catch-all already handles unknown tags.
- Write a function that takes a lex item (sum-type example) and returns an **Int** (the expression value, or 0 for operators) using **match**.

Records and both styles of pattern matching give you structured data and clear handling of each shape in one place.
