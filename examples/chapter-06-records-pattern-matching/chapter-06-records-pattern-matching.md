# Chapter 6: Records and Pattern Matching — Examples

This folder contains runnable examples for **Chapter 6** of the First book: **Records and Pattern Matching**. The main source is `src/main.first`. You can export this document to `.doc` if needed.

---

## 1. Expression example (sum types and match)

We define an expression type with terms and binary expressions, and an operator type:

```first
type Operator = Operator(op: String);
type Expr = Term(value: Int)
          | BinaryExpression(left: Expr, op: Operator, right: Expr);

function showExpression(expr: Expr) -> String {
    return match expr {
        Term(value) => "expr: " + intToString(value),
        BinaryExpression(left, op, right) =>
            "Binary(" + showExpression(left) + ", " + op.op + ", " + showExpression(right) + ")"
    };
}
```

- **Sum type:** `Expr` is either a `Term` or a `BinaryExpression`; `Operator` is a single-variant type wrapping a string.
- **Pattern matching:** The `match` is exhaustive over the constructors of `Expr`; in `BinaryExpression(left, op, right)` we use `op.op` to get the operator string.

**Construction:** `Term(42)`, `Operator("+")`, `BinaryExpression(left, op, right)`.

---

## 2. FizzBuzz (guards and otherwise)

Integer pattern with **when** guards and an **otherwise** catch-all:

```first
function fizzBuzz(n: Int) -> String {
    return match n {
        n when n % 15 == 0 => "FizzBuzz",
        n when n % 3 == 0 => "Fizz",
        n when n % 5 == 0 => "Buzz",
        otherwise => intToString(n)
    };
}
```

- **Guards:** `n when n % 3 == 0` means “match any integer and then require the condition.”
- **Otherwise:** The last branch catches any value not matched above (e.g. 1, 2, 4).

---

## 3. Shape example (record types)

**Sum-of-records** style: each variant is a record type; match with record patterns.

```first
type Circle = { radius: Float };
type Rectangle = { len: Float, width: Float };
type Shape = Circle | Rectangle;

function describe(s: Shape) -> String {
    return match s {
        Circle({ radius }) => "Circle with radius " + floatToString(radius),
        Rectangle({ len, width }) => "Rectangle " + floatToString(len) + " x " + floatToString(width),
        other => "unknown shape"
    };
}
```

- **Record types:** `Circle` and `Rectangle` are record types; `Shape` is the sum of the two.
- **Construction:** `Circle({ radius: 5.0 })`, `Rectangle({ len: 10.0, width: 20.0 })`.
- **Patterns:** `Circle({ radius })` and `Rectangle({ len, width })` bind the record fields. When all constructors are covered, no default case is needed; otherwise use `other` or `_`.

---

## How to run

From the project root (or wherever the First toolchain is configured):

- Build/run the example (exact command depends on your setup), e.g.:
  - `first run examples/chapter-06-records-pattern-matching`
  - or open the project and run the main target.

The `main` interaction in `src/main.first` prints sample output for the expression example, FizzBuzz, and the shape `describe` function.

---

## File layout

- `src/main.first` — All three examples and `main`.
- `fir.json` — Project config (`"main": "src/main.first"`).
- `chapter-06-records-pattern-matching.md` — This document (export to `.doc` if needed).
