# Chapter 7: A Haskell-like List library

This folder contains the runnable example for **Chapter 7** of the First book. The main source is **src/main.first**. It demonstrates **generics**: type parameters, generic data types, generic functions, and constrained generics.

---

## Concepts of generics (using this example)

### 1. Type parameters: `T` as a placeholder

A **type parameter** is a name that stands for “some type” so you can write one definition that works for many types. In First we use **angle brackets** to introduce type parameters.

- In **`List<T>`**, the `T` is a type parameter: “a list of *some* type T.”
- In **`function cons<T>(x: T, xs: List<T>)`**, the `<T>` declares that this function is generic over `T`: the first argument has type `T`, the second is `List<T>`, and the result is `List<T>`.

So **one** definition of `cons` works for **List&lt;Int&gt;**, **List&lt;String&gt;**, **List&lt;Bool&gt;**, etc. The compiler generates a specialized version (e.g. `cons_Int`) when you call `cons(1, xs)` with `xs : List<Int>`.

---

### 2. Generic data types: `type List<T> = ...`

A **generic data type** is a type that takes one or more type parameters. In this example, the list type is defined as:

```first
type List<T> = Cons(T, List<T>) | Nil;
```

- **`List<T>`** — “list of elements of type T.”
- **`Cons(T, List<T>)`** — a constructor that holds one value of type `T` and the rest of the list, which has type `List<T>` (recursive).
- **`Nil`** — the empty list (no type arguments).

So:

- **`List<Int>`** — list of integers: `Cons(1, Cons(2, Cons(3, Nil())))`.
- **`List<String>`** — list of strings: `Cons("a", Cons("b", Nil()))`.
- **`List<Bool>`** — list of booleans, etc.

The **same** shape (Cons / Nil) is reused for every element type; only `T` changes.

---

### 3. Generic functions: one implementation, many types

A **generic function** is a function that has type parameters. Its body is written once in terms of those parameters; at each call site the compiler **monomorphizes** it (produces a version for the concrete types used).

Examples from **src/main.first**:

| Function | Signature | Meaning |
|----------|-----------|--------|
| **cons** | `function cons<T>(x: T, xs: List<T>) -> List<T>` | Add `x` (of type `T`) to the front of `xs` (a `List<T>`). One implementation for all `T`. |
| **isEmpty** | `function isEmpty<T>(xs: List<T>) -> Bool` | Whether the list is empty. No dependence on element type. |
| **length** | `function length<T>(xs: List<T>) -> Int` | Number of elements. Same logic for any `T`. |
| **head** | `function head<T>(xs: List<T>) -> T \| null` | First element, or null if empty. Return type is **T or null** (optional). |
| **tail** | `function tail<T:Eq>(xs: List<T>) -> List<T>` | Rest of the list (requires **T : Eq**; see below). |

When you write:

```first
let xs: List<Int> = cons(1, cons(2, cons(3, Nil())));
head(xs)   // type Int | null; value 1
length(xs) // 3
```

the compiler infers `T = Int` and uses the specialized versions (e.g. `head_Int`, `length_Int`). So **generics give you one implementation and many concrete types**.

---

### 4. Optional / union return types: `T | null`

Some functions need to express “a value of type T **or** nothing.” In First we use a **union type** `T | null`:

```first
function head<T>(xs: List<T>) -> T | null {
    return match xs {
        Cons(h, t) => h,
        Nil => null
    };
}
```

- **`T | null`** means “either a value of type `T` or `null`.”
- For **`List<Int>`**, `head(xs)` has type **`Int | null`**: either an integer or null (empty list).
- The **match** covers both constructors: `Cons` returns the first element `h`; `Nil` returns `null`. The type checker unifies these two branches into the single return type `T | null`.

So generics work together with union types to type “optional generic value” safely.

---

### 5. Constrained generics: `T : Eq`

Some operations (e.g. “is this element in the list?”) need to compare elements. So we **constrain** the type parameter to types that support equality:

```first
function elem<T:Eq>(x: T, xs: List<T>) -> Bool {
    return match xs {
        Cons(h, t) => (h == x) || elem(x, t),
        Nil => false
    };
}
```

- **`T : Eq`** means “T must implement the **Eq** interface” (which provides `eq` / `==`).
- **`tail`** in this example also uses **`T:Eq`** so the compiler knows comparison is available if needed.

**Prelude** (imported with `import * "Prelude"`) defines **Eq&lt;T&gt;** in **lib/Prelude.first**. Built-in types **Int**, **Float**, **Bool**, **String** implement **Eq** in the compiler. For your own types you write:

```first
implementation Eq<MyType> { eq = ... }
```

So **constrained generics** let you write one generic function (e.g. `elem`) that works for any type that supports the required operations (here, equality).

---

### 6. How it fits together in `main`

The demo in **src/main.first** ties these ideas together:

```first
let xs: List<Int> = cons(1, cons(2, cons(3, Nil())));
//                  ^^^^   ^^^^   ^^^^  ^^^
//                  T=Int  List<Int>    List<Int>

isEmpty(xs)   // false
length(xs)    // 3
head(xs)      // 1 (type Int | null)
elem(2, xs)   // true (uses T:Eq for Int)
tail(xs)      // List<Int> = cons(2, cons(3, Nil()))
```

- **Generic data type**: `List<Int>`.
- **Generic functions**: `cons`, `isEmpty`, `length`, `head`, `elem`, `tail` — each defined once, used at type `Int`.
- **Optional type**: `head(xs)` is `Int | null`.
- **Constrained generic**: `elem(2, xs)` and `tail(xs)` use `T : Eq` with `T = Int`.

---

## What this example does (summary)

1. **Generic data type** — **`type List<T> = Cons(T, List<T>) | Nil`** so you can have **List&lt;Int&gt;**, **List&lt;String&gt;**, etc.
2. **Interfaces from Prelude** — **Eq**, **Ord**, **Iterator** in **lib/Prelude.first**; built-in types implement **Eq** (and **Ord** where applicable); user types use **implementation Eq&lt;MyType&gt;** { ... }.
3. **Haskell-style list functions** — **cons**, **isEmpty**, **length**, **elem**, **head**, **tail**, all generic over **List&lt;T&gt;** (with **T : Eq** where comparison is needed).
4. **Demo** — Builds **List&lt;Int&gt;** and prints **isEmpty**, **length**, **head**, **elem**, and **tail**.

---

## Running

Build from the **repository root** so the compiler can find **lib/Prelude.first**:

```bash
# From repo root
./build/bin/firstc examples/chapter-07-Intro-to-generic-types/src/main.first -o build/ch07
./build/ch07
```

Or use **fir** from this directory:

```bash
cd examples/chapter-07-Intro-to-generic-types
../../tools/fir build
../../tools/fir run
```

Or from the build directory (Prelude is found via `lib/` relative to cwd):

```bash
cd build
./bin/firstc ../examples/chapter-07-Intro-to-generic-types/src/main.first -o ch07
./ch07
```

---

## See the book

**first-book/chapter-07-Intro-to-generic-types.md** explains generic data types, interfaces, constrained generics (**T : Eq**), and how to build this Haskell-like list library in First.
