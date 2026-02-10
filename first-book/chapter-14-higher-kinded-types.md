# Chapter 14: Higher-kinded types

This chapter introduces **higher-kinded types (HKT)** in First: type parameters that stand for **type constructors** (like `Option` or `List`) rather than plain types (like `Int` or `String`). You will define a **Functor** interface using Scala-like syntax **`F<_>`** and implement it for **Option**, then use **map** in an interaction.

---

## 1. Kinds: types of types

In First, most type parameters have **kind** `*` (pronounced “type”): they stand for a concrete type such as **Int**, **String**, or **Option&lt;Int&gt;**:

- **T** in **List&lt;T&gt;** has kind **\*** — you give it **Int**, **Bool**, etc.

A **higher-kinded** parameter has kind **\* → \*** (or more): it stands for a **type constructor** that takes a type and returns a type. For example:

- **Option** has kind **\* → \***: you apply it to **Int** to get **Option&lt;Int&gt;**.
- **F** in **Functor&lt;F&gt;** should be something like **Option** or **List**, not **Int**.

First lets you declare such parameters with **Scala-like syntax**: **`F<_>`** means “F takes one type argument”; **`F<_, _>`** would mean two type arguments.

---

## 2. The Functor interface

**Functor** is a standard abstraction: a type constructor **F** with a **map** operation that lifts a function **A → B** to **F&lt;A&gt; → F&lt;B&gt;** (e.g. map over the value inside an **Option**).

### Declaring a higher-kinded parameter: F<_>

Use **`F<_>`** in the generic parameter list to mean “F is a type constructor of one argument”:

```first
import * "Prelude"

interface Functor<F<_>> {
  map: forall A B. function(F<A>, function(A) -> B) -> F<B>;
}
```

- **F&lt;_&gt;** says: **F** has kind **\* → \*** (one type argument).
- **map** takes a value **F&lt;A&gt;** and a function **A → B**, and returns **F&lt;B&gt;**.
- So **x.map(f)** in the sense of “apply **f** to the value inside **x**” is expressed as **map(x, f)** here; the compiler also allows **x.map(f)** when **x** has a type that implements **Functor**.

You can read **F&lt;_&gt;** as “F of something” — the underscore is a placeholder for the single type argument.

---

## 3. Implementing Functor for Option

**Option&lt;T&gt;** is a type constructor: it takes **T** and gives **Option&lt;T&gt;** (Some(T) | None). So **Option** has kind **\* → \*** and is a good candidate for **Functor**.

Implement the interface by giving a **map** function that works on **Option&lt;A&gt;**:

```first
function optionMap<A, B>(opt: Option<A>, f: function(A) -> B) -> Option<B> {
  return match opt {
    None => none(),
    Some(x) => some(f(x))
  };
}

implementation Functor<Option> {
  map = optionMap;
}
```

- **optionMap** is a plain function: pattern-match on **Option&lt;A&gt;**; if **None**, return **none()**; if **Some(x)**, return **some(f(x))**.
- **implementation Functor&lt;Option&gt;** says: “the type constructor **Option** implements **Functor** with **map = optionMap**.”

After this, any **Option&lt;T&gt;** can use **.map(f)** (or **map(opt, f)**) wherever the compiler can infer the **Functor&lt;Option&gt;** implementation.

---

## 4. Using map in an interaction

With **Functor&lt;Option&gt;** in scope, you can call **map** on an **Option** value. The compiler resolves **.map** to **optionMap**:

```first
interaction main() -> Unit {
  let o: Option<Int> = some(42);
  let dbl = function(n: Int) -> Int { return n * 2; };
  let mapped = o.map(dbl);
  match mapped {
    Some(v) => println("Functor<Option>: map (*2) Some(42) = Some(" + intToString(v) + ")"),
    None => println("unexpected None")
  };
}
```

Here **o** is **Some(42)**; **o.map(dbl)** produces **Some(84)**. So **mapped** is **Some(84)** and the **Some(v)** branch runs, printing the expected line.

---

## 5. Full program

Putting the interface, implementation, and main together:

```first
// Chapter 14: Higher-kinded types — Functor<F<_>>
import * "Prelude"

// F<_> means F has kind * -> * (type constructor).
interface Functor<F<_>> {
  map: forall A B. function(F<A>, function(A) -> B) -> F<B>;
}

function optionMap<A, B>(opt: Option<A>, f: function(A) -> B) -> Option<B> {
  return match opt {
    None => none(),
    Some(x) => some(f(x))
  };
}

implementation Functor<Option> {
  map = optionMap;
}

interaction main() -> Unit {
  let o: Option<Int> = some(42);
  let dbl = function(n: Int) -> Int { return n * 2; };
  let mapped = o.map(dbl);
  match mapped {
    Some(v) => println("Functor<Option>: map (*2) Some(42) = Some(" + intToString(v) + ")"),
    None => println("unexpected None")
  };
}
```

Running this prints: **Functor&lt;Option&gt;: map (*2) Some(42) = Some(84)**.

---

## 6. Syntax summary: F<_> and F<_, _>

| Syntax | Kind | Meaning |
|--------|------|---------|
| **F&lt;_&gt;** | **\* → \*** | Type constructor with one argument (e.g. Option, List). |
| **F&lt;_, _&gt;** | **\* → \* → \*** | Type constructor with two arguments (e.g. Either, Tuple2). |

You can still use the older form **F : * → *** in generic parameters if you prefer; **F&lt;_&gt;** is the Scala-style alternative.

---

## Summary

| Concept | Syntax / idea |
|--------|----------------|
| **Higher-kinded parameter** | **F&lt;_&gt;** — F is a type constructor (kind **\* → \***). |
| **Functor** | Interface with **map: (F&lt;A&gt;, A→B) → F&lt;B&gt;** (method or function). |
| **Implementation** | **implementation Functor&lt;Option&gt; { map = optionMap; }**. |
| **Using map** | **o.map(f)** or **map(o, f)** when **Option** implements **Functor**. |

---

## Runnable example: chapter-14-HKT

The project **examples/chapter-14-HKT** contains:

1. **Functor&lt;F&lt;_&gt;&gt;** — the interface.
2. **optionMap** and **implementation Functor&lt;Option&gt;**.
3. **main** — builds **some(42)**, applies **map** with a doubling function, and prints the result.

From the repo root:

```bash
cd examples/chapter-14-HKT
fir run
```

You should see: **Functor&lt;Option&gt;: map (*2) Some(42) = Some(84)**.

---

## Try it

- Add a second function (e.g. “add 10”) and call **o.map(add10)** after **o.map(dbl)**.
- Implement **Functor** for a small **List&lt;T&gt;** type (define **listMap** and **implementation Functor&lt;List&gt; { map = listMap; }**).
- Experiment with **F&lt;_, _&gt;** by defining a **Bifunctor** interface (e.g. **first** and **second** that map over the first or second type argument) and implementing it for a two-parameter type if you have one.

Higher-kinded types let you write generic code over type constructors like **Option** and **List**, with Scala-style **F&lt;_&gt;** syntax in First.
