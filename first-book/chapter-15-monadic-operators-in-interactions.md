# Chapter 15: Monadic operators in interactions

First allows **monadic operators** (`>>=`, `>>`, `<$>`, `<*>`) **only inside interaction functions**. They let you chain effectful computations (I/O, optional values, etc.) in a clear way. Using them in a pure **function** is a compile-time error. This chapter explains the four operators and gives three runnable interactions that **main** invokes.

---

## 1. Why only in interactions?

- **Pure functions** must stay side-effect free: same inputs → same output, no I/O or mutable state.
- **Interactions** are where effects live. Monadic operators are a way to sequence and combine those effects.
- Restricting these operators to interactions keeps the boundary between pure and effectful code explicit.

---

## 2. The four operators

The parser rewrites each operator into an ordinary function call. Your code (or Prelude) must provide functions with the right names and types.

| Operator | Name   | Desugaring     | Meaning |
|----------|--------|-----------------|---------|
| **>>=**  | bind   | **bind(a, f)**  | Run **a**, pass its result to **f**, return the result of **f**. |
| **>>**   | then   | **then(a, b)**  | Run **a**, then run **b**, return the result of **b** (discard **a**’s result). |
| **<\$>** | fmap   | **fmap(f, m)**  | Apply pure function **f** to the value inside the “container” **m** (e.g. **Option**). |
| **<\*>** | apply  | **apply(f, m)** | Applicative apply: function and value both inside a context. |

So when you write **a >> b**, the compiler turns it into **then(a, b)**. For that to type-check and run, **then** must be in scope (e.g. defined in your file or in an imported module) for the types of **a** and **b**.

---

## 3. Example 1: Chaining I/O with **>>** (then)

Use **then** when you want to run one effect after another and keep only the second result. For I/O, both sides often return **Unit**:

```first
println("Step 1") >> println("Step 2") >> println("Step 3");
```

This runs the first **println**, then the second, then the third. The type of **>>** here is effectively **then(Unit, Unit) → Unit**: do the first action, then the second, return unit.

**In the example project**, the interaction **runThenExample** does exactly this; **main** calls it first.

---

## 4. Example 2: Chaining optional values with **>>=** (bind)

Use **bind** when each step produces an optional value and you want to pass that value to the next step. If the first step is **None**, the whole chain is **None** without running the function.

```first
some(7) >>= function(x: Int) -> Option<Int> { return some(x * 2); }
```

So: take the value inside **some(7)**, apply the function to it, and return the resulting **Option**. Here the result is **some(14)**. If the left-hand side were **none()**, the whole expression would be **none()**.

**In the example project**, the interaction **runBindExample** computes **some(7) >>= (*2)** and prints the result.

---

## 5. Example 3: Lifting a pure function with **<\$>** (fmap)

Use **fmap** when you have a value inside a container (e.g. **Option**) and want to apply a **pure** function to that value, keeping the same container type.

```first
(function(x: Int) -> Int { return x + 100; }) <$> some(5)
```

So: “inside **some(5)**, apply **x → x + 100**,” giving **some(105)**. If the option were **none()**, the result would stay **none()**.

**In the example project**, the interaction **runFmapExample** does this and prints the result.

---

## 6. Minimal then, bind, and fmap

The example project defines minimal **then**, **bind**, and **fmap** so that the desugared operator calls resolve. It uses **Option** from Prelude (via **import \* "Prelude"**).

- **then(a: Unit, b: Unit) → Unit** — run **a** then **b**; for **Unit** we just sequence and return.
- **bind(m, f)** — for **Option&lt;A&gt;** and **function(A) → Option&lt;B&gt;**; pattern-match on **m**, if **Some(x)** return **f(x)**, else **none()**.
- **fmap(f, m)** — for **function(A) → B** and **Option&lt;A&gt;**; pattern-match on **m**, if **Some(x)** return **some(f(x))**, else **none()**.

With these in scope, **main** can call the three interactions that use **>>**, **>>=**, and **<\$>**.

---

## 7. Summary

| Concept | Detail |
|--------|--------|
| **Where allowed** | Monadic operators are **only** allowed in **interaction** functions. |
| **>>=** | **bind(a, f)** — run **a**, pass result to **f**, return **f**’s result. |
| **>>** | **then(a, b)** — run **a**, then **b**, return **b**’s result. |
| **<\$>** | **fmap(f, m)** — apply **f** to the value inside **m**. |
| **<\*>** | **apply(f, m)** — applicative apply (not used in this chapter’s examples). |

---

## Runnable example: chapter-15-monadic-operators

The project **examples/chapter-15-monadic-operators** contains:

1. **then**, **bind**, and **fmap** — minimal implementations for **Unit** and **Option**.
2. **runThenExample** — chains three **println**s with **>>**.
3. **runBindExample** — uses **>>=** with **some(7)** and a doubling function.
4. **runFmapExample** — uses **<\$>** to add 100 to the value in **some(5)**.
5. **main** — calls all three interactions.

From the repo root (with **FIRST_LIB_PATH** set so Prelude is found, e.g. via **fir** or **run-all-local.sh**):

```bash
cd examples/chapter-15-monadic-operators
fir run
```

You should see output from all three examples: the three “Step” lines, then the bind result **Some(14)**, then the fmap result **Some(105)**.

---

## Try it

- Add a fourth interaction that uses **<\*>** (e.g. combine two **Option** values with a two-argument function) if your Prelude or helpers provide **apply** for **Option**.
- Define **then** so it works with a hypothetical **IO&lt;T&gt;** type (e.g. **then(IO&lt;A&gt;, IO&lt;B&gt;) → IO&lt;B&gt;** that runs the first then the second).
- Try using **>>=** or **<\$>** inside a **function** and confirm the compiler reports an error (monadic operators only in interactions).

Monadic operators in First keep effects explicit and composable, while pure functions stay free of them.
