# Chapter 15: Monadic operators in interactions

In First, **monadic operators** (`>>=`, `>>`, `<$>`, `<*>`) are available **only inside interaction functions**. They let you chain effectful computations (I/O, optional values, etc.) in a clear, compositional way. Using them in a pure `function` is a compile-time error.

## Why only in interactions?

- **Pure functions** must stay side-effect free: same inputs → same output, no I/O or mutable state.
- **Interactions** are where effects live. Monadic operators are a way to sequence and combine those effects.
- Restricting these operators to interactions keeps the distinction between pure and effectful code explicit in the language.

## The four operators

| Operator | Name    | Desugaring       | Meaning |
|----------|---------|------------------|--------|
| `>>=`     | bind    | `bind(a, f)`     | Run `a`, pass its result to `f`, return the result of `f`. |
| `>>`      | then    | `then(a, b)`     | Run `a`, then run `b`, return the result of `b` (discard result of `a`). |
| `<$>`     | fmap    | `fmap(f, m)`     | Apply pure function `f` to the value inside the “container” `m` (e.g. `Option`). |
| `<*>`     | apply   | `apply(f, m)`    | Apply a function *inside* a context to a value inside a context (applicative style). |

The parser rewrites these operators into ordinary function calls (`bind`, `then`, `fmap`, `apply`), so you need implementations of those functions for the types you use (e.g. `Unit` for I/O, `Option<T>` for optional values).

## Example 1: Chaining I/O with `>>` (then)

Use **then** when you want to run one effect after another and keep only the second result (often both are `Unit`):

```first
println("Hello") >> println("World");
```

This runs the first `println`, then the second. The type of `>>` here is effectively `then(Unit, Unit) -> Unit`: do first action, then second, return unit.

## Example 2: Chaining optional computations with `>>=` (bind)

Use **bind** when each step can “fail” or produce an optional value and you want to pass that value to the next step:

```first
some(7) >>= function(x) { return some(x * 2); }
```

So: “take the value inside `some(7)`, apply the function to it, and return the resulting `Option`.” If the left-hand side were `none()`, the whole expression would be `none()` without calling the function.

## Example 3: Lifting a pure function with `<$>` (fmap)

Use **fmap** when you have a value inside a “container” (e.g. `Option`) and want to apply a **pure** function to that value, keeping the same container type:

```first
(function(x) { return x + 100; }) <$> some(5)
```

So: “inside `some(5)`, apply `x -> x + 100`,” giving `some(105)`. If the option were `none()`, the result would stay `none()`.

---

The example program in this chapter defines minimal `then`, `bind`, and `fmap` for `Unit` and `Option`, then implements three interactions that use these operators; `main` calls all three so you can see each style in one run.
