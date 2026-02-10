# Array

Built-in type `Array<T>` and array operations. Arrays are **immutable**: operations like `insertAt`, `deleteAt`, and `filter` return new arrays; the original is unchanged. These functions are always in scope; no import is required (but you need `Option` from Prelude for `insertAt`/`deleteAt` results).

---

## Type and literals

- **Type:** `Array<T>` — fixed-length, immutable sequence of values of type `T`.
- **Literals:** `[e1, e2, ...]` — e.g. `[1, 2, 3]`, `["a", "b"]`. All elements must have the same type.
- **Indexing:** `arr[i]` — get element at index `i` (zero-based). Bounds are checked (or undefined if out of range; see implementation).

---

## Length

| Function | Type | Description |
|----------|------|--------------|
| `arrayLength(arr)` | `Array<T> -> Int` | Number of elements. |

---

## Insert and delete (return Option)

Both return `Option<Array<T>>`: `Some(newArray)` on success, `None` when the index is invalid. Import `Option` from Prelude to pattern-match (e.g. `match insertAt(...) { Some(a) => a; None => ... }`).

| Function | Type | Description |
|----------|------|--------------|
| `insertAt(a, value, position)` | `(Array<T>, T, Int) -> Option<Array<T>>` | New array with `value` at index `position`. Fails if position &lt; 0 or &gt; length. |
| `deleteAt(a, position)` | `(Array<T>, Int) -> Option<Array<T>>` | New array with element at `position` removed. Fails if position &lt; 0 or ≥ length. |

---

## Reduce (fold)

| Function | Type | Description |
|----------|------|--------------|
| `reduce(a, init, f)` | `(Array<T>, U, (acc: U, cur: T) -> U) -> U` | Fold left: start with `init`, then repeatedly apply `f(acc, cur)` over elements left to right. |
| `reduceRight(a, init, f)` | `(Array<T>, U, (cur: T, acc: U) -> U) -> U` | Fold right: start with `init`, then apply `f(cur, acc)` from right to left. |

Example (sum): `reduce(a, 0, function(acc: Int, cur: Int) -> Int { return acc + cur; })`

---

## Filter

| Function | Type | Description |
|----------|------|--------------|
| `filter(a, p)` | `(Array<T>, (item: T) -> Bool) -> Array<T>` | New array containing only elements for which `p(item)` is true. |

---

## Iteration

`Array<T>` implements `Iterator<T>`, so you can use **`for x in arr { ... }`** to iterate over elements. The loop variable `x` is immutable.

---

## Example

```first
import "Prelude"

interaction main() -> Unit {
  let a: Array<Int> = [1, 2, 3];
  println(intToString(arrayLength(a)));
  let sum = reduce(a, 0, function(acc: Int, cur: Int) -> Int { return acc + cur; });
  let evens = filter(a, function(x: Int) -> Bool { return x % 2 == 0; });
  match insertAt(a, 10, 1) {
    Some(b) => println(intToString(arrayLength(b))),
    None => println("insert failed")
  };
  for x in a {
    println(intToString(x));
  };
}
```
