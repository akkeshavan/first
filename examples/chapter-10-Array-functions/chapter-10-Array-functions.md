# Chapter 10: Array Functions — Examples

This folder contains runnable examples for **Chapter 10** of the First book: **Array functions**, including immutability, building arrays, and the standard array API.

---

## Arrays are immutable

In First, **arrays are immutable**: you cannot change an existing array in place. Any operation that “adds,” “removes,” or “changes” an element **returns a new array**; the original is unchanged.

- **Literals** like `[1, 2, 3]` create a new array.
- **insertAt(a, value, pos)** returns `Option<Array<T>>`: `Some(newArray)` with the value inserted, or `None` if the index is invalid. `a` is unchanged.
- **deleteAt(a, pos)** returns `Option<Array<T>>`: `Some(newArray)` with the element at `pos` removed, or `None` if the index is invalid. `a` is unchanged.
- **filter(a, p)** returns a new array containing only elements for which `p(item)` is true. `a` is unchanged.
- **reduce** / **reduceRight** do not change the array; they compute a single value (e.g. sum, product) from it.

So you “build” new arrays by starting from literals or existing arrays and using these functions, not by mutating.

---

## Creating arrays

### 1. Literals

The simplest way to create an array is with a literal:

```first
let a: Array<Int> = [1, 2, 3];
let words: Array<String> = ["hello", "world"];
```

### 2. Using for-in to iterate (not to mutate)

Because arrays are immutable, you **do not** build an array by mutating a variable inside a `for` loop. Instead, you use `for-in` to **iterate over an existing array** (e.g. to print, or to feed into another computation).

- **for-in over a range:** `for i in 1..=10 { println(intToString(i)); }` — loop over the range, no array built in the loop.
- **for-in over an array:** `Array<T>` implements `Iterator<T>`, so you can do `for x in myArray { println(toString(x)); }` to iterate over elements of an array you already have (e.g. one built from a literal or from a recursive function).

So “creating arrays” with for-in means: **use for-in to walk over a range or an array**; the array itself is created by literals or by recursive building (see below).

### 3. Building arrays recursively

You can build an array recursively by starting from a base case and using **insertAt** to add one element at a time. For example, building the range `[lo, lo+1, ..., hi]`:

- **Base case:** if `lo == hi`, return `[lo]`.
- **Step:** otherwise, build `rangeToArray(lo, hi - 1)`, then insert `hi` at the end with `insertAt(rest, hi, arrayLength(rest))`. The result is `Some(newArray)`; unwrap with `match` to get the new array.

This pattern (build a smaller array recursively, then insert one more element) is the immutable way to “grow” an array. See `rangeToArray` in `src/main.first`.

---

## Array functions (examples in code)

| Function | Signature (conceptually) | Description |
|----------|--------------------------|-------------|
| **arrayLength** | `(a: Array<T>) -> Int` | Returns the number of elements. |
| **reduce** | `(a: Array<T>, init: U, f: (acc: U, cur: T) -> U) -> U` | Fold left: start with `init`, then repeatedly apply `f(acc, cur)` for each element. Example: sum with `init = 0` and `f = (acc, cur) => acc + cur`. |
| **reduceRight** | `(a: Array<T>, init: U, f: (cur: T, acc: U) -> U) -> U` | Fold right: same idea, but elements are processed from right to left. |
| **filter** | `(a: Array<T>, p: (item: T) -> Bool) -> Array<T>` | Returns a new array with only elements for which `p(item)` is true. |
| **insertAt** | `(a: Array<T>, value: T, position: Int) -> Option<Array<T>>` | Returns `Some(newArray)` with `value` at index `position`, or `None` if the index is invalid. |
| **deleteAt** | `(a: Array<T>, position: Int) -> Option<Array<T>>` | Returns `Some(newArray)` with the element at `position` removed, or `None` if the index is invalid. |

All of these are used in `src/main.first` with concrete examples (sum, product, evens, range building, insert/delete with `match` on `Option`).

---

## How to run

From the project root (or wherever the First toolchain is configured):

- Build/run the example (exact command depends on your setup), e.g.:
  - `firstc chapter-10-Array-functions` then run the generated binary, or
  - Use your IDE’s run for the `chapter-10-Array-functions` example.

The `main` interaction prints outputs for: array literals, length, reduce (sum), reduceRight (last element), filter (evens), range built recursively, for-in over a literal array (for-in currently requires array metadata, so we use `[1,2,3,4,5]` rather than `oneToFive`), and insertAt/deleteAt with pattern matching on `Option`.
