# Chapter 10: Array functions

This chapter covers **arrays** in First: they are **immutable**; operations like **insertAt**, **deleteAt**, **filter**, **reduce**, and **reduceRight** return new arrays or values and leave the original unchanged. The runnable examples are in **examples/chapter-10-Array-functions**.

Run them with:

```bash
cd examples/chapter-10-Array-functions
fir run
```

---

## 1. Arrays are immutable

- **Type:** `Array<T>` — fixed-length, immutable sequence of values of type `T`.
- **Literals:** `[e1, e2, ...]` — e.g. `[1, 2, 3]`. All elements must have the same type.
- **Indexing:** `arr[i]` — get element at index `i` (zero-based).

Any function that “changes” an array (e.g. insert, delete, filter) **returns a new array**; the original is never modified. You use the return value and keep or discard it.

---

## 2. Length and indexing

| Function | Type | Description |
|----------|------|--------------|
| **arrayLength(arr)** | `Array<T> -> Int` | Number of elements. |

Example:

```first
let a: Array<Int> = [1, 2, 3];
println("arrayLength([1,2,3]) = " + intToString(arrayLength(a)));
```

---

## 3. Insert and delete (return Option&lt;Array&lt;T&gt;&gt;)

Both return **Option&lt;Array&lt;T&gt;&gt;** so you can handle invalid indices safely. Import Prelude to use `Some` / `None` and **match**.

| Function | Type | Description |
|----------|------|--------------|
| **insertAt(a, value, position)** | `(Array<T>, T, Int) -> Option<Array<T>>` | New array with `value` at index `position`. `None` if position &lt; 0 or &gt; length. |
| **deleteAt(a, position)** | `(Array<T>, Int) -> Option<Array<T>>` | New array with element at `position` removed. `None` if position out of range. |

From the example:

```first
match insertAt(a, 10, 1) {
    Some(b) => println("insertAt(a, 10, 1) ok; b[1] = " + intToString(b[1])),
    None => println("insertAt failed (bad index)")
};
match deleteAt(a, 1) {
    Some(c) => println("deleteAt(a, 1) ok; c[0] = " + intToString(c[0]) + ", c[1] = " + intToString(c[1])),
    None => println("deleteAt failed (bad index)")
};
// Original a is unchanged
println("Original a unchanged: a[0] = " + intToString(a[0]) + ", a[1] = " + intToString(a[1]));
```

---

## 4. Reduce (fold left and right)

| Function | Type | Description |
|----------|------|--------------|
| **reduce(a, init, f)** | `(Array<T>, U, (acc: U, cur: T) -> U) -> U` | Fold left: start with `init`, then repeatedly apply `f(acc, cur)` over elements left to right. |
| **reduceRight(a, init, f)** | `(Array<T>, U, (cur: T, acc: U) -> U) -> U` | Fold right: start with `init`, then apply `f(cur, acc)` from right to left. |

**Example — sum (fold left):**

```first
let sum: Int = reduce(a, 0, function(acc: Int, cur: Int) -> Int { return acc + cur; });
```

**Example — last element (fold right):**

```first
let last: Int = reduceRight(a, 0, function(cur: Int, acc: Int) -> Int { return cur; });
```

---

## 5. Filter

| Function | Type | Description |
|----------|------|--------------|
| **filter(a, p)** | `(Array<T>, (item: T) -> Bool) -> Array<T>` | New array containing only elements for which `p(item)` is true. |

**Example — keep even numbers:**

```first
let evens: Array<Int> = filter(a, function(x: Int) -> Bool { return x % 2 == 0; });
println("filter(even) length = " + intToString(arrayLength(evens)));
println("evens[0] = " + intToString(evens[0]));
```

---

## 6. Building arrays with insertAt

You can build an array recursively by inserting one element at a time. The example defines **rangeToArray(lo, hi)** to build `[lo, lo+1, ..., hi]`:

```first
function rangeToArray(lo: Int, hi: Int) -> Array<Int> {
    return if (lo > hi) {
        let sentinel: Array<Int> = [0];
        sentinel
    } else if (lo == hi) {
        let single: Array<Int> = [lo];
        single
    } else {
        let rest = rangeToArray(lo, hi - 1);
        let len = arrayLength(rest);
        let out: Array<Int> = match insertAt(rest, hi, len) {
            Some(a) => a,
            None => rest
        };
        out
    };
}
```

This shows that **insertAt** returns a **new** array; the original `rest` is unchanged.

---

## 7. Iteration: for-in

`Array<T>` implements **Iterator&lt;T&gt;** (from Prelude), so you can iterate with **for-in**:

```first
for x in [1, 2, 3, 4, 5] {
    println("  " + intToString(x));
}
```

The loop variable `x` is immutable. You do not mutate the array inside the loop; you just read each element.

---

## 8. Summary

| Topic | Summary |
|-------|---------|
| **Immutability** | Arrays are immutable; insertAt, deleteAt, filter return new arrays. |
| **arrayLength** | Returns the number of elements. |
| **insertAt / deleteAt** | Return `Option<Array<T>>`; use **match** to handle `Some` / `None`. |
| **reduce / reduceRight** | Fold over the array with an initial value and a function. |
| **filter** | Returns a new array of elements that satisfy a predicate. |
| **for-in** | Iterate over array elements; array implements Iterator. |

All of these functions are available in scope when you use the standard library (see **docs/STDLIB_REFERENCE.md** under “Arrays”).

---

## Running the example

```bash
cd examples/chapter-10-Array-functions
fir run
```

Expected output: arrayLength, reduce (sum), reduceRight (last), filter (evens), rangeToArray, for-in, insertAt, deleteAt, and a line showing the original array unchanged.
