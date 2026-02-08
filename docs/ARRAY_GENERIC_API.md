# Generic Array<T> API (to implement)

Array operations should be **generic over element type** (and, for map, result type), not Int-specific. They live on `Array<T>` and are implemented in the **runtime** (like print/println, math), with compiler support for type-check and codegen.

## Target signatures

- **append**(a: Array<T>, value: T) -> Array<T>  
  Return a new array with `value` at the end.

- **insertAt** / **deleteAt** — see below (Option vs Error return).

- **map**<T, U>(a: Array<T>, f: (item: T) -> U) -> Array<U>  
  T and U may be the same. Apply `f` to each element, return new array.

- **reduce** (foldLeft) and **reduceRight** (foldRight) — see below.

- **filter**<T>(a: Array<T>, p: (item: T) -> Bool) -> Array<T>  
  Return a new array containing only elements for which p(item) is true. **Implemented** (compiler: two-pass with closure; result has dynamic length).

- **And so on**: etc. — same idea: generic over T (and U where applicable), implemented in runtime or compiler, exposed as built-ins or via a standard Arrays module that the compiler knows about.

---

## insertAt and deleteAt — return type: Option vs Error

Both can fail when the index is invalid (negative or ≥ length for insertAt, or ≥ length for deleteAt). Two common ways to model that:

### Option<Array<T>>

- **insertAt**<T>(a: Array<T>, value: T, position: Int) -> Option<Array<T>>  
  - Some(newArray) on success; None when position is out of range.

- **deleteAt**<T>(a: Array<T>, position: Int) -> Option<Array<T>>  
  - Some(newArray) on success; None when position is out of range.

**Pros:** Simple, matches “maybe no value”; no need for an Error type; easy to chain with map/getOrElse.  
**Cons:** Caller does not get a reason (e.g. “index 5 out of range for length 3”).

### Array<T> | Error (or Result<Array<T>, Error>)

- **insertAt**<T>(a: Array<T>, value: T, position: Int) -> Array<T> | Error  
  - Array on success; Error (with message/code) when position is invalid.

- **deleteAt**<T>(a: Array<T>, position: Int) -> Array<T> | Error  
  - Array on success; Error when position is invalid.

**Pros:** Caller can log or handle a concrete error (e.g. index and length).  
**Cons:** Requires an Error type (or string/variant); slightly heavier when you only need success/failure.

### Recommendation

- **Prefer Option<Array<T>>** if you only need “succeeded or not” and want to keep the type system small. It’s consistent with other “maybe missing” APIs (e.g. lookup, head) and is enough for insertAt/deleteAt where the only failure is “bad index”.

- **Use Array<T> | Error (or Result)** if you want diagnosable failures (debugging, user-facing messages, or richer error handling). Then you need a shared Error type (or Result<E,T>) in the language.

So: **Option<Array<T>>** for minimal design; **Array<T> | Error** if you value explicit error information.

**Implemented:** insertAt, deleteAt (return Option<Array<T>>); reduce, reduceRight (Haskell/Scala style); **filter**<T>(a: Array<T>, p: (item: T) -> Bool) -> Array<T>. Option<T> = Some(T) | None is in Prelude; runtime has first_array_insert_at, first_array_delete_at. filter is implemented in the compiler (two-pass: count then fill) with dynamic-length result metadata.

---

## reduce (foldLeft) and reduceRight (foldRight)

### Haskell / Scala style (signatures only — no index)

**Haskell**

- **foldl** (fold left):  
  `foldl :: (b -> a -> b) -> b -> [a] -> b`  
  So: operator `(acc, element) -> acc`, initial value, list → result.  
  First equivalent: **reduce**<T,U>(a: Array<T>, init: U, f: (acc: U, cur: T) -> U) -> U

- **foldr** (fold right):  
  `foldr :: (a -> b -> b) -> b -> [a] -> b`  
  So: operator `(element, acc) -> acc`, initial value, list → result.  
  First equivalent: **reduceRight**<T,U>(a: Array<T>, init: U, f: (cur: T, acc: U) -> U) -> U

- **foldl1** / **foldr1** (no initial value; non-empty list; result type = element type):  
  `foldl1 :: (a -> a -> a) -> [a] -> a`  
  First equivalent: **reduce1**<T>(a: Array<T>, f: (acc: T, cur: T) -> T) -> T (and similarly reduceRight1).

**Scala**

- **foldLeft**:  
  `def foldLeft[B](z: B)(op: (B, A) => B): B`  
  So: initial `z`, then `(accumulator, element) -> new accumulator`.  
  First equivalent: **reduce**<T,U>(a: Array<T>, init: U, f: (acc: U, cur: T) -> U) -> U

- **foldRight**:  
  `def foldRight[B](z: B)(op: (A, B) => B): B`  
  So: initial `z`, then `(element, accumulator) -> new accumulator`.  
  First equivalent: **reduceRight**<T,U>(a: Array<T>, init: U, f: (cur: T, acc: U) -> U) -> U

So, matching Haskell/Scala (no index in the callback), the signatures are:

| First (Haskell/Scala style) | Meaning |
|-----------------------------|--------|
| **reduce**<T,U>(a: Array<T>, init: U, f: (acc: U, cur: T) -> U) -> U | foldLeft: left-to-right, f(acc, cur) |
| **reduceRight**<T,U>(a: Array<T>, init: U, f: (cur: T, acc: U) -> U) -> U | foldRight: right-to-left, f(cur, acc) |

Optional (no init, non-empty, result T):

| First | Meaning |
|-------|--------|
| **reduce1**<T>(a: Array<T>, f: (acc: T, cur: T) -> T) -> T | foldl1: first element as init |
| **reduceRight1**<T>(a: Array<T>, f: (cur: T, acc: T) -> T) -> T | foldr1: last element as init |

---

### Why your proposed reduce signature needs a small fix

You suggested:

- `reduce<T,U>(a: Array<T>, (prev: T, cur: T, index: Int) -> U) -> U`

Two issues:

1. **Accumulator type vs element type**  
   In a fold, the “previous” value is the **accumulator**, and its type is the **result type U**, not the element type T. So the callback should take `(prev: U, cur: T, index: Int) -> U`: we combine the current accumulator (U) with the current element (T) to produce the next accumulator (U). With `(prev: T, cur: T) -> U`, we’d be combining two elements and getting U, which doesn’t match the idea of “accumulate over the array into a U”.

2. **Initial value**  
   foldLeft needs an **initial accumulator** (e.g. 0 for sum, "" for string concat). Without it, we’d need a non-empty array and use the first element as seed, and then the result type would have to be T (a special case).

### Recommended signatures (match Haskell/Scala)

Use the **Haskell/Scala-style** signatures above (no index). Optional extension: some languages add an index argument; First could add that later if needed.

## Implementation notes

- **Runtime**: Needs a generic array ABI (e.g. pointer + length + element size or type tag) so append/insertAt/map can work for any T. Allocations for new arrays; for map, calling into First closures or a callback ABI.
- **Compiler**: Type-check these as stdlib/built-ins (or via a standard module); codegen calls into the runtime with the right types and, for map, closure/callback lowering.
- **Option / Result**: insertAt’s “or Error” or “Option<Array<T>>” may require adding Option/Result types to the language if not present.

This doc is the reference for the intended generic Array API; current Int-only intrinsics (arrayLength, arrayReduceIntSum, arrayMapIntDouble, arrayFilterIntPositive) remain as-is until the generic API is implemented.
