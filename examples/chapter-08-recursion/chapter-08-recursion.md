# Chapter 8: Recursion and tail-call optimization

This folder explains **recursion** and **tail-call optimization (TCO)** in First. The main source is **src/main.first**, with three runnable examples. The key idea: **tail recursion** lets the compiler reuse the current stack frame for the next call, so the **stack does not grow** even for many recursive steps.

---

## 1. What is recursion?

A function is **recursive** when it calls itself. Recursion is a natural way to express problems that break into smaller subproblems of the same shape.

**Example: factorial**

\[
n! = n \times (n-1) \times \cdots \times 1, \quad 0! = 1
\]

We can define it recursively: “factorial of n is n times factorial of n−1, and factorial of 0 or 1 is 1.”

In code (from **src/main.first**):

```first
function factorial(n: Int) -> Int {
    return if (n <= 1) {
        1
    } else {
        n * factorial(n - 1)
    };
}
```

- **Base case:** `n <= 1` → return 1 (stops the recursion).
- **Recursive case:** `n * factorial(n - 1)` → we need the result of `factorial(n - 1)` before we can multiply by `n` and return.

So each call **waits** for the inner call to finish. That means:

- `factorial(5)` calls `factorial(4)`, which calls `factorial(3)`, … down to `factorial(1)`.
- Every call stays on the **call stack** until the innermost one returns.
- The stack has **one frame per recursive call**, so for large `n` the stack can grow large and you can hit stack overflow.

So: **ordinary recursion** is clear and correct, but the **stack grows with the depth** of recursion.

---

## 2. What is tail recursion?

A call is in **tail position** when it is the **last** thing the function does — its result is returned directly as the result of the current function, with no further computation.

**Tail call (tail position):**

```first
function factTail(n: Int, acc: Int) -> Int {
    return if (n <= 1) {
        acc
    } else {
        factTail(n - 1, n * acc)   // tail call: result is returned as-is
    };
}
```

Here, in the `else` branch, the **only** thing we do is `return factTail(n - 1, n * acc)`. So the recursive call is in **tail position**.

**Not a tail call:**

```first
return n * factorial(n - 1);   // not tail: we still multiply after the call returns
```

After `factorial(n - 1)` returns, we still have to do `n * ...` and then return. So that recursive call is **not** in tail position.

**Tail recursion** means: every path that leads to a recursive call does so in tail position. Then the compiler can apply **tail-call optimization**.

---

## 3. How tail-call optimization works (and why the stack doesn’t grow)

When a call is in **tail position**, the current function’s job is done once it has computed the arguments and jumped to the callee. So:

- The current stack frame no longer needs to hold any useful data for “after the call.”
- The compiler (or runtime) can **reuse** the current frame for the callee: overwrite arguments, jump to the target, and **not** push a new frame.
- That is equivalent to a **loop**: “update variables and jump back to the top of the function.”

So instead of:

- `factTail(5,1)` → push frame, call `factTail(4,5)` → push frame, call `factTail(3,20)` → … (stack grows),

we get:

- `factTail(5,1)` → reuse same frame as `factTail(4,5)` → reuse same frame as `factTail(3,20)` → … (stack size stays the same).

So **tail-call optimization** turns tail recursion into something like a loop: the stack depth stays **bounded** (often just one frame), and we avoid stack overflow even for very large `n`.

In First, when you write `return someFunction(...)` and the compiler recognizes a direct tail call, it can emit a **tail call** (e.g. LLVM `tail` call), so the callee can return directly to the original caller and the stack does not grow.

---

## 4. Example 1 — Ordinary recursion: factorial (stack grows)

```first
function factorial(n: Int) -> Int {
    return if (n <= 1) {
        1
    } else {
        n * factorial(n - 1)
    };
}
```

- **Behavior:** Correct: `factorial(n)` = n!.
- **Stack:** Each call pushes a new frame and waits for `factorial(n - 1)`. Depth ≈ n. For large n, the stack can overflow.
- **Why it’s not tail:** After `factorial(n - 1)` returns, we still multiply by `n` and then return, so the recursive call is not in tail position.

---

## 5. Example 2 — Tail recursion: factorial with accumulator (stack stays flat)

```first
function factTail(n: Int, acc: Int) -> Int {
    return if (n <= 1) {
        acc
    } else {
        factTail(n - 1, n * acc)
    };
}
function factorialTail(n: Int) -> Int {
    return factTail(n, 1);
}
```

- **Behavior:** Same result as factorial: `factorialTail(n)` = n!. The **accumulator** `acc` carries “partial product so far”: we maintain `acc = n × (n−1) × … × (k+1)` and recurse with `factTail(k, acc)`.
- **Tail position:** In the `else` branch we only do `return factTail(n - 1, n * acc)`; no work after the call. So the recursive call is in tail position.
- **Stack:** With TCO, the compiler reuses the stack frame. Depth stays constant (e.g. one frame), so no stack growth even for large n.

---

## 6. Example 3 — Tail recursion: sum 1 + 2 + … + n (stack stays flat)

```first
function sumAcc(n: Int, acc: Int) -> Int {
    return if (n <= 0) {
        acc
    } else {
        sumAcc(n - 1, acc + n)
    };
}
function sumTo(n: Int) -> Int {
    return sumAcc(n, 0);
}
```

- **Behavior:** `sumTo(n)` = 1 + 2 + … + n. The accumulator `acc` holds the sum of the part already processed; we add `n` and recurse with `sumAcc(n - 1, acc + n)`.
- **Tail position:** In the `else` branch we only `return sumAcc(n - 1, acc + n)`. So the recursive call is in tail position.
- **Stack:** With TCO, stack depth stays bounded; no growth with n.

---

## 7. Example 4 — List length (from Chapter 7): tail recursion on data structures

In **chapter-07-Intro-to-generic-types** we have a tail-recursive list length:

```first
function length<T>(xs: List<T>) -> Int {
    return lengthAcc(xs, 0);
}
function lengthAcc<T>(xs: List<T>, acc: Int) -> Int {
    return match xs {
        Cons(h, t) => lengthAcc(t, acc + 1),
        Nil => acc
    };
}
```

- **Behavior:** `lengthAcc` walks the list and adds 1 to `acc` for each `Cons`; at `Nil` it returns `acc`. So the result is the length.
- **Tail position:** In the `Cons` branch we only `return lengthAcc(t, acc + 1)`. So the recursive call is in tail position.
- **Stack:** With TCO, the stack does not grow with the length of the list. Without TCO, a long list would add one frame per element and could overflow.

So the same idea (accumulator + tail call) applies to **recursion on data structures** (lists, trees, etc.): put the recursive call in tail position so the stack stays bounded.

---

## 8. Summary: how tail recursion helps keep the stack from growing

| Kind of recursion        | After the recursive call returns… | Stack behavior without TCO | With TCO      |
|--------------------------|-----------------------------------|-----------------------------|---------------|
| **Ordinary** (e.g. `n * factorial(n-1)`) | We still compute (e.g. multiply) and then return | Grows with depth (e.g. n frames) | N/A (not a tail call) |
| **Tail** (e.g. `factTail(n-1, n*acc)`)   | We return that result as-is       | Would grow with depth       | Stays bounded |

- **Tail recursion** = recursive call is the last thing the function does (tail position).
- **Tail-call optimization** = compiler reuses the current stack frame for the callee (like a loop), so the stack **does not grow** with the number of tail calls.
- **Accumulator pattern** = pass partial results (e.g. `acc`) so you can return the result of the recursive call directly, making the call tail.

By writing recursive functions in tail form (often with an accumulator), you get clear, recursive code that runs with **bounded stack** and avoids overflow on large inputs.

---

## Running the example

From the **repository root** or this directory:

```bash
cd examples/chapter-08-recursion
../../tools/fir build
../../tools/fir run
```

Or from repo root:

```bash
./build/bin/firstc examples/chapter-08-recursion/src/main.first -o build/ch08
./build/ch08
```

Expected output (for n=10):

- `factorial(10) = 3628800`
- `factorialTail(10) = 3628800`
- `sumTo(10) = 1+...+n = 55`

*Note:* If the build reports an LLVM attribute error on these functions, the compiler may need to avoid adding `readonly` to functions that use tail calls. The code and the explanation above are still correct.
