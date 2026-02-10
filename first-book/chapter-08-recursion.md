# Chapter 8: Recursion and tail-call optimization

This chapter explains **recursion** in First: ordinary recursive functions (where the stack grows with each call) and **tail-recursive** functions that the compiler can optimize so the **stack stays bounded**. The runnable examples are in **examples/chapter-08-recursion**.

Run them with:

```bash
cd examples/chapter-08-recursion
fir run
```

---

## 1. What is recursion?

A function is **recursive** when it calls itself. Recursion is a natural way to express problems that break into smaller subproblems of the same shape.

**Example: factorial**

\[
n! = n \times (n-1) \times \cdots \times 1, \quad 0! = 1
\]

We can define it recursively: “factorial of n is n times factorial of n−1, and factorial of 0 or 1 is 1.”

From **examples/chapter-08-recursion/src/main.first**:

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

So each call **waits** for the inner call to finish. That means every call stays on the **call stack** until the innermost one returns. The stack has **one frame per recursive call**, so for large `n` the stack can grow large and you can hit stack overflow.

**Summary:** ordinary recursion is clear and correct, but the **stack grows with the depth** of recursion.

---

## 2. Tail recursion and tail-call optimization

A call is in **tail position** when it is the **last** thing the function does — its result is returned directly, with no further computation.

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

In the `else` branch, the only thing we do is `return factTail(n - 1, n * acc)`. So the recursive call is in **tail position**.

**Not a tail call:**

```first
return n * factorial(n - 1);   // not tail: we still multiply after the call returns
```

After `factorial(n - 1)` returns, we still have to do `n * ...` and then return, so that recursive call is **not** in tail position.

When every path that leads to a recursive call does so in tail position, the compiler can apply **tail-call optimization (TCO)**: it reuses the current stack frame for the callee (like a loop), so the **stack does not grow** with the number of tail calls.

---

## 3. Example 1 — Ordinary recursion: factorial (stack grows)

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
- **Why it’s not tail:** After `factorial(n - 1)` returns, we still multiply by `n` and then return.

---

## 4. Example 2 — Tail-recursive factorial (stack stays flat)

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

- **Behavior:** Same result as factorial: `factorialTail(n)` = n!. The **accumulator** `acc` carries the partial product; we maintain the invariant and recurse with `factTail(n - 1, n * acc)`.
- **Tail position:** In the `else` branch we only do `return factTail(n - 1, n * acc)`; no work after the call.
- **Stack:** With TCO, the compiler reuses the stack frame. Depth stays constant, so no stack growth even for large n.

---

## 5. Example 3 — Tail-recursive sum 1 + 2 + … + n

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

- **Behavior:** `sumTo(n)` = 1 + 2 + … + n. The accumulator holds the sum of the part already processed.
- **Tail position:** In the `else` branch we only `return sumAcc(n - 1, acc + n)`.
- **Stack:** With TCO, stack depth stays bounded.

---

## 6. Summary

| Kind of recursion        | After the recursive call returns… | Stack (with TCO)      |
|--------------------------|------------------------------------|------------------------|
| **Ordinary** (e.g. `n * factorial(n-1)`) | We still compute and then return   | Grows with depth       |
| **Tail** (e.g. `factTail(n-1, n*acc)`)   | We return that result as-is        | Stays bounded          |

- **Tail recursion** = recursive call is the last thing the function does (tail position).
- **Tail-call optimization** = compiler reuses the current stack frame for the callee, so the stack does not grow.
- **Accumulator pattern** = pass partial results (e.g. `acc`) so you can return the result of the recursive call directly, making the call tail.

By writing recursive functions in tail form (often with an accumulator), you get clear, recursive code that runs with **bounded stack** and avoids overflow on large inputs.

---

## Running the example

```bash
cd examples/chapter-08-recursion
fir run
```

Expected output (for n=10):

- `factorial(10) = 3628800`
- `factorialTail(10) = 3628800`
- `sumTo(10) = 1+...+n = 55`
