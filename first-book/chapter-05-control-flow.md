# Chapter 5: Control Flow

This chapter introduces **if expressions** (Rust-style conditionals that produce a value), the **for-in** loop for iterating over a **range** or other sequences, and **range expressions**—including ranges with a **custom step**. You will then build a classic **FizzBuzz** program: a pure function that computes the FizzBuzz string for a number, and an interaction that uses **for-in** over a range to print the result for each value.

---

## 1. The if expression

In First, **if is an expression**, not a statement. The whole `if (condition) { ... } else { ... }` has a **value**: the value of whichever branch is taken. This is the same idea as in Rust.

### Syntax

- **Single-expression branches:** `if (condition) expr1 else expr2`
- **Block branches:** the value of a block is its **last expression** (no semicolon after it). You can use `let` and other statements before that final expression.

### Rules

- **All branches must have the same type.** The type of the whole if expression is that common type.
- **Else if** is supported: after `else` you can write another `if (condition) branch` instead of a final `else branch`.
- To return a value from a function, **return the if expression**: `return if (...) { ... } else { ... };` — do not use `return` inside the branches.

### Examples

```first
// Single-expression branches
let result = if (x > 0) "positive" else "non-positive";

// Block branches: value = last expression in the block
let n = if (flag) {
    let a = 1;
    let b = 2;
    a + b
} else {
    0
};

// Else if
let kind = if (x < 0) { "negative" } else if (x > 0) { "positive" } else { "zero" };

// Returning from a function
function factorial(n: Int) -> Int {
    return if (n <= 1) {
        1
    } else {
        n * factorial(n - 1)
    };
}
```

---

## 2. The for-in loop

**For-in** lets you iterate over a sequence. In First, for-in is **only allowed in interactions** (not in pure functions), because looping is considered an effect for the purpose of the function/interaction split.

### Syntax

```first
for variable in iterable {
    statements
}
```

The **iterable** can be:

- A **range** (see below), which yields **Int** values.
- An **array**, which yields elements of the array’s type.

### Examples

```first
// Iterate over a range (see next section)
for i in 1..5 {
    println(intToString(i));
}

// Iterate over an array
let nums = [10, 20, 30];
for x in nums {
    println(intToString(x));
}
```

---

## 3. The range object

A **range** represents a sequence of integers. It is used in **for-in** to control how many times the loop runs and what value the loop variable takes.

### Creating a range

You write a range as an expression. Two forms are supported:

| Form | Meaning | Step |
|------|--------|------|
| `start..end` | From `start` up to but **not including** `end` | 1 |
| `start..=end` | From `start` **through** `end` (inclusive) | 1 |
| `start, second..end` | From `start` toward `end`; step = **second − start** | second − start |
| `start, second..=end` | Same, but **end** is included | second − start |

- Both bounds (and the optional **second** value) must be **Int**.
- **Step** is computed as `second - first` when you use the `first, second..last` form. That lets you write counting-up or counting-down ranges with a custom step.

### Examples

```first
// 1, 2, 3, 4 (exclusive end)
for i in 1..5 {
    println(intToString(i));
}

// 1, 2, 3, 4, 5 (inclusive end)
for i in 1..=5 {
    println(intToString(i));
}

// Custom step: 1, 3, 5, 7, 9 (step = 3 - 1 = 2)
for i in 1, 3..10 {
    println(intToString(i));
}

// Count down: 10, 8, 6, 4, 2 (step = 8 - 10 = -2)
for i in 10, 8..0 {
    println(intToString(i));
}
```

So: **range** is the object you get from these expressions; **for-in** consumes that range and binds the loop variable to each integer in sequence.

---

## 4. FizzBuzz: a pure function and for-in

**FizzBuzz** is a classic exercise: for an integer `n`,

- if `n` is divisible by 15, return `"FizzBuzz"`;
- else if divisible by 3, return `"Fizz"`;
- else if divisible by 5, return `"Buzz"`;
- else return the string form of `n`.

We implement it as a **pure function** (no I/O, no mutation). Then we use an **interaction** that uses **for-in** over a range **1 to 30** and **println**s the result of applying **fizzBuzz** to each value.

### Pure function: fizzBuzz

```first
function fizzBuzz(n: Int) -> String {
    return if (n % 15 == 0) {
        "FizzBuzz"
    } else if (n % 3 == 0) {
        "Fizz"
    } else if (n % 5 == 0) {
        "Buzz"
    } else {
        intToString(n)
    };
}
```

- `%` is the modulo operator; `n % 15 == 0` means “n is divisible by 15”.
- We check 15 first, then 3, then 5, so that multiples of 15 get `"FizzBuzz"` and not `"Fizz"` or `"Buzz"`.
- The **else** branch uses **intToString(n)** to turn the number into a string.

### Main: for-in over range 1 to 30

We call **fizzBuzz** for each number in the range **1..=30** (1 through 30) and print the result. Because we use **println** and **for-in**, this must be an **interaction**.

```first
interaction main() -> Unit {
    for i in 1..=30 {
        println(fizzBuzz(i));
    }
}
```

Putting it together:

```first
function fizzBuzz(n: Int) -> String {
    return if (n % 15 == 0) {
        "FizzBuzz"
    } else if (n % 3 == 0) {
        "Fizz"
    } else if (n % 5 == 0) {
        "Buzz"
    } else {
        intToString(n)
    };
}

interaction main() -> Unit {
    for i in 1..=30 {
        println(fizzBuzz(i));
    }
}
```

Running this program prints the classic FizzBuzz output for 1 through 30.

---

## Summary

| Concept | Syntax / idea |
|--------|----------------|
| **If expression** | `if (cond) expr1 else expr2` or block branches; value = last expression in the chosen branch. |
| **For-in** | `for x in iterable { ... }` — only in interactions; iterable = range or array. |
| **Range (default step)** | `start..end` (exclusive end), `start..=end` (inclusive end); step 1. |
| **Range (custom step)** | `start, second..end` or `start, second..=end`; step = second − start. |
| **FizzBuzz** | Pure function `fizzBuzz(n: Int) -> String`; main uses `for i in 1..=30 { println(fizzBuzz(i)); }`. |

---

## Runnable example: chapter-05-control-flow

The project **examples/chapter-05-control-flow** contains:

1. **fizzBuzz** – the pure function above.
2. **main** – an interaction that loops over **1..=30** and prints **fizzBuzz(i)** for each **i**.

From the repo root:

```bash
cd examples/chapter-05-control-flow
fir run
```

You should see the FizzBuzz lines for 1 through 30.

---

## Try it

- Change the range in **main** to **1..=15** or **1, 2..=20** (evens from 2 to 20) and run again.
- Add another interaction that uses **for i in 10, 8..0** and prints **fizzBuzz(i)** to see FizzBuzz in reverse with step −2.
- Use **if** expressions in a small pure function (e.g. “absolute value” or “sign”) and call it from an interaction.

Control flow in First stays expression-oriented (if) and loop-over-sequences (for-in over range or array), with a clear split between pure functions and interactions that do I/O or loops.
