# Chapter 2: Functions and Interactions

First separates code into **functions** (pure, no side effects) and **interactions** (can perform I/O, use mutable state, and other effects). This chapter explains the distinction and shows how to define and use both.

---

## Functions: pure computation

A **function** in First is **pure**: given the same arguments, it always returns the same result and does not change the world (no I/O, no mutable global state). You use the keyword **function** to define one:

```first
function add(x: Int, y: Int) -> Int {
    return x + y;
}
```

- **function** – declares a pure function.
- **add** – name.
- **(x: Int, y: Int)** – parameters and types.
- **-> Int** – return type.
- The body is an expression or block; no **print**, no file access, no mutable variables.

Pure functions can only call other pure functions. They cannot call **print**, **println**, or any other interaction.

**Example: a pure helper**

```first
function double(n: Int) -> Int {
    return n * 2;
}
```

You can call **double** from other functions or from interactions.

---

## Interactions: side effects

An **interaction** is a piece of code that is allowed to have **side effects**: reading or writing the console, files, or the network, and using mutable variables or loops. You use the keyword **interaction** to define one:

```first
interaction main() -> Unit {
    println("Result: " + intToString(double(10)));
}
```

- **interaction** – declares a side-effecting entry point.
- **main** – name (often **main** for the program entry).
- **()-> Unit** – no parameters, returns nothing (Unit).
- The body can call **print**, **println**, and other interactions, and can call **function**s like **double**.

Interactions can call both other interactions and pure functions. The program’s entry point (e.g. **main**) must be an interaction so it can do I/O.

---

## When to use which

| Use **function** when …           | Use **interaction** when …        |
|-----------------------------------|-----------------------------------|
| Logic depends only on arguments   | You need to print or read input   |
| No I/O, no mutable globals        | You need files or network         |
| You want easy testing and reuse  | You need mutable variables/loops |
| Result is determined by inputs   | You coordinate effects (e.g. main)|

Keeping most logic in **function**s and using **interaction**s only at the “edges” (I/O, main) makes programs easier to reason about and test.

---

## Complete example: pure function and interaction

The following program defines a pure function **greet** that builds a string, and an interaction **main** that prints it:

```first
// Pure: no I/O, same input -> same output
function greet(name: String) -> String {
    return "Hello, " + name + "!";
}

// Interaction: can call print/println and call pure functions
interaction main() -> Unit {
    let msg = greet("First");
    println(msg);
}
```

Output: `Hello, First!`

**greet** is pure (string concatenation only). **main** is an interaction so it can call **println** and **greet**.

A runnable version of this pattern is in **examples/chapter-02-functions-and-interactions**.

---

## Rules at a glance

1. **Functions** are pure: no **print**/ **println**, no mutable **var**, no **while** inside them. They can only call other **function**s.
2. **Interactions** can do I/O, use **var** and **while**, and call both **function**s and other **interaction**s.
3. The entry point of a program (e.g. **main**) must be an **interaction** so it can perform effects.
4. Prefer **function** for most logic; use **interaction** when you need effects.

---

## Try it

From the repo root, run the chapter example:

```bash
cd examples/chapter-02-functions-and-interactions
fir run
```

Edit **src/main.first** to add another pure function (e.g. **triple(n: Int) -> Int**) and call it from **main** with **println** to see the split between pure logic and interaction.
