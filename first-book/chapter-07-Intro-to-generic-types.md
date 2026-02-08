# Chapter 7: Generic data types and a Haskell-like List

This chapter introduces **generic data types** and **interfaces** in First, and shows how to build a small **Haskell-like list library** using only **List&lt;T&gt;** with standard names: `cons`, `isEmpty`, `length`, `elem`, `head`, and `tail`.

---

## 1. Generic data types

First lets you define types that are parameterized by other types. Such a type is called a **generic** (or **parametric**) data type.

### 1.1 Defining a generic type

You declare a type and one or more **type parameters** in angle brackets, then define the type in terms of those parameters:

```first
type List<T> = Cons(T, List<T>);
```

Here:

- **`List<T>`** is a type that depends on **`T`** (the type of the list elements).
- **`Cons(T, List<T>)`** is a constructor that holds one value of type **`T`** and the rest of the list, which has type **`List<T>`**.

So you get **`List<Int>`**, **`List<String>`**, **`List<Bool>`**, and so on, all from the same definition.

### 1.2 Empty list: null

In First the empty list is not a separate constructor; it is the value **`null`**. So a value of type **`List<T>`** is either:

- **`null`** (empty list), or  
- **`Cons(x, xs)`** where **`x : T`** and **`xs : List<T>`**.

This is the same idea as a linked list: either “no node” (null) or “one cell and the rest of the list” (Cons).

---

## 2. Interfaces and constrained generics

Some list operations need to compare elements (for example, “is this element in the list?”). That requires **equality** on the element type. In First we express that with an **interface** and a **constraint** on the type parameter.

### 2.1 The Eq interface (from the standard library)

The standard library (**lib/Prelude.first**) declares **Eq**, **Ord**, and **Iterator**. Import it so you can use constrained generics:

```first
import * "Prelude";
```

**Prelude** defines interfaces such as:

```first
interface Eq<T> {
    eq: function(T, T) -> Bool;
}
```

So any type that “implements” **Eq** must provide a way to compare two values of that type for equality. The compiler treats **Int**, **Float**, **Bool**, and **String** as implementing **Eq**; you do not need to write an implementation for those. For your own type **T** that **List** uses, you provide **implementation Eq&lt;T&gt;** { ... } (or use a built-in type).

### 2.2 Constrained type parameters

When you define a function that needs equality on the element type, you **constrain** the type parameter with **`: Eq`**:

```first
function elem<T : Eq>(x: T, xs: List<T>) -> Bool {
    return match xs {
        Cons(h, t) => (h == x) || elem(x, t),
        null => false
    };
}
```

Here **`T : Eq`** means: “**T** can be any type that implements the **Eq** interface.” Inside the function you may use **`==`** on values of type **T**. At the call site the compiler checks that the type you use (e.g. **Int**) does implement **Eq**.

---

## 3. Building a Haskell-like list library

You can implement a small list library that follows the same **names and roles** as in Haskell. The list type is **`List<T>`**; the empty list is **`null`**; non-empty lists are **`Cons(head, tail)`**.

### 3.1 Naming (Haskell vs First)

| Haskell | First       | Meaning                          |
|---------|-------------|-----------------------------------|
| `(:)`   | **`cons`**  | Add an element to the front      |
| `null`  | **`isEmpty`** | True if the list is empty (First uses `null` for the value, so the predicate is `isEmpty`) |
| `length`| **`length`** | Number of elements              |
| `elem`  | **`elem`**  | Is this element in the list? (needs **Eq**) |
| `head`  | **`head`**  | First element (with a default for empty) |
| `tail`  | **`tail`**  | All but the first element        |

### 3.2 Type signatures and implementations

All of these can be written as **generic** functions over **`List<T>`**:

- **`cons<T>(x: T, xs: List<T>) -> List<T>`**  
  Build a list with **x** at the front and **xs** as the rest.

- **`isEmpty<T>(xs: List<T>) -> Bool`**  
  Return **true** when **xs** is **null**, **false** when it is **Cons(_, _)**.

- **`length<T>(xs: List<T>) -> Int`**  
  Match on **null** (return 0) and **Cons(_, t)** (return 1 + length(t)).

- **`elem<T : Eq>(x: T, xs: List<T>) -> Bool`**  
  Requires **T** to implement **Eq**. Match on **null** (false) and **Cons(h, t)** (true if **h == x** or **elem(x, t)**).

- **`head<T>(xs: List<T>, defaultVal: T) -> T`**  
  Return the first element when **xs** is **Cons(h, _)**; otherwise return **defaultVal**. (In Haskell, **head** is partial; here we make it total with a default.)

- **`tail<T>(xs: List<T>) -> List<T>`**  
  Return the rest when **xs** is **Cons(_, t)**; otherwise return **null**.

### 3.3 Pattern matching on List&lt;T&gt;

Every list function does the same thing: **match** on the list.

- **Empty list:** **`null`** (or **`null => ...`** in a match).
- **Non-empty list:** **`Cons(h, t)`** where **h** is the first element and **t** is the rest.

Example:

```first
function length<T>(xs: List<T>) -> Int {
    return match xs {
        Cons(h, t) => 1 + length(t),
        null => 0
    };
}
```

Recursion on **t** walks the list; the **null** case ends the recursion.

---

## 4. Summary

- **Generic data types** are declared with type parameters: **`type List<T> = Cons(T, List<T>);`**. You get **List&lt;Int&gt;**, **List&lt;String&gt;**, etc., from one definition.
- The **empty list** is **`null`**; a non-empty list is **`Cons(head, tail)`**.
- **Interfaces** (e.g. **Eq&lt;T&gt;** ) describe what operations a type must support. The compiler treats **Int**, **Float**, **Bool**, and **String** as implementing **Eq**.
- **Constrained generics** (**`T : Eq`**) let you write functions that work for any type that implements an interface (e.g. **elem** for lists of comparable elements).
- You can build a **Haskell-like list library** with generic **List&lt;T&gt;** and the same *concepts* as in Haskell: **cons**, **isEmpty** (Haskell’s **null**), **length**, **elem**, **head**, **tail**, all as generic functions.

The **chapter-07-Intro-to-generic-types** example (in **examples/chapter-07-Intro-to-generic-types/**) imports **Prelude** and defines this list type and these functions. Build from the **repository root** so the compiler finds **lib/Prelude.first**:

```bash
./build/bin/firstc examples/chapter-07-Intro-to-generic-types/src/main.first -o build/ch07
./build/ch07
```

From here you can extend the library with more list operations (e.g. **map**, **filter**, **fold**) following the same pattern: generic **List&lt;T&gt;** and pattern matching on **null** and **Cons(h, t)**.
