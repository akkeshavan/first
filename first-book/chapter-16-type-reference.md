# Chapter 16: Type reference

This chapter is a **reference for all types supported by First**: primitives, arrays, records, function types, type aliases, generic (parameterized) types, **algebraic data types (ADTs)**, union types, and **type-level programming** via interfaces and higher-kinded type parameters. **Refinement types** and **dependent types** are not covered here; they are described elsewhere in the language specification.

---

## 1. Primitive types

First provides a fixed set of **primitive (built-in) types**:

| Type     | Description              | Example literals / values      |
|----------|--------------------------|---------------------------------|
| **Int**  | 64-bit signed integer    | `0`, `42`, `-7`                 |
| **Float**| 64-bit floating point    | `0.0`, `3.14`, `-1.5`           |
| **Bool** | Boolean                  | `true`, `false`                 |
| **String** | UTF-8 string          | `"hello"`, `""`                 |
| **Unit** | Unit type (void)        | `()`                            |
| **Null** | Null type               | `null`                          |
| **ArrayBuf** | Mutable buffer (interaction-only) | — |

**Unit** is used for functions or interactions that return no meaningful value; the only value is `()`. **Null** is used in optional/union contexts (e.g. optional list tail, or `T | Null`).

```first
let n: Int = 42;
let x: Float = 3.14;
let ok: Bool = true;
let s: String = "hello";
let u: Unit = ();
let empty: Null = null;
```

---

## 2. Array types

An **array type** has the form **`Array<T>`**, where **T** is the element type. **T** can be any type: primitive, record, or another array.

```first
Array<Int>           // Array of integers
Array<String>        // Array of strings
Array<Bool>          // Array of booleans
Array<Array<Int>>    // Nested arrays (e.g. matrix of integers)
Array<Option<Int>>   // Array of optional integers (with Prelude)
```

**Array literals** use square brackets; the element type is inferred or checked against context:

```first
let xs: Array<Int> = [1, 2, 3];
let words: Array<String> = ["a", "b", "c"];
let grid: Array<Array<Int>> = [[1, 2], [3, 4]];
```

---

## 3. Record types

A **record type** is a product type: a fixed set of **named fields**, each with a type.

### 3.1 Inline record types

You can write a record type **inline** in a parameter or return type:

```first
function getX(p: { x: Int, y: Int }) -> Int {
    return p.x;
}

function makePoint(x: Int, y: Int) -> { x: Int, y: Int } {
    return { x: x, y: y };
}
```

Syntax: **`{ fieldName: Type, ... }`**. Field access: **`expr.fieldName`**.

### 3.2 Named record types (type aliases)

Give a record type a name with **type**:

```first
type Point = { x: Float, y: Float };
type Person = { name: String, age: Int, email: String };

function distance(p: Point, q: Point) -> Float {
    return sqrt((q.x - p.x) * (q.x - p.x) + (q.y - p.y) * (q.y - p.y));
}
```

### 3.3 Record literals

Build a value of a record type with **record literals**:

```first
let p: Point = { x: 1.0, y: 2.0 };
let alice: Person = { name: "Alice", age: 30, email: "alice@example.com" };
```

Records can contain **function-typed fields** (e.g. callbacks or interfaces):

```first
type Handler = {
    onSuccess: function(Int) -> Unit,
    onError: function(String) -> Unit
};
```

---

## 4. Function and interaction types

### 4.1 Pure function types

The type of a **pure function** is **`function(Param1, Param2, ...) -> ReturnType`**:

```first
function(Int, Int) -> Int
function(String) -> Bool
function() -> Unit
function(Array<Int>) -> Int
```

Example: a variable that holds a function from **Int** to **Int**:

```first
let f: function(Int) -> Int = function(n: Int) -> Int { return n + 1; };
```

### 4.2 Interaction types

Functions that perform **side effects** are declared with **interaction**. Their type is **`interaction(Param1, ...) -> ReturnType`**:

```first
interaction() -> Unit
interaction(String) -> Unit
interaction() -> String
```

Interactions are distinct from pure functions in the type system; you cannot pass an interaction where a pure function is expected.

---

## 5. Type aliases

A **type alias** introduces a name for an existing type (or a new record/ADT). Syntax: **`type Name = ... ;`**

```first
type Count = Int;
type Name = String;
type Point = { x: Float, y: Float };
type IntPair = { first: Int, second: Int };
```

Aliases do not create a new distinct type; they are interchangeable with the right-hand side. They are useful for clarity and for reusing complex types.

---

## 6. Generic (parameterized) types

A type can be **parameterized** by one or more **type parameters** in angle brackets. Such a type is called **generic** or **parameterized**.

### 6.1 Single type parameter

```first
type Option<T> = Some(T) | None;   // Prelude
type List<T> = Cons(T, List<T>);    // recursive list
```

- **Option&lt;T&gt;** — either **Some(value)** or **None**; **T** is the type of the value.
- **List&lt;T&gt;** — either **null** (empty) or **Cons(head, tail)**; **T** is the element type.

You get **Option&lt;Int&gt;**; **Option&lt;String&gt;**; **List&lt;Bool&gt;**; etc., from the same definition.

### 6.2 Multiple type parameters

```first
type Pair<A, B> = { first: A, second: B };
type Result<T, E> = Ok(T) | Err(E);
```

- **Pair&lt;Int, String&gt;** — a record **{ first: Int, second: String }**.
- **Result&lt;T, E&gt;** — either **Ok(value)** or **Err(error)**; **T** is success type, **E** is error type.

### 6.3 Using parameterized types

Values are built with constructors (for ADTs) or record literals (for record types):

```first
import * "Prelude";

let o: Option<Int> = some(42);
let p: Pair<Int, String> = { first: 1, second: "one" };
```

---

## 7. Algebraic data types (ADTs)

**Algebraic data types** are **sum types**: a value is exactly one of several **variants** (constructors). Each variant can carry **payload** types. ADTs are the main way to model “one of several shapes” in First.

### 7.1 Syntax

Declare an ADT with **type** and **constructors** separated by **|**:

```first
type Name = Constructor1(Payload1, ...) | Constructor2(Payload2, ...) | ... ;
```

Payload types can be named for documentation (e.g. **Some(value: T)**); the important part is the type. **None** has no payload: **None** or **None()**.

### 7.2 Option — two variants

**Option&lt;T&gt;** (from Prelude) is the standard ADT for an optional value:

```first
type Option<T> = Some(T) | None;
```

- **Some(x)** — carries a value of type **T**.
- **None** — no value (like “nothing there”).

```first
import * "Prelude";

function safeHead<T>(xs: Array<T>) -> Option<T> {
    return if (length(xs) == 0) { none() } else { some(xs[0]) };
}

function getOrZero(opt: Option<Int>) -> Int {
    return match opt {
        Some(n) => n,
        None => 0
    };
}
```

### 7.3 Result — success or error

**Result&lt;T, E&gt;** models a computation that either succeeds with **T** or fails with **E**:

```first
type Result<T, E> = Ok(T) | Err(E);

function divide(x: Int, y: Int) -> Result<Int, String> {
    return if (y == 0) {
        Err("division by zero")
    } else {
        Ok(x / y)
    };
}

function showResult(r: Result<Int, String>) -> String {
    return match r {
        Ok(n) => "Ok: " + intToString(n),
        Err(s) => "Err: " + s
    };
}
```

### 7.4 List — recursive ADT

**List&lt;T&gt;** is either empty (**null**) or **Cons(head, tail)**:

```first
type List<T> = Cons(T, List<T>);

// Empty list is null; non-empty is Cons(x, xs).
function sumList(xs: List<Int>) -> Int {
    return match xs {
        Cons(h, t) => h + sumList(t),
        null => 0
    };
}
```

### 7.5 Expression tree — multiple constructors

ADTs are ideal for **abstract syntax trees** or other tree-shaped data:

```first
type Expr =
    Lit(Int)
  | Var(String)
  | Add(Expr, Expr)
  | Mul(Expr, Expr);

function evalExpr(e: Expr) -> Int {
    return match e {
        Lit(n) => n,
        Var(_) => 0,
        Add(a, b) => evalExpr(a) + evalExpr(b),
        Mul(a, b) => evalExpr(a) * evalExpr(b)
    };
}
```

### 7.6 Binary tree

```first
type Tree<T> = Leaf | Node(T, Tree<T>, Tree<T>);

function size<T>(t: Tree<T>) -> Int {
    return match t {
        Leaf => 0,
        Node(_, l, r) => 1 + size(l) + size(r)
    };
}
```

### 7.7 JSON-like value

One ADT can represent several “kinds” of value:

```first
type Json =
    JNull
  | JBool(Bool)
  | JInt(Int)
  | JFloat(Float)
  | JString(String)
  | JArray(Array<Json>)
  | JObject(Array<{ key: String, value: Json }>);
```

### 7.8 Single constructor (wrapper)

An ADT with a single constructor is a **wrapper** or **newtype-style** type:

```first
type UserId = UserId(Int);
type NonEmptyString = NonEmpty(String);

function getUserId(u: UserId) -> Int {
    return match u { UserId(n) => n };
}
```

### 7.9 Exhaustive matching

When you **match** on an ADT, the compiler can check **exhaustiveness**: every constructor must be handled (or a catch-all used). This avoids forgetting a case when you add a new variant later.

```first
function describe(o: Option<Int>) -> String {
    return match o {
        Some(n) => "Some(" + intToString(n) + ")",
        None => "None"
    };
}
```

---

## 8. Union types

First supports **union types**: a value has **one of** several types. Syntax: **`T1 | T2 | ...`**

```first
type NumberOrString = Int | String;
type NullableInt = Int | Null;
```

Union types are useful for APIs that can return one of several types or for optional values (e.g. **T | Null**). Pattern matching or type checks can be used to narrow the type in each branch.

---

## 9. Interfaces and constrained generics (type-level programming)

**Interfaces** describe what operations a type must support. **Constrained type parameters** restrict a generic to types that implement a given interface. This is a form of **type-level programming**: the type system enforces that only “valid” types are used.

### 9.1 Interface declaration

```first
interface Eq<T> {
    eq: function(T, T) -> Bool;
}

interface ToString<T> {
    toString: function(T) -> String;
}
```

**Eq&lt;T&gt;** means: “type **T** has an equality test.” **ToString&lt;T&gt;** means: “type **T** can be converted to **String**.” Prelude provides these; built-in types (**Int**, **Float**, **Bool**, **String**) implement them.

### 9.2 Constrained type parameters

Use **`: InterfaceName`** to constrain a type parameter:

```first
function elem<T : Eq>(x: T, xs: List<T>) -> Bool {
    return match xs {
        Cons(h, t) => (h == x) || elem(x, t),
        null => false
    };
}
```

Here **T : Eq** means: “**T** can be any type that implements **Eq**.” Inside the function you may use **==** on values of type **T**. The compiler checks at call sites that the concrete type implements **Eq**.

### 9.3 Implementing an interface for your type

For your own ADT or record, you provide an **implementation**:

```first
type Point = { x: Int, y: Int };

implementation Eq<Point> {
    eq = function(p: Point, q: Point) -> Bool {
        return p.x == q.x && p.y == q.y;
    };
}
```

After that, **elem** (and any other **T : Eq** function) can be used with **Point**.

### 9.4 Multiple constraints

You can constrain a type parameter by multiple interfaces (syntax may allow **T : Eq, Ord** or similar). The type then must implement all of them.

---

## 10. Higher-kinded type parameters (type-level programming)

**Higher-kinded types** let you abstract over **type constructors** (like **Option** or **List**) rather than over plain types (like **Int**). This is another form of **type-level programming**: the kind of a parameter is **\* → \*** (or more).

### 10.1 Kind \* vs \* → \*

- **Kind \*** — a concrete type: **Int**, **String**, **Option&lt;Int&gt;**.
- **Kind \* → \*** — a type constructor that takes one type and returns a type: **Option**, **List**.

So **Option** has kind **\* → \***: you apply it to **Int** to get **Option&lt;Int&gt;** (kind **\***).

### 10.2 Declaring a higher-kinded parameter: F<_>

First uses **Scala-like** syntax: **`F<_>`** means “F is a type constructor of one argument”:

```first
import * "Prelude";

interface Functor<F<_>> {
    map: forall A B. function(F<A>, function(A) -> B) -> F<B>;
}
```

- **F&lt;_&gt;** says: **F** has kind **\* → \***.
- **map** takes **F&lt;A&gt;** and **A → B**, and returns **F&lt;B&gt;**.
- So the **type** of **map** is expressed in terms of **F**, **A**, and **B** — that is type-level programming.

### 10.3 Implementing Functor for Option

```first
function optionMap<A, B>(opt: Option<A>, f: function(A) -> B) -> Option<B> {
    return match opt {
        None => none(),
        Some(x) => some(f(x))
    };
}

implementation Functor<Option> {
    map = optionMap;
}
```

**implementation Functor&lt;Option&gt;** ties the **Option** type constructor to the **Functor** interface. After that, **Option&lt;T&gt;** can use **.map(f)** (or **map(opt, f)**) wherever the compiler can find this implementation.

### 10.4 Type-level summary

- **Constrained generics** (**T : Eq**) — restrict **T** to types that support certain operations.
- **Higher-kinded parameters** (**F&lt;_&gt;**) — abstract over type constructors so that interfaces like **Functor** can be defined once and implemented for **Option**, **List**, etc.

Both mechanisms work at the **type level**: they govern which types are valid and how they can be used, without runtime overhead.

---

## 11. Summary table

| Category        | Form / example                    | Notes                                      |
|-----------------|-----------------------------------|--------------------------------------------|
| Primitive       | **Int**, **Float**, **Bool**, **String**, **Unit**, **Null**, **ArrayBuf** | Built-in                                   |
| Array           | **Array&lt;T&gt;**                | Element type **T**                         |
| Record          | **{ f: T, ... }** or **type R = { ... }** | Named fields, product type                 |
| Function        | **function(T1, T2) -> R**         | Pure function type                         |
| Interaction     | **interaction(T1, ...) -> R**     | Side-effecting function type               |
| Type alias      | **type N = T;**                   | Name for an existing type                   |
| Generic         | **Option&lt;T&gt;**; **Pair&lt;A,B&gt;** | Parameterized types                        |
| ADT             | **type T = C1(A) \| C2(B) \| ...** | Sum type; exhaustive match                 |
| Union           | **T1 \| T2**                      | Value is one of several types              |
| Interface       | **interface I&lt;T&gt; { ... }**  | Constraint for type-level programming      |
| Constraint      | **T : Eq**                        | **T** must implement **Eq**                |
| Higher-kinded   | **F&lt;_&gt;**                    | **F** is a type constructor (\* → \*)      |

**Not covered in this chapter:** refinement types (e.g. **{{ x: Int where x > 0 }}**) and dependent types (types that depend on values). See the language specification for those.

---

**Try it:** The example project **examples/chapter-16-type-reference** contains small programs that use primitives, arrays, records, **Option**, a custom ADT, interfaces, and **Functor&lt;Option&gt;** so you can run and modify the examples from this chapter.
