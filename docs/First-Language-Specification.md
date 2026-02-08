# First Programming Language Specification

## Overview

First is a **functional-first programming language** that emphasizes pure functional programming while providing **controlled access to imperative features**. It features a strict distinction between pure functions and side-effecting interactions, strong typing with TypeScript-like expressiveness, and Haskell-style type classes.

## 1. Language Fundamentals

### 1.1 Paradigm
- **Functional-first**: Primary programming paradigm emphasizing pure functions
- **Controlled imperative**: Imperative features (mutable state, loops) restricted to interaction functions
- **Strong typing**: Static type checking with comprehensive type inference

### 1.2 Key Principles
- **Pure functions by default**: Functions are pure unless explicitly marked as interactions
- **Explicit side effects**: Side effects are contained within interaction functions
- **Immutable by default**: Variables are immutable unless explicitly marked as mutable
- **Effect isolation**: Mutable state and loops are restricted to interaction functions only
- **Memory safety**: Guaranteed through the type system and effect restrictions

## 2. Syntax and Grammar

### 2.1 Basic Syntax
```
// Comments
/* Multi-line comments */

// Variable declarations
let x: Int = 10;          // Immutable (allowed everywhere)
let z = 30;               // Immutable with type inference (allowed everywhere)

// Mutable variables - ONLY allowed in interaction functions
interaction processData(): Unit {
    var y: Int = 20;      // Mutable (only allowed in interactions)
    y = 25;               // Assignment (only allowed in interactions)
}

// This would cause a compile error:
function pureFunction(): Int {
    var invalid: Int = 5; // ERROR: mutable variables not allowed in pure functions
    return invalid;
}
```

### 2.2 Identifiers
- Start with letter or underscore
- Followed by letters, digits, or underscores
- Case-sensitive
- Keywords are reserved

### 2.3 Keywords
```
let var function interaction if else return
for in type interface implementation import export
true false null Int String Bool Float Array
```

## 3. Type System

### 3.1 Primitive Types
- `Int`: 64-bit signed integers
- `Float`: 64-bit floating point
- `Bool`: Boolean values (`true`, `false`)
- `String`: UTF-8 encoded strings
- `Unit`: Void type (represented as `()`)
- `Null`: Null type (value `null`); used in union types like `T | Null`

### 3.2 Composite Types

#### 3.2.1 Arrays
```
Array<Int>           // Array of integers
Array<String>        // Array of strings
Array<Array<Int>>    // Nested arrays
```

#### 3.2.2 Record Types
```
type Person = {
    name: String,
    age: Int,
    email: String
};

type Calculator = {
    add: function(Int, Int) -> Int,
    multiply: function(Int, Int) -> Int
};
```

#### 3.2.3 Function Types
```
// Pure functions
function(Int, Int) -> Int
function(String) -> Bool
function() -> Unit

// Interactions (side-effecting functions)
interaction(String) -> Unit
interaction() -> String
```

#### 3.2.4 Generic Types
```
type Option<T> = Some(T) | None;
type Result<T, E> = Ok(T) | Err(E);
type Pair<A, B> = { first: A, second: B };
```

#### 3.2.5 Union Types
```
type Status = "loading" | "success" | "error";
type NumberOrString = Int | String;
```

#### 3.2.6 Algebraic Data Types (Sum Types)

**Case-class style** (constructors with payload types; exhaustive match required). Parameter names in the declaration are optional and document the payload:

```
type LexItem = Expression(value: Int) | Operator(op: String);
type Expression = Term(Int) | Operator(String) | BinaryExpression(Expression, Operator, Expression);
match e {
    Term(value) => ...,
    Operator(op) => ...,
    BinaryExpression(left, op, right) => ...
}
```

**Sum-of-records style** (each variant carries a named record type; default case required):

```
type Circle = { radius: Float };
type Rectangle = { len: Float, width: Float };
type Shape = Circle | Rectangle;   // sugar for Circle(Circle) | Rectangle(Rectangle)

match v {
    Circle({ radius }) => ...,
    Rectangle({ len, width }) => ...,
    other => ...
}
```

- **Construction:** `Circle({ radius: 5.0 })`, `Rectangle({ len: 10.0, width: 20.0 })`.
- **Record pattern shorthand:** `{ radius }` means `{ radius: radius }` (bind field to variable of same name).
- **Exhaustiveness:** Case-class style requires all constructors covered (or a catch-all). Sum-of-records requires a default (variable or `_`) case.

#### 3.2.7 Refinement Types
Refinement types allow you to specify types with predicates that constrain values:

```first
// Basic refinement type syntax: {{variable: BaseType where predicate}}
type PositiveInt = {{x: Int where x > 0}};
type NonZeroInt = {{n: Int where n != 0}};
type BoundedInt = {{x: Int where x >= 0 && x <= 100}};
type Grade = {{x: Int where x in {0, 1, 2, 3, 4}}};

// Refinement types can be used in function parameters and return types
function safeDivide(x: Int, y: {{n: Int where n != 0}}): Int {
    return x / y;
}

function increment(n: PositiveInt): PositiveInt {
    return n + 1;
}
```

**Refinement Predicates:**
- Equality: `x = value`, `x != value`
- Comparisons: `x < value`, `x <= value`, `x > value`, `x >= value`
- Range: `min <= x <= max`
- Membership: `x in {value1, value2, ...}`
- Logical: `predicate1 && predicate2`, `predicate1 || predicate2`, `!predicate`
- Arbitrary expressions: Any boolean expression involving the refined variable

#### 3.2.8 Dependent Types
Dependent types allow types to depend on values, enabling more precise type specifications.

**Dependent Function Types (Pi Types):**
```first
// Syntax: (paramName: ParamType) -> ReturnType
// The return type can reference the parameter name
type VectorConstructor = (n: Int) -> Vector[n];
type MatrixMultiply = (n: Int) -> (m: Int) -> (p: Int) -> Matrix[n][m] -> Matrix[m][p] -> Matrix[n][p];

// Dependent function types can be used in function return types
function vectorLength<n>(v: Vector[n]): (x: Int) -> {{x: Int where x = n}} {
    return function(x: Int) {
        return x;  // Return type ensures x = n
    };
}
```

**Dependent Pair Types (Sigma Types):**
```first
// Syntax: (varName: VarType) * BodyType
// The body type can reference the variable name
type DependentPair = (n: Int) * Array<Int>[n];
type NestedPair = (n: Int) * ((m: Int) * Array<Int>[n + m]);
```

**Indexed Types:**
```first
// Syntax: BaseType[index1, index2, ...]
type Vector<n> = Array<Int>[n];
type Matrix<n, m> = Array<Array<Int>[m]>[n];

// Indexed types can use variables, literals, or expressions
function createVector<n>(size: n): Vector[n] {
    // Implementation
}
```

#### 3.2.9 Intersection and Union Types
```first
// Intersection types: T1 & T2 & ...
type ReadableWritable = Readable & Writable;

// Union types: T1 | T2 | ...
type StringOrNumber = String | Int;

// T | Null for optional values (Iterator.next returns T | null)
interface Iterator<T> {
    next: function() -> T | Null;
}
```

#### 3.2.10 Existential Types
```first
// Syntax: exists varName: VarType. BodyType
type Container = exists T: Type. {value: T, size: Int};
```

### 3.3 Type Inference
The compiler uses Hindley-Milner type inference to deduce types:
```
let add = function(x, y) { return x + y; };  // Inferred: function(Int, Int) -> Int
let numbers = [1, 2, 3];                     // Inferred: Array<Int>
```

## 4. Expressions and Operators

### 4.1 Arithmetic Expressions
```
Precedence (highest to lowest):
1. () [] .               // Grouping, array access, field access
2. - + ! (unary)         // Unary minus, plus, logical not
3. * / %                 // Multiplication, division, modulo
4. + -                   // Addition, subtraction
5. < <= > >=             // Comparison
6. == !=                 // Equality
7. &&                    // Logical and
8. ||                    // Logical or
9. <$> <*>               // Monadic operators (interactions only)
10. >>=                  // Bind operator (interactions only)
11. >>                   // Then operator (interactions only)
12. = += -= *= /=        // Assignment
```

### 4.2 Operator Examples
```
let result = (a + b) * c / d;
let isValid = x > 0 && y < 100;
let updated = record.{ field = newValue };

// Monadic operators (only in interactions)
interaction processData() -> Result<String, String> {
    return String.toUpperCase <$> readFile("data.txt") >>= writeFile("output.txt");
}
```

### 4.3 Short-circuit Evaluation
- `&&` and `||` operators use short-circuit evaluation
- `a && b`: if `a` is false, `b` is not evaluated
- `a || b`: if `a` is true, `b` is not evaluated

### 4.4 Monadic Operator Restrictions
- Monadic operators (`>>=`, `>>`, `<$>`, `<*>`) are **only available within interaction functions**
- Attempting to use monadic operators in pure functions results in a compile-time error
- This maintains the purity distinction and ensures side effects are properly contained

```
// ✅ Valid - monadic operators in interaction
interaction processFile() -> Result<String, String> {
    return readFile("input.txt") >>= function(content) {
        return Ok(content.toUpperCase());
    };
}

// ❌ Invalid - monadic operators in pure function
function processString(s: String) -> Result<String, String> {
    return Ok(s) >>= function(content) {  // Compile error!
        return Ok(content.toUpperCase());
    };
}
```

## 5. Control Flow

### 5.1 If / Else If / Else as Expression (Rust-style)

**If is an expression, not a statement.** The value of the `if` expression is the **last expression** in whichever branch is taken. This is the same as in Rust.

- **Branches** are either a **block** `{ stmt* expr? }` or a single **expression**.
- In a block, the last expression (without a trailing semicolon) is the value of the block; if there is no trailing expression, the block’s value is **Unit**.
- **Else if** is supported: after `else` you may write another `if (condition) branch` instead of a final `else branch`.
- **No `return` inside if-expression branches.** You may not use `return` inside the block of an if/else branch. To return a value from a function, return the whole if-expression: `return if (...) { ... } else { ... };`

```
// Single-expression branches (value = that expression)
let result = if (x > 0) "positive" else "non-positive";

// Block branches: value = last expression in the block (no return inside)
let n = if (flag) {
    let a = 1;
    let b = 2;
    a + b
} else {
    0
};

// Returning from a function: return the result of the if-expression
function factorial(n: Int) -> Int {
    return if (n <= 1) {
        1
    } else {
        n * factorial(n - 1)
    };
}

// Else if
let kind = if (x < 0) { "negative" } else if (x > 0) { "positive" } else { "zero" };

// Used as statement: evaluate and discard value (semicolon required)
if (condition) {
    doSomething();
} else {
    doOther();
};
```

**All branches must have the same type.** The type of the whole `if` expression is that common type.

### 5.2 For-In Loops (Rust-style) and Range Expressions

**For-in loops** iterate over any type that implements the `Iterator<T>` interface. They are **only allowed in interaction functions** (not in pure functions).

```
// Iterate over a range (exclusive end: 1, 2, 3, 4)
for i in 1..5 {
    print(intToString(i));
}

// Iterate over a range (inclusive end: 1, 2, 3, 4, 5)
for i in 1..=5 {
    print(intToString(i));
}

// Iterate over an array
let nums = [10, 20, 30];
for x in nums {
    print(intToString(x));
}
```

**Range expressions:**
- `start..end` – exclusive range (values from `start` up to but not including `end`)
- `start..=end` – inclusive range (values from `start` through `end`, inclusive)
- Both bounds must be `Int`

**Iterator<T> interface:**
```first
interface Iterator<T> {
    next: function() -> T | Null;
}
```

Types that implement `Iterator<T>` can be used in `for-in` loops. Built-in implementations:
- **Range** (from `start..end` or `start..=end`): implements `Iterator<Int>`
- **Array<T>**: implements `Iterator<T>`

## 6. Functions and Interactions

First makes a **strict distinction** between pure functions and side-effecting interactions. This separation enforces functional programming principles while allowing controlled imperative programming where needed.

### 6.1 Pure Functions

Pure functions are the **primary and preferred** way to write code in First. They guarantee **referential transparency** and **no side effects**.

**Restrictions in Pure Functions:**
- ❌ **No mutable variables** (`var` declarations)
- ❌ **No for-in loops** (only allowed in interactions)
- ❌ **No monadic operators** (`>>=`, `>>`, `<$>`, `<*>`)
- ❌ **No I/O operations** (reading files, printing, etc.)

**Allowed in Pure Functions:**
- ✅ **Immutable variables** (`let` declarations)
- ✅ **Function calls and recursion**
- ✅ **Pattern matching and conditionals**
- ✅ **Mathematical computations**
- ✅ **Data structure operations**

```first
// ✅ Pure function examples
function add(x: Int, y: Int) -> Int {
    return x + y;
}

function factorial(n: Int) -> Int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);  // Recursion instead of loops
    }
}

function processNumbers(nums: Array<Int>) -> Array<Int> {
    // Functional approach using map/filter
    return nums.map(function(x) { return x * 2; })
               .filter(function(x) { return x > 10; });
}

// Generic functions with type parameters
function identity<T>(x: T) -> T {
    return x;
}

// Function signatures (without body) - useful for interfaces and forward declarations
function vectorLength<n>(v: Vector[n]): (x: Int) -> {{x: Int where x = n}};
function safeDivide(x: Int, y: {{n: Int where n != 0}}): Int;
```

### 6.2 Interactions (Side-effecting Functions)

Interactions allow **controlled imperative programming** and are used when side effects are necessary.

**Everything Pure Functions Can Do, Plus:**
- ✅ **Mutable variables** (`var` declarations and assignments)
- ✅ **For-in loops** (iterate over ranges and arrays)
- ✅ **Monadic operators** (`>>=`, `>>`, `<$>`, `<*>`)
- ✅ **I/O operations** (file operations, printing, user input)
- ✅ **Side effects** and state modification

```first
// ✅ Interaction function examples
interaction greet(name: String) -> Unit {
    print("Hello, " + name + "!");  // I/O operation
}

interaction processWithState(): Int {
    var counter: Int = 0;           // Mutable variable
    if (counter < 10) {
        counter = counter + 1;      // Assignment
        print(counter.toString());  // I/O operation
    } else {
        return counter;
    };
    return counter;
}

interaction readInput() -> String {
    return input();                 // I/O operation
}

interaction processFile(filename: String) -> Result<String, String> {
    let content = readFile(filename);  // I/O operation
    if (content.isOk()) {
        return Ok(content.unwrap().toUpperCase());
    } else {
        return Err("Could not read file");
    }
}
```

### 6.3 Monadic Operators (Interaction Functions Only)

Monadic operators from Haskell are **strictly restricted to interaction functions**. They enable elegant composition of side-effecting operations while maintaining the purity of regular functions.

**These operators are FORBIDDEN in pure functions:**
- `>>=` (bind operator)
- `>>` (then operator) 
- `<$>` (functor map)
- `<*>` (applicative apply)

#### 6.3.1 Bind Operator (`>>=`)
```first
interaction processFiles(filenames: Array<String>) -> Result<Array<String>, String> {
    return filenames.traverse(function(filename) {
        readFile(filename) >>= function(content) {
            return Ok(content.toUpperCase());
        };
    });
}

// Alternative syntax with do notation
interaction processFilesWithDo(filenames: Array<String>) -> Result<Array<String>, String> {
    do {
        contents <- filenames.traverse(readFile);
        return Ok(contents.map(function(c) { return c.toUpperCase(); }));
    }
}
```

#### 6.3.2 Then Operator (`>>`)
```first
interaction setupAndRun() -> Unit {
    initializeSystem() >> 
    loadConfiguration() >> 
    startServer();
}
```

#### 6.3.3 Functor Map (`<$>`)
```first
interaction getFileLength(filename: String) -> Result<Int, String> {
    return String.length <$> readFile(filename);
}
```

#### 6.3.4 Applicative Apply (`<*>`)
```first
interaction combineFiles(file1: String, file2: String) -> Result<String, String> {
    return function(a, b) { return a + "\n" + b; } <$> 
           readFile(file1) <*> 
           readFile(file2);
}
```

### 6.4 Function Signatures

Function declarations can optionally omit the body to create function signatures. This is useful for:
- Interface definitions
- Forward declarations
- Type-only declarations

```first
// Function with body
function add(x: Int, y: Int): Int {
    return x + y;
}

// Function signature (no body) - ends with semicolon
function multiply(x: Int, y: Int): Int;

// Generic function signature
function vectorLength<n>(v: Vector[n]): (x: Int) -> {{x: Int where x = n}};

// Function signature with refinement types
function safeDivide(x: Int, y: {{n: Int where n != 0}}): Int;
```

**Note:** Function signatures are parsed but not yet fully implemented in the type checker. They are primarily used for documentation and interface definitions.

### 6.5 Function Design Guidelines

**Prefer Pure Functions:**
- Use pure functions whenever possible
- Leverage recursion instead of loops
- Use functional data transformation (map, filter, fold)
- Keep side effects isolated to interaction functions

**When to Use Interactions:**
- I/O operations (file system, network, user input)
- State management requiring mutation
- Performance-critical code requiring loops
- Integration with external systems

### 6.6 First-class Functions
```first
let operation: function(Int, Int) -> Int = add;
let numbers = [1, 2, 3, 4, 5];
let doubled = numbers.map(function(x) { return x * 2; });
```

### 6.7 Closures (Pure Functions Only)
```first
// Note: Closures with mutable state require interaction functions
function createMultiplier(factor: Int) -> function(Int) -> Int {
    return function(x: Int) { return x * factor; };  // Pure closure
}

interaction createCounter() -> function() -> Int {
    var count = 0;  // Mutable state requires interaction
    return function() {
        count += 1;
        return count;
    };
}
```

### 6.8 Semantic Restrictions Summary

First enforces **strict semantic restrictions** to maintain functional purity and effect isolation:

#### Pure Functions (`function` keyword)

**✅ ALLOWED:**
- Immutable variable declarations (`let`)
- Function calls and recursion
- Pattern matching and conditionals (`if`/`else`)
- Mathematical and logical operations
- Data structure operations (map, filter, fold)
- Pure computations and transformations

**❌ FORBIDDEN:**
- Mutable variable declarations (`var`)
- Variable assignments (`=`, `+=`, `-=`, etc.)
- Monadic operators (`>>=`, `>>`, `<$>`, `<*>`)
- I/O operations (print, readFile, input, etc.)
- Any side effects or state mutations

#### Interaction Functions (`interaction` keyword)

**✅ ALLOWED:**
- Everything pure functions can do, PLUS:
- Mutable variable declarations (`var`)
- Variable assignments and mutations
- Monadic operators for composing effects
- I/O operations and side effects
- State management and mutations

#### Compile-Time Enforcement

The compiler enforces these restrictions with clear error messages:

```first
// ❌ This will cause a compile error:
function invalidPure(): Int {
    var x: Int = 5;  // ERROR: Mutable variables can only be used in interaction functions
    return x;
}

// ✅ This is the correct way:
interaction validInteraction(): Int {
    var x: Int = 5;  // OK: Mutable variables allowed in interactions
    x = x + 1;
    return x;
}

// ✅ Pure alternative using recursion:
function pureFunctional(x: Int): Int {
    if (x < 10) {
        return pureFunctional(x + 1);  // Recursion instead of loops
    }
    return x;
}
```

This design ensures that:
1. **Pure functions remain truly pure** - no hidden side effects
2. **Side effects are explicit** - clearly marked with `interaction`
3. **Functional programming is encouraged** - pure functions are the default
4. **Imperative programming is available** - when needed in interactions

## 7. Records and Object-like Structures

### 7.1 Record Definition and Usage
```
type User = {
    id: Int,
    name: String,
    email: String,
    isActive: Bool
};

let user: User = {
    id: 1,
    name: "Alice",
    email: "alice@example.com",
    isActive: true
};

// Field access
let userName = user.name;

// Record update (creates new record)
let updatedUser = user.{ name = "Alice Smith" };
```

### 7.2 Records with Function Fields
```
type Calculator = {
    add: function(Int, Int) -> Int,
    subtract: function(Int, Int) -> Int,
    multiply: function(Int, Int) -> Int
};

let calc: Calculator = {
    add: function(x, y) { return x + y; },
    subtract: function(x, y) { return x - y; },
    multiply: function(x, y) { return x * y; }
};

let result = calc.add(5, 3);
```

## 8. Interfaces and Implementations (Type Classes)

### 8.1 Interface Definition
```
interface Show<T> {
    show: function(T) -> String;
}

interface Eq<T> {
    equals: function(T, T) -> Bool;
    notEquals: function(T, T) -> Bool;
}

interface Ord<T> extends Eq<T> {
    compare: function(T, T) -> Int;
    lessThan: function(T, T) -> Bool;
    greaterThan: function(T, T) -> Bool;
}

// Monadic interfaces (only usable in interactions)
interface Functor<F> {
    fmap: function<A, B>(function(A) -> B, F<A>) -> F<B>;
}

interface Applicative<F> extends Functor<F> {
    pure: function<A>(A) -> F<A>;
    apply: function<A, B>(F<function(A) -> B>, F<A>) -> F<B>;
}

interface Monad<M> extends Applicative<M> {
    bind: interaction<A, B>(M<A>, interaction(A) -> M<B>) -> M<B>;
    then: interaction<A, B>(M<A>, M<B>) -> M<B>;
}
```

### 8.2 Implementation
```
implementation Show<Int> {
    show = function(x: Int) -> String {
        return x.toString();
    };
}

implementation Eq<String> {
    equals = function(a: String, b: String) -> Bool {
        return a == b;
    };
    
    notEquals = function(a: String, b: String) -> Bool {
        return !(a == b);
    };
}

implementation Show<Array<T>> where Show<T> {
    show = function(arr: Array<T>) -> String {
        let elements = arr.map(show);
        return "[" + elements.join(", ") + "]";
    };
}

// Monadic implementations
implementation Monad<Result<T, E>> {
    bind = interaction(result: Result<T, E>, f: interaction(T) -> Result<U, E>) -> Result<U, E> {
        match result {
            Ok(value) => f(value),
            Err(error) => Err(error)
        }
    };
    
    then = interaction(result1: Result<T, E>, result2: Result<U, E>) -> Result<U, E> {
        match result1 {
            Ok(_) => result2,
            Err(error) => Err(error)
        }
    };
}

implementation Monad<IO<T>> {
    bind = interaction(io: IO<T>, f: interaction(T) -> IO<U>) -> IO<U> {
        // Internal implementation
    };
}
```

### 8.3 Using Interfaces
```
function printValue<T>(value: T) -> Unit where Show<T> {
    print(show(value));
}

// Usage
printValue(42);        // prints "42"
printValue("hello");   // prints "hello"
```

## 9. Module System

### 9.1 Module Definition
```
// math.first
export function square(x: Int) -> Int {
    return x * x;
}

export function cube(x: Int) -> Int {
    return x * x * x;
}

export type Point = {
    x: Float,
    y: Float
};
```

### 9.2 Module Import
```
// main.first
import { square, cube, Point } from "./math";
import * as Math from "./math";

let result = square(5);
let point: Point = { x: 1.0, y: 2.0 };
```

### 9.3 Native Library Integration

First provides seamless integration with native libraries (C, C++, Rust) through a foreign function interface (FFI) and automatic wrapper generation. This allows First programs to leverage existing native libraries while maintaining type safety and First's functional programming model.

#### 9.3.1 Foreign Function Interface (FFI)

The `foreign` keyword allows you to declare bindings to native library functions:

```
// C library binding example
foreign c "libm" {
    // Link against libm (math library)
    function sqrt(x: Float) -> Float;
    function sin(x: Float) -> Float;
    function cos(x: Float) -> Float;
    function pow(base: Float, exponent: Float) -> Float;
}

// Usage
import { sqrt, sin, cos, pow } from "./math-bindings";

function distance(x1: Float, y1: Float, x2: Float, y2: Float) -> Float {
    let dx = x2 - x1;
    let dy = y2 - y1;
    return sqrt(dx * dx + dy * dy);
}
```

#### 9.3.2 C Library Bindings

C libraries can be wrapped using the `foreign c` syntax:

```
// c-bindings.first - Wrapping libcurl for HTTP
foreign c "curl" {
    // C function: CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...);
    // Wrapped as:
    interaction curlEasySetopt(curl: CURLPtr, option: Int, value: String) -> Int;
    
    // C function: CURLcode curl_easy_perform(CURL *curl);
    interaction curlEasyPerform(curl: CURLPtr) -> Int;
    
    // C function: CURL *curl_easy_init(void);
    interaction curlEasyInit() -> CURLPtr;
    
    // Opaque pointer type
    type CURLPtr;
}

// Usage
import { curlEasyInit, curlEasySetopt, curlEasyPerform, CURLPtr } from "./c-bindings";

interaction fetchUrl(url: String) -> Result<String, String> {
    do {
        curl <- curlEasyInit();
        if (curl == null) {
            return Err("Failed to initialize curl");
        }
        
        _ <- curlEasySetopt(curl, 10002, url); // CURLOPT_URL
        result <- curlEasyPerform(curl);
        
        if (result == 0) {
            return Ok("Success");
        } else {
            return Err("HTTP request failed");
        }
    }
}
```

#### 9.3.3 C++ Library Bindings

C++ libraries can be wrapped using `foreign cpp`:

```
// cpp-bindings.first - Wrapping a C++ library
foreign cpp "mylib" {
    // C++ class wrapped as a type
    type MyClass;
    
    // Constructor
    interaction MyClass.new(value: Int) -> MyClass;
    
    // Method calls
    function MyClass.getValue() -> Int;
    interaction MyClass.setValue(value: Int) -> Unit;
    
    // Destructor (automatic via reference counting)
    interaction MyClass.delete() -> Unit;
}

// Usage
import { MyClass } from "./cpp-bindings";

interaction useMyClass() -> Int {
    do {
        obj <- MyClass.new(42);
        value <- obj.getValue();
        _ <- obj.setValue(value * 2);
        return obj.getValue();
    }
}
```

#### 9.3.4 Rust Library Bindings

Rust libraries can be wrapped using `foreign rust`:

```
// rust-bindings.first - Wrapping a Rust crate
foreign rust "serde_json" {
    // Rust function: pub fn from_str(s: &str) -> Result<Value, Error>
    // Wrapped as:
    function fromStr(json: String) -> Result<JSONValue, String>;
    
    // Rust function: pub fn to_string(v: &Value) -> String
    function toString(value: JSONValue) -> String;
    
    type JSONValue;
}

// Usage
import { fromStr, toString, JSONValue } from "./rust-bindings";

interaction parseJSON(jsonString: String) -> Result<JSONValue, String> {
    return fromStr(jsonString);
}
```

#### 9.3.5 Type Mapping

The compiler automatically maps native types to First types:

```
// C/C++ -> First type mappings
int32_t     -> Int
int64_t     -> Int
float       -> Float
double      -> Float
bool        -> Bool
char*       -> String
void*       -> OpaquePtr (or specific pointer type)
const char* -> String

// Rust -> First type mappings
i32         -> Int
i64         -> Int
f32         -> Float
f64         -> Float
bool        -> Bool
String      -> String
&str        -> String
Result<T,E> -> Result<T, E>
Option<T>   -> Option<T>
Vec<T>      -> Array<T>

// Pointer types
C: void*           -> OpaquePtr
C: int*            -> IntPtr
C++: std::string*  -> StringPtr
Rust: *mut T       -> MutPtr<T>
Rust: *const T     -> ConstPtr<T>
```

#### 9.3.6 Memory Management

Native library bindings handle memory management automatically:

```
// Automatic memory management for C strings
foreign c "stringlib" {
    // C function that allocates: char* strdup(const char* s);
    // Automatically freed when no longer referenced
    function strdup(s: String) -> String;
    
    // C function that takes ownership: void free_string(char* s);
    // Called automatically when String is dropped
    interaction freeString(s: String) -> Unit;
}

// For manual memory management (advanced)
foreign c "mylib" {
    // Raw pointer - requires manual management
    type RawPtr;
    
    interaction malloc(size: Int) -> RawPtr;
    interaction free(ptr: RawPtr) -> Unit;
    
    // Safe wrapper with automatic cleanup
    function withPtr<T>(size: Int, f: function(RawPtr) -> T) -> T;
}
```

#### 9.3.7 Error Handling

Native library errors are converted to First's `Result` type:

```
// C function that returns error codes
foreign c "mylib" {
    // C: int process_data(int* result, const char* input);
    // Returns 0 on success, non-zero on error
    interaction processData(input: String) -> Result<Int, String>;
}

// Rust function with Result
foreign rust "mylib" {
    // Rust: pub fn parse(input: &str) -> Result<Value, ParseError>
    function parse(input: String) -> Result<Value, String>;
}
```

#### 9.3.8 Callback Functions

Native libraries can call back into First code:

```
// C library with callback
foreign c "mylib" {
    // C: void iterate(int* array, int length, void (*callback)(int));
    interaction iterate(array: Array<Int>, callback: function(Int) -> Unit) -> Unit;
}

// Usage
interaction processArray(arr: Array<Int>) -> Unit {
    iterate(arr, function(value: Int) {
        print(value.toString());
    });
}
```

#### 9.3.9 Package Configuration

Declare native library dependencies in `package.first`:

```json
{
    "name": "my-first-app",
    "version": "1.0.0",
    "dependencies": {
        "first-stdlib": "^1.0.0"
    },
    "native-dependencies": {
        "c": {
            "curl": {
                "library": "curl",
                "include": "/usr/local/include",
                "libdir": "/usr/local/lib"
            },
            "sqlite3": {
                "library": "sqlite3",
                "pkg-config": "sqlite3"
            }
        },
        "cpp": {
            "boost": {
                "library": "boost_system",
                "version": "1.82.0"
            }
        },
        "rust": {
            "serde_json": "1.0",
            "reqwest": "0.11"
        }
    }
}
```

### 9.4 Custom Native Bindings

#### 9.4.1 Manual Wrapper Definition

For complex cases, you can define custom wrappers:

```
// native-bindings.first
foreign c "sqlite3" {
    type SQLiteDB;
    type SQLiteStmt;
    
    // Open database
    interaction sqlite3Open(filename: String) -> Result<SQLiteDB, String>;
    
    // Prepare statement
    interaction sqlite3Prepare(db: SQLiteDB, sql: String) -> Result<SQLiteStmt, String>;
    
    // Execute statement
    interaction sqlite3Step(stmt: SQLiteStmt) -> Result<Int, String>;
    
    // Get column value
    function sqlite3ColumnText(stmt: SQLiteStmt, col: Int) -> String;
    
    // Close resources
    interaction sqlite3Close(db: SQLiteDB) -> Unit;
    interaction sqlite3Finalize(stmt: SQLiteStmt) -> Unit;
}

// Usage
import { sqlite3Open, sqlite3Prepare, sqlite3Step, sqlite3ColumnText, sqlite3Close, SQLiteDB } from "./native-bindings";

interaction queryOneRow(dbPath: String, query: String) -> Result<Option<String>, String> {
    do {
        db <- sqlite3Open(dbPath);
        stmt <- sqlite3Prepare(db, query);
        stepResult <- sqlite3Step(stmt);
        match stepResult {
            Ok(100) => { // SQLITE_ROW
                let value = sqlite3ColumnText(stmt, 0);
                return Ok(Some(value));
            },
            Ok(101) => return Ok(None), // SQLITE_DONE
            Err(e) => return Err(e)
        }
    }
}
```

#### 9.4.2 Automatic Wrapper Generation

The compiler can automatically generate wrappers from header files or library metadata:

```bash
# Generate wrappers from C header
firstc --generate-bindings --from-header=curl/curl.h --output=curl-bindings.first

# Generate wrappers from Rust crate
firstc --generate-bindings --from-crate=serde_json --output=serde-bindings.first

# Generate wrappers using pkg-config
firstc --generate-bindings --pkg-config=sqlite3 --output=sqlite-bindings.first
```

#### 9.4.3 Advanced FFI Features

```
// Inline native code (for performance-critical sections)
foreign c inline {
    // C code embedded directly
    int fast_hash(const char* str) {
        int hash = 5381;
        int c;
        while ((c = *str++)) {
            hash = ((hash << 5) + hash) + c;
        }
        return hash;
    }
}

// This generates a First function:
function fastHash(str: String) -> Int;

// Usage
let hash = fastHash("hello");
```

## 10. Memory Management

### 10.1 Ownership and Borrowing
- Values have a single owner
- References can be borrowed
- Automatic memory management through reference counting and cycle detection

### 10.2 Mutability
```
let immutableValue = 42;              // Cannot be changed
var mutableValue = 42;                // Can be changed
var mutableArray: Array<Int> = [1, 2, 3];  // Array contents can be modified
```

## 11. Error Handling

### 11.1 Result Type
```
type Result<T, E> = Ok(T) | Err(E);

interaction divide(x: Float, y: Float) -> Result<Float, String> {
    if (y == 0.0) {
        return Err("Division by zero");
    } else {
        return Ok(x / y);
    }
}
```

### 11.2 Pattern Matching
```
let result = divide(10.0, 2.0);
match result {
    Ok(value) => print("Result: " + value.toString()),
    Err(error) => print("Error: " + error)
}
```

## 12. Built-in Functions and Interactions

### 12.1 I/O Interactions
```
interaction print(message: String) -> Unit;
interaction input() -> String;
interaction readFile(filename: String) -> Result<String, String>;
interaction writeFile(filename: String, content: String) -> Result<Unit, String>;
```

### 12.2 Array Functions
```
function Array<T>.length() -> Int;
function Array<T>.map<U>(f: function(T) -> U) -> Array<U>;
function Array<T>.filter(f: function(T) -> Bool) -> Array<T>;
function Array<T>.reduce<U>(f: function(U, T) -> U, initial: U) -> U;
```

### 12.3 String Functions
```
function String.length() -> Int;
function String.substring(start: Int, end: Int) -> String;
function String.split(delimiter: String) -> Array<String>;
function String.contains(substring: String) -> Bool;
```

## 13. Compiler Architecture

### 13.1 Compilation Pipeline
1. **Dependency Resolution**: Parse `package.first` and resolve native library dependencies
2. **Wrapper Generation**: Generate First bindings for native libraries (C/C++/Rust)
3. **Lexical Analysis**: Tokenization using ANTLR4 lexer
4. **Syntax Analysis**: Parse tree generation using ANTLR4 parser
5. **Semantic Analysis**: Type checking, inference, and type system validation
6. **IR Generation**: Generate LLVM IR from validated AST
7. **Optimization**: LLVM optimization passes
8. **Code Generation**: Generate native machine code via LLVM
9. **Linking**: Link with native libraries and runtime

### 13.2 Wrapper Generation System

#### 13.2.1 Automatic Wrapper Generation
The compiler analyzes native library interfaces and generates appropriate First wrappers:

```c
// C library header: mylib.h
int process_data(int* result, const char* input);
void* create_context();
void destroy_context(void* ctx);
```

```first
// Generated First wrapper: mylib-bindings.first
foreign c "mylib" {
    type Context;
    
    interaction processData(input: String) -> Result<Int, String>;
    interaction createContext() -> Context;
    interaction destroyContext(ctx: Context) -> Unit;
}
```

#### 13.2.2 Type Translation Engine
The compiler includes a sophisticated type translation engine:

```
Native Type System → First Type System Translation Rules:

1. C Basic Types:
   int32_t     → Int
   int64_t     → Int
   float       → Float
   double      → Float
   bool        → Bool
   char*       → String
   const char* → String
   void        → Unit
   void*       → OpaquePtr

2. C++ Types:
   std::string     → String
   std::vector<T>  → Array<T>
   std::optional<T> → Option<T>
   std::unique_ptr<T> → T (with automatic memory management)

3. Rust Types:
   i32         → Int
   i64         → Int
   f32         → Float
   f64         → Float
   bool        → Bool
   String      → String
   &str        → String
   Vec<T>      → Array<T>
   Result<T,E> → Result<T, E>
   Option<T>   → Option<T>

4. Function Types:
   C: int (*)(int, int)     → function(Int, Int) -> Int
   C++: std::function<int(int, int)> → function(Int, Int) -> Int
   Rust: fn(i32, i32) -> i32 → function(Int, Int) -> Int

5. Pointer Types:
   C: T*                    → Ptr<T> or MutPtr<T>
   C: const T*              → ConstPtr<T>
   C++: std::shared_ptr<T>  → T (reference counted)
   Rust: *mut T             → MutPtr<T>
   Rust: *const T           → ConstPtr<T>
```

#### 13.2.3 FFI Bridge Generation
For each wrapped native function, the compiler generates FFI bridge code:

```cpp
// Generated C++ FFI bridge
#include "mylib.h"
#include "first_runtime.h"

extern "C" {
    // Bridge for processData
    FirstResult* first_mylib_processData(const char* input) {
        int result;
        int status = process_data(&result, input);
        
        if (status == 0) {
            return first_result_ok(first_int_new(result));
        } else {
            return first_result_err(first_string_new("Processing failed"));
        }
    }
    
    // Bridge for createContext
    void* first_mylib_createContext() {
        return create_context();
    }
    
    // Bridge for destroyContext
    void first_mylib_destroyContext(void* ctx) {
        destroy_context(ctx);
    }
}
```

### 13.3 Target Output
The compiler generates LLVM IR that:
- Maintains the semantics of the First program
- Uses appropriate LLVM types and constructs
- Handles the function/interaction distinction
- Preserves type safety through LLVM's type system
- Includes FFI calls to native libraries
- Manages memory safely through reference counting
- Optimizes code through LLVM optimization passes

### 13.4 Build System
```bash
# Complete build pipeline
first install    # Install native dependencies and generate wrappers
first build      # Compile First → LLVM IR → native executable
first run        # Execute the program

# Detailed build steps:
firstc src/main.first \
  --native-libs=curl,sqlite3 \
  --generate-bindings \
  --output=dist/

# This creates:
# dist/main.ll              - Generated LLVM IR
# dist/bindings/            - Generated FFI bindings
# dist/main.o                - Object file
# dist/main                  - Final executable

# Compilation flow:
# First → AST → LLVM IR → Object Code → Executable
firstc src/main.first -emit-llvm -o dist/main.ll
llc dist/main.ll -o dist/main.s
clang dist/main.s -o dist/main -lcurl -lsqlite3
```

### 13.5 Integration with Native Ecosystems

#### 13.5.1 CMake Integration
```cmake
# Generated CMakeLists.txt for native dependencies
cmake_minimum_required(VERSION 3.15)
project(first-app)

find_package(PkgConfig REQUIRED)
pkg_check_modules(CURL REQUIRED libcurl)
pkg_check_modules(SQLITE3 REQUIRED sqlite3)

add_executable(first-app main.cpp)
target_link_libraries(first-app ${CURL_LIBRARIES} ${SQLITE3_LIBRARIES})
target_include_directories(first-app PRIVATE ${CURL_INCLUDE_DIRS} ${SQLITE3_INCLUDE_DIRS})
```

#### 13.5.2 Cargo Integration (for Rust dependencies)
```toml
# Generated Cargo.toml for Rust dependencies
[package]
name = "first-app-bindings"
version = "0.1.0"

[dependencies]
serde_json = "1.0"
reqwest = { version = "0.11", features = ["json"] }

[lib]
crate-type = ["cdylib"]
```

#### 13.5.3 Error Handling Across Language Boundaries
```cpp
// Generated error handling bridge
namespace First {
    template<typename T>
    struct Result {
        bool is_ok;
        union {
            T ok_value;
            const char* err_message;
        };
        
        static Result<T> Ok(T value) {
            Result<T> r;
            r.is_ok = true;
            r.ok_value = value;
            return r;
        }
        
        static Result<T> Err(const char* msg) {
            Result<T> r;
            r.is_ok = false;
            r.err_message = msg;
            return r;
        }
    };
}

// Convert C error codes to First Result
First::Result<int> c_to_first_result(int c_status, int value) {
    if (c_status == 0) {
        return First::Result<int>::Ok(value);
    } else {
        return First::Result<int>::Err("C function returned error");
    }
}
```

## 14. Standard Library

### 14.1 Core Modules
The First standard library provides native implementations built on top of native libraries:

#### 14.1.1 Native First Modules
- `Prelude`: Basic functions and types native to First
- `Array`: Array manipulation functions optimized for First
- `String`: String processing functions with First semantics
- `IO`: Input/output interactions with First error handling
- `Math`: Mathematical functions and constants (wraps libm)
- `Result`: Error handling utilities
- `Option`: Optional value utilities
- `Map`: Efficient key-value maps (native implementation)
- `Set`: Set operations and data structures (native implementation)
- `JSON`: JSON parsing and generation (wraps native JSON libraries)
- `HTTP`: HTTP client functionality (wraps libcurl or similar)
- `Regex`: Regular expression matching (wraps native regex libraries)
- `Time`: Date and time operations (wraps native time libraries)
- `FileSystem`: File system operations (native implementation)

### 14.2 Example Standard Library Usage

#### 14.2.1 Native First Usage
```
import { map, filter, reduce } from "Array";           // Native First
import { print, readFile } from "IO";                  // Native First  
import { sin, cos, pi } from "Math";                   // Native First (wraps libm)
import * as Map from "Map";                            // Native First
import * as JSON from "JSON";                          // Native First (wraps native JSON lib)

interaction main() -> Unit {
    let numbers = [1, 2, 3, 4, 5];
    
    // Using native First functions
    let doubled = map(numbers, function(x) { return x * 2; });
    let evens = filter(doubled, function(x) { return x % 2 == 0; });
    let sum = reduce(numbers, function(acc, x) { return acc + x; }, 0);
    
    // Using native Map
    var phoneBook = Map.empty();
    phoneBook = Map.insert("Alice", "555-1234", phoneBook);
    phoneBook = Map.insert("Bob", "555-5678", phoneBook);
    
    let alicePhone = Map.lookup("Alice", phoneBook); // Returns Option<String>
    
    print("Even doubled numbers: " + show(evens));
    print("Sum: " + sum.toString());
}
```

#### 14.2.2 JSON Processing
```
import * as JSON from "JSON";
import { Result, Ok, Err } from "Result";

type User = {
    id: Int,
    name: String,
    email: String,
    active: Bool
};

// JSON parsing using native JSON library
interaction parseUser(jsonString: String) -> Result<User, String> {
    match JSON.parse(jsonString) {
        Ok(value) => {
            // Pattern match on JSON structure
            match value {
                JSON.Object(obj) => {
                    do {
                        id <- JSON.getInt("id", obj);
                        name <- JSON.getString("name", obj);
                        email <- JSON.getString("email", obj);
                        active <- JSON.getBool("active", obj);
                        return Ok({ id, name, email, active });
                    }
                },
                _ => Err("Expected JSON object")
            }
        },
        Err(parseError) => Err("JSON parse error: " + parseError)
    }
}

interaction saveUserAsJSON(user: User, filename: String) -> Result<Unit, String> {
    do {
        let jsonValue = JSON.object([
            ("id", JSON.number(user.id)),
            ("name", JSON.string(user.name)),
            ("email", JSON.string(user.email)),
            ("active", JSON.bool(user.active))
        ]);
        jsonString <- JSON.stringify(jsonValue);
        _ <- writeFile(filename, jsonString);
        return Ok(());
    }
}
```

#### 14.2.3 HTTP Client Operations
```
import * as HTTP from "HTTP";
import * as JSON from "JSON";

interaction fetchUserData(userId: Int) -> Result<User, String> {
    do {
        let url = "https://api.example.com/users/" + userId.toString();
        response <- HTTP.get(url);
        
        match response {
            HTTP.Response(status, body) => {
                if (status == 200) {
                    user <- parseUser(body);
                    return Ok(user);
                } else {
                    return Err("HTTP error: " + status.toString());
                }
            },
            HTTP.Error(msg) => Err("HTTP request failed: " + msg)
        }
    }
}

interaction postUserData(user: User) -> Result<User, String> {
    do {
        userJson <- JSON.stringify(user);
        response <- HTTP.post("https://api.example.com/users", userJson, [
            ("Content-Type", "application/json")
        ]);
        
        match response {
            HTTP.Response(status, body) => {
                if (status == 201) {
                    createdUser <- parseUser(body);
                    return Ok(createdUser);
                } else {
                    return Err("Failed to create user");
                }
            },
            HTTP.Error(msg) => Err("HTTP request failed: " + msg)
        }
    }
}
```

### 14.3 Package Management for Native Dependencies

#### 14.3.1 Package Configuration
```json
// package.first
{
    "name": "my-first-app",
    "version": "1.0.0",
    "description": "A First language application",
    "main": "src/main.first",
    "dependencies": {
        "first-stdlib": "^1.0.0"
    },
    "native-dependencies": {
        "c": {
            "curl": {
                "library": "curl",
                "pkg-config": "libcurl"
            },
            "sqlite3": {
                "library": "sqlite3",
                "pkg-config": "sqlite3"
            },
            "json-c": {
                "library": "json-c",
                "pkg-config": "json-c"
            }
        },
        "cpp": {
            "boost": {
                "library": "boost_system",
                "version": "1.82.0"
            }
        },
        "rust": {
            "serde_json": "1.0",
            "reqwest": "0.11"
        }
    },
    "compiler": {
        "llvm-options": ["-O2", "-Wall"],
        "link-flags": ["-lcurl", "-lsqlite3"]
    }
}
```

#### 14.3.2 Build System Integration
```bash
# Install dependencies
first install

# This generates native wrapper modules and installs native dependencies
# Creates: .first/bindings/curl.first
#          .first/bindings/sqlite3.first
#          etc.

# Build project
first build

# This compiles First code to LLVM IR, then to native executable
# Equivalent to:
# firstc src/main.first -emit-llvm -o dist/main.ll
# llc dist/main.ll -o dist/main.s
# clang dist/main.s -o dist/main -lcurl -lsqlite3

# Run project
first run
# Or directly: ./dist/main
```

## 15. Language Examples

### 15.1 Hello World
```
interaction main() -> Unit {
    print("Hello, World!");
}
```

### 15.2 Factorial with Memoization
```
var memo: Array<Int> = [];

function factorial(n: Int) -> Int {
    if (n <= 1) {
        return 1;
    } else if (memo.length() > n && memo[n] != 0) {
        return memo[n];
    } else {
        let result = n * factorial(n - 1);
        if (memo.length() <= n) {
            memo = memo.resize(n + 1);
        }
        memo[n] = result;
        return result;
    }
}
```

### 15.3 Web Server with Monadic Error Handling
```
type Request = {
    method: String,
    path: String,
    body: String
};

type Response = {
    status: Int,
    body: String
};

interface Handler {
    handle: interaction(Request) -> Response;
}

implementation Handler for APIHandler {
    handle = interaction(req: Request) -> Response {
        do {
            // Monadic composition for request processing
            user <- authenticateRequest(req);
            data <- processRequest(req, user);
            response <- formatResponse(data);
            _ <- logRequest(req, response);
            return response;
        } catch {
            AuthError(msg) => Response { status: 401, body: msg },
            ValidationError(msg) => Response { status: 400, body: msg },
            _ => Response { status: 500, body: "Internal server error" }
        }
    };
}

interaction authenticateRequest(req: Request) -> Result<User, AuthError> {
    do {
        token <- extractToken(req);
        user <- validateToken(token);
        _ <- checkPermissions(user, req.path);
        return Ok(user);
    }
}

interaction processFileUpload(files: Array<String>) -> Result<Array<String>, String> {
    // Using monadic operators for elegant error handling
    return files.traverse(function(filename) {
        validateFile(filename) >>
        readFile(filename) >>= function(content) {
            processContent(content) >>= function(processed) {
                saveProcessedFile(filename + ".processed", processed);
            };
        };
    });
}
```

### 15.4 Database Operations with Monadic Composition
```
type User = {
    id: Int,
    name: String,
    email: String
};

type Database = {
    connection: Connection
};

interaction createUser(db: Database, userData: User) -> Result<User, String> {
    do {
        // Validate input
        _ <- validateEmail(userData.email);
        _ <- validateName(userData.name);
        
        // Check if user exists
        existing <- findUserByEmail(db, userData.email);
        match existing {
            Some(_) => return Err("User already exists"),
            None => {}
        };
        
        // Create user
        userId <- insertUser(db, userData);
        newUser <- getUserById(db, userId);
        
        // Log creation
        _ <- logUserCreation(newUser);
        
        return Ok(newUser);
    }
}

interaction transferFunds(db: Database, fromId: Int, toId: Int, amount: Float) -> Result<Unit, String> {
    // Monadic composition with transaction handling
    withTransaction(db, function() {
        do {
            fromAccount <- getAccount(db, fromId);
            toAccount <- getAccount(db, toId);
            
            // Validate transfer
            _ <- validateSufficientFunds(fromAccount, amount);
            _ <- validateAccountStatus(fromAccount);
            _ <- validateAccountStatus(toAccount);
            
            // Perform transfer
            _ <- debitAccount(db, fromId, amount);
            _ <- creditAccount(db, toId, amount);
            
            // Log transaction
            _ <- logTransaction(fromId, toId, amount);
            
            return Ok(());
        }
    });
}
```

### 15.5 Async Operations with Monadic Patterns
```
type Promise<T> = /* Internal async type */;

interaction fetchUserData(userId: Int) -> Promise<Result<User, String>> {
    do {
        // Parallel fetching with applicative operators
        user <- fetchUser(userId);
        profile <- fetchProfile(userId);
        preferences <- fetchPreferences(userId);
        
        // Combine results
        return function(u, p, prefs) { 
            return { 
                user: u, 
                profile: p, 
                preferences: prefs 
            }; 
        } <$> user <*> profile <*> preferences;
    }
}

interaction processUserWorkflow(userId: Int) -> Promise<Result<String, String>> {
    fetchUserData(userId) >>= function(userData) {
        validateUserData(userData) >>= function(validData) {
            processData(validData) >>= function(result) {
                saveResults(userId, result) >>= function(_) {
                    sendNotification(userId, "Processing complete") >>
                    return Promise.resolve(Ok("Workflow completed"));
                };
            };
        };
    };
}
```

This specification provides a comprehensive foundation for the First programming language, incorporating all the requested features including Haskell's monadic operators restricted to interaction functions, while maintaining consistency and clarity in design. 