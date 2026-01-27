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
let var function interaction if else while return
type interface implementation import export
true false null Int String Bool Float Array
```

## 3. Type System

### 3.1 Primitive Types
- `Int`: 64-bit signed integers
- `Float`: 64-bit floating point
- `Bool`: Boolean values (`true`, `false`)
- `String`: UTF-8 encoded strings
- `Unit`: Void type (represented as `()`)

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

#### 3.2.6 Algebraic Data Types
```
type Shape = 
    | Circle(radius: Float)
    | Rectangle(width: Float, height: Float)
    | Square(side: Float);
```

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

### 5.1 Conditional Statements
```
if (condition) {
    // statements
} else if (anotherCondition) {
    // statements
} else {
    // statements
}

// Conditional expressions
let result = if (x > 0) "positive" else "non-positive";
```

### 5.2 While Loops

**While loops are ONLY allowed in interaction functions, not in pure functions.**

```
// ✅ Valid - while loop in interaction function
interaction processNumbers(): Unit {
    var i = 0;
    while (i < 10) {
        print(i.toString());
        i += 1;
    }
}

// ❌ Invalid - while loop in pure function
function invalidLoop(): Int {
    while (true) {        // ERROR: while loops not allowed in pure functions
        return 1;
    }
    return 0;
}

// ✅ Alternative - use recursion in pure functions
function countdown(n: Int): Unit {
    if (n > 0) {
        // Use recursion instead of loops in pure functions
        return countdown(n - 1);
    }
}
```

## 6. Functions and Interactions

First makes a **strict distinction** between pure functions and side-effecting interactions. This separation enforces functional programming principles while allowing controlled imperative programming where needed.

### 6.1 Pure Functions

Pure functions are the **primary and preferred** way to write code in First. They guarantee **referential transparency** and **no side effects**.

**Restrictions in Pure Functions:**
- ❌ **No mutable variables** (`var` declarations)
- ❌ **No while loops** (use recursion instead)
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
- ✅ **While loops** and imperative control flow
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
    while (counter < 10) {          // While loop
        counter = counter + 1;      // Assignment
        print(counter.toString());  // I/O operation
    }
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
- While loops (`while`)
- Monadic operators (`>>=`, `>>`, `<$>`, `<*>`)
- I/O operations (print, readFile, input, etc.)
- Any side effects or state mutations

#### Interaction Functions (`interaction` keyword)

**✅ ALLOWED:**
- Everything pure functions can do, PLUS:
- Mutable variable declarations (`var`)
- Variable assignments and mutations
- While loops and imperative control flow
- Monadic operators for composing effects
- I/O operations and side effects
- State management and mutations

#### Compile-Time Enforcement

The compiler enforces these restrictions with clear error messages:

```first
// ❌ This will cause a compile error:
function invalidPure(): Int {
    var x: Int = 5;  // ERROR: Mutable variables can only be used in interaction functions
    while (x < 10) { // ERROR: While loops can only be used in interaction functions  
        x = x + 1;
    }
    return x;
}

// ✅ This is the correct way:
interaction validInteraction(): Int {
    var x: Int = 5;  // OK: Mutable variables allowed in interactions
    while (x < 10) { // OK: While loops allowed in interactions
        x = x + 1;
    }
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

### 9.3 Haskell Library Integration

First provides seamless integration with Haskell libraries through automatic wrapper generation and a foreign function interface (FFI).

#### 9.3.1 Haskell Standard Library Access
```
// Import Haskell standard library modules
import * as List from "haskell:Data.List";
import * as Maybe from "haskell:Data.Maybe";
import * as Map from "haskell:Data.Map";
import { foldr, foldl, map } from "haskell:Prelude";

// Use Haskell functions directly
let numbers = [1, 2, 3, 4, 5];
let sum = foldr(function(x, acc) { return x + acc; }, 0, numbers);
let doubled = List.map(function(x) { return x * 2; }, numbers);
```

#### 9.3.2 Third-party Haskell Libraries
```
// Import third-party Haskell libraries
import * as Aeson from "haskell:Data.Aeson";
import * as Http from "haskell:Network.HTTP.Simple";
import * as Text from "haskell:Data.Text";

// Declare library dependencies in package.first
{
    "name": "my-first-project",
    "version": "1.0.0",
    "dependencies": {
        "first-stdlib": "^1.0.0"
    },
    "haskell-dependencies": {
        "aeson": "^2.1.0",
        "http-conduit": "^2.3.0",
        "text": "^2.0.0",
        "containers": "^0.6.0"
    }
}
```

#### 9.3.3 Automatic Type Mapping
The compiler automatically maps Haskell types to First types:

```
// Haskell -> First type mappings
Int        -> Int
Double     -> Float  
Bool       -> Bool
String     -> String
[a]        -> Array<a>
Maybe a    -> Option<a>
Either a b -> Result<b, a>
IO a       -> IO<a>
(a, b)     -> { first: a, second: b }
(a, b, c)  -> { first: a, second: b, third: c }

// Custom Haskell data types are mapped to algebraic types
data Tree a = Leaf a | Branch (Tree a) (Tree a)
// Becomes:
type Tree<T> = Leaf(T) | Branch(Tree<T>, Tree<T>);
```

#### 9.3.4 Wrapper Generation
The compiler automatically generates wrappers for Haskell functions:

```
// Haskell function:
-- Data.List.sortBy :: (a -> a -> Ordering) -> [a] -> [a]

// Generated First wrapper:
function List.sortBy<T>(compareFn: function(T, T) -> Int, list: Array<T>) -> Array<T>;

// Usage in First:
let sorted = List.sortBy(function(a, b) { 
    if (a < b) return -1;
    else if (a > b) return 1;
    else return 0;
}, [3, 1, 4, 1, 5]);
```

#### 9.3.5 Interaction Wrapping
Haskell IO actions are automatically wrapped as interactions:

```
// Haskell: readFile :: FilePath -> IO String
// Wrapped as:
interaction readFile(path: String) -> String;

// Haskell: writeFile :: FilePath -> String -> IO ()  
// Wrapped as:
interaction writeFile(path: String, content: String) -> Unit;

// Usage:
interaction processFiles() -> Result<Unit, String> {
    do {
        content <- readFile("input.txt");
        processed <- processContent(content);
        _ <- writeFile("output.txt", processed);
        return Ok(());
    }
}
```

### 9.4 Custom Haskell Bindings

#### 9.4.1 Manual Wrapper Definition
For complex cases, you can define custom wrappers:

```
// haskell-bindings.first
foreign haskell "Data.ByteString" {
    type ByteString;
    
    function readFile(path: String) -> ByteString;
    function writeFile(path: String, content: ByteString) -> Unit;
    function length(bs: ByteString) -> Int;
    
    // Map Haskell functions with different signatures
    function "Data.ByteString.take" as take(n: Int, bs: ByteString) -> ByteString;
    function "Data.ByteString.drop" as drop(n: Int, bs: ByteString) -> ByteString;
}

// Usage
import { ByteString, readFile, writeFile, length } from "./haskell-bindings";

interaction copyFile(src: String, dest: String) -> Result<Unit, String> {
    do {
        content <- readFile(src);
        _ <- writeFile(dest, content);
        return Ok(());
    }
}
```

#### 9.4.2 Type Class Mapping
Haskell type classes are mapped to First interfaces:

```
// Haskell: class Show a where show :: a -> String
// Maps to:
interface Show<T> {
    show: function(T) -> String;
}

// Haskell instances are automatically available
// show :: Int -> String becomes available when importing Show<Int>
```

#### 9.4.3 Advanced FFI Features
```
// Direct Haskell code embedding (for complex cases)
foreign haskell inline {
    parseJSON :: String -> Either String Value
    parseJSON str = case eitherDecode (encodeUtf8 (pack str)) of
        Left err -> Left err
        Right val -> Right val
}

// This generates a First function:
function parseJSON(json: String) -> Result<Value, String>;
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
1. **Dependency Resolution**: Parse package.first and resolve Haskell dependencies
2. **Wrapper Generation**: Generate First bindings for Haskell libraries
3. **Lexical Analysis**: Tokenization using Parsec
4. **Syntax Analysis**: Parse tree generation
5. **Semantic Analysis**: Type checking, inference, and Haskell type mapping
6. **Code Generation**: Haskell code generation with FFI bindings
7. **Haskell Compilation**: Use GHC to compile generated Haskell code
8. **Optimization**: Optional optimization passes

### 13.2 Wrapper Generation System

#### 13.2.1 Automatic Wrapper Generation
The compiler analyzes Haskell library interfaces and generates appropriate First wrappers:

```haskell
-- Haskell library analysis
-- Input: Data.List module
sort :: Ord a => [a] -> [a]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
group :: Eq a => [a] -> [[a]]

-- Generated First wrapper: Data/List.first
interface Ord<T> {
    compare: function(T, T) -> Int;
}

function sort<T>(list: Array<T>) -> Array<T> where Ord<T>;
function sortBy<T>(compareFn: function(T, T) -> Int, list: Array<T>) -> Array<T>;
function group<T>(list: Array<T>) -> Array<Array<T>> where Eq<T>;
```

#### 13.2.2 Type Translation Engine
The compiler includes a sophisticated type translation engine:

```
Haskell Type System → First Type System Translation Rules:

1. Basic Types:
   Int        → Int
   Double     → Float
   Bool       → Bool
   Char       → String (single char)
   String     → String
   ()         → Unit

2. Parametric Types:
   [a]        → Array<a>
   Maybe a    → Option<a>
   Either a b → Result<b, a>  // Note: flipped for First conventions
   (a, b)     → {first: a, second: b}
   IO a       → IO<a> (interaction context)

3. Function Types:
   a -> b           → function(a) -> b
   a -> b -> c      → function(a, b) -> c  // Curried to uncurried
   a -> IO b        → interaction(a) -> b  // IO becomes interaction

4. Type Classes:
   Show a    → interface Show<a> { show: function(a) -> String; }
   Eq a      → interface Eq<a> { equals: function(a, a) -> Bool; }
   Ord a     → interface Ord<a> extends Eq<a> { compare: function(a, a) -> Int; }

5. Custom Data Types:
   data Tree a = Leaf a | Branch (Tree a) (Tree a)
   → type Tree<a> = Leaf(a) | Branch(Tree<a>, Tree<a>);
```

#### 13.2.3 FFI Bridge Generation
For each wrapped Haskell function, the compiler generates FFI bridge code:

```haskell
-- Generated Haskell FFI bridge
{-# LANGUAGE ForeignFunctionInterface #-}
module First.Bindings.Data.List where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

-- Bridge for Data.List.sort
foreign export ccall "first_data_list_sort"
  firstDataListSort :: Ptr (StablePtr [Int]) -> IO (Ptr (StablePtr [Int]))

firstDataListSort listPtr = do
  listStable <- deRefStablePtr =<< peek listPtr
  let sorted = Data.List.sort listStable
  newStable <- newStablePtr sorted
  return =<< newPtr newStable

-- Similar bridges for other functions...
```

### 13.3 Target Output
The compiler generates Haskell code that:
- Maintains the semantics of the First program
- Uses appropriate Haskell types and constructs
- Handles the function/interaction distinction
- Preserves type safety
- Includes FFI bindings for library integration
- Manages memory safely across FFI boundaries

### 13.4 Build System
```bash
# Complete build pipeline
first install    # Install and generate wrappers
first build      # Compile First → Haskell → executable
first run        # Execute the program

# Detailed build steps:
firstc src/main.first \
  --haskell-libs=aeson,http-conduit,text \
  --generate-wrappers \
  --output=dist/

# This creates:
# dist/Main.hs              - Generated Haskell code
# dist/First/Bindings/      - FFI bridge modules  
# dist/First/Wrappers/      - Type-safe wrappers
# dist/package.yaml         - Stack/Cabal configuration

ghc dist/Main.hs -package-db dist/.packages -o dist/main
./dist/main
```

### 13.5 Integration with Haskell Ecosystem

#### 13.5.1 Stack Integration
```yaml
# Generated stack.yaml
resolver: lts-20.0

packages:
- .

extra-deps:
- first-runtime-1.0.0

ghc-options:
  "*": -Wall -O2
  
allow-newer: true
```

#### 13.5.2 Cabal Integration  
```cabal
-- Generated package.cabal
name:                first-generated-app
version:             1.0.0
build-type:          Simple
cabal-version:       >=1.10

executable main
  main-is:             Main.hs
  other-modules:       First.Runtime
                     , First.Bindings.Data.List
                     , First.Bindings.Data.Aeson
  build-depends:       base >=4.7 && <5
                     , first-runtime
                     , aeson
                     , http-conduit
                     , text
                     , containers
  default-language:    Haskell2010
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
```

#### 13.5.3 Error Handling Across Language Boundaries
```haskell
-- Generated error handling bridge
data FirstResult a = FirstOk a | FirstErr String

-- Convert Haskell Either to First Result
toFirstResult :: Either String a -> FirstResult a
toFirstResult (Left err) = FirstErr err
toFirstResult (Right val) = FirstOk val

-- Convert Haskell Maybe to First Option
data FirstOption a = FirstSome a | FirstNone

toFirstOption :: Maybe a -> FirstOption a
toFirstOption Nothing = FirstNone  
toFirstOption (Just x) = FirstSome x

-- Exception handling bridge
catchToResult :: IO a -> IO (FirstResult a)
catchToResult action = do
  result <- try action
  case result of
    Left (ex :: SomeException) -> return $ FirstErr (show ex)
    Right val -> return $ FirstOk val
```

## 14. Standard Library

### 14.1 Core Modules
The First standard library provides both native implementations and wrappers around Haskell's standard library:

#### 14.1.1 Native First Modules
- `Prelude`: Basic functions and types native to First
- `Array`: Array manipulation functions optimized for First
- `String`: String processing functions with First semantics
- `IO`: Input/output interactions with First error handling
- `Math`: Mathematical functions and constants
- `Result`: Error handling utilities

#### 14.1.2 Haskell Standard Library Wrappers
- `haskell:Prelude`: Complete Haskell Prelude with type mapping
- `haskell:Data.List`: List processing functions
- `haskell:Data.Map`: Efficient key-value maps
- `haskell:Data.Set`: Set operations and data structures
- `haskell:Data.Text`: Efficient text processing
- `haskell:Data.ByteString`: Binary data handling
- `haskell:Control.Monad`: Additional monadic operations
- `haskell:Data.Maybe`: Optional value utilities
- `haskell:Data.Either`: Either type utilities
- `haskell:System.IO`: Advanced I/O operations
- `haskell:Network.HTTP`: HTTP client functionality
- `haskell:Data.Aeson`: JSON parsing and generation

### 14.2 Example Standard Library Usage

#### 14.2.1 Mixed Native and Haskell Usage
```
import { map, filter, reduce } from "Array";           // Native First
import { print, readFile } from "IO";                  // Native First  
import { sin, cos, pi } from "Math";                   // Native First
import * as HList from "haskell:Data.List";           // Haskell wrapper
import * as Map from "haskell:Data.Map";              // Haskell wrapper
import { foldr } from "haskell:Prelude";              // Haskell wrapper

interaction main() -> Unit {
    let numbers = [1, 2, 3, 4, 5];
    
    // Using native First functions
    let doubled = map(numbers, function(x) { return x * 2; });
    let evens = filter(doubled, function(x) { return x % 2 == 0; });
    
    // Using Haskell functions
    let sum = foldr(function(x, acc) { return x + acc; }, 0, numbers);
    let sorted = HList.sort(numbers);
    
    // Using Haskell Map
    var phoneBook = Map.empty();
    phoneBook = Map.insert("Alice", "555-1234", phoneBook);
    phoneBook = Map.insert("Bob", "555-5678", phoneBook);
    
    let alicePhone = Map.lookup("Alice", phoneBook); // Returns Option<String>
    
    print("Even doubled numbers: " + show(evens));
    print("Sum: " + sum.toString());
    print("Sorted: " + show(sorted));
}
```

#### 14.2.2 JSON Processing with Aeson
```
import * as JSON from "haskell:Data.Aeson";
import { Result, Ok, Err } from "Result";

type User = {
    id: Int,
    name: String,
    email: String,
    active: Bool
};

// JSON parsing using Haskell's Aeson
interaction parseUser(jsonString: String) -> Result<User, String> {
    match JSON.decode(jsonString) {
        Ok(value) => {
            // Pattern match on JSON structure
            match value {
                Object(obj) => {
                    do {
                        id <- JSON.lookupInt("id", obj);
                        name <- JSON.lookupString("name", obj);
                        email <- JSON.lookupString("email", obj);
                        active <- JSON.lookupBool("active", obj);
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
        jsonString <- JSON.encode(jsonValue);
        _ <- writeFile(filename, jsonString);
        return Ok(());
    }
}
```

#### 14.2.3 HTTP Client Operations
```
import * as HTTP from "haskell:Network.HTTP.Simple";
import * as JSON from "haskell:Data.Aeson";

interaction fetchUserData(userId: Int) -> Result<User, String> {
    do {
        let url = "https://api.example.com/users/" + userId.toString();
        response <- HTTP.httpGet(url);
        
        if (HTTP.getResponseStatusCode(response) == 200) {
            let body = HTTP.getResponseBody(response);
            user <- parseUser(body);
            return Ok(user);
        } else {
            return Err("HTTP error: " + HTTP.getResponseStatusCode(response).toString());
        }
    }
}

interaction postUserData(user: User) -> Result<User, String> {
    do {
        userJson <- JSON.encode(user);
        let request = HTTP.setRequestMethod("POST") $
                     HTTP.setRequestHeader("Content-Type", "application/json") $
                     HTTP.setRequestBody(userJson) $
                     HTTP.parseRequest("https://api.example.com/users");
        
        response <- HTTP.httpRequest(request);
        
        if (HTTP.getResponseStatusCode(response) == 201) {
            let responseBody = HTTP.getResponseBody(response);
            createdUser <- parseUser(responseBody);
            return Ok(createdUser);
        } else {
            return Err("Failed to create user");
        }
    }
}
```

### 14.3 Package Management for Haskell Dependencies

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
    "haskell-dependencies": {
        // Core libraries
        "base": "^4.17.0",
        "containers": "^0.6.0",
        "text": "^2.0.0",
        "bytestring": "^0.11.0",
        
        // Web and JSON
        "aeson": "^2.1.0",
        "http-conduit": "^2.3.0",
        "warp": "^3.3.0",
        
        // Database
        "persistent": "^2.14.0",
        "persistent-sqlite": "^2.13.0",
        
        // Parsing
        "parsec": "^3.1.0",
        "megaparsec": "^9.2.0"
    },
    "compiler": {
        "ghc-options": ["-O2", "-Wall"],
        "extensions": ["OverloadedStrings", "DeriveGeneric"]
    }
}
```

#### 14.3.2 Build System Integration
```bash
# Install dependencies
first install

# This generates Haskell wrapper modules and installs Haskell dependencies
# Creates: .first/haskell-wrappers/Data/List.first
#          .first/haskell-wrappers/Data/Aeson.first
#          etc.

# Build project
first build

# This compiles First code to Haskell, then uses GHC to compile to executable
# Equivalent to:
# firstc src/main.first -o dist/main.hs
# ghc dist/main.hs -o dist/main

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