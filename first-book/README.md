# The First Programming Language Book

A practical guide to the First programming language: installation, the **fir** project manager, and the core language.

All example code lives in the repository’s **examples** directory. You can run examples with **fir** (see Chapter 1).

---

## Chapters

1. **[Getting Started](chapter-01-getting-started.md)** – Introduction, install on macOS (Homebrew), create the hello project with fir, and use `print` and `println`.

2. **[Functions and Interactions](chapter-02-functions-and-interactions.md)** – Pure **function**s vs side-effecting **interaction**s, when to use which, and examples.

3. **[Modules, Import, and Export](chapter-03-modules-import-export.md)** – Split code into modules, **export** functions, and **import** from other modules with a small project example.

4. **[Basic Types, Expressions, and Type Inference](chapter-04-basic-types-expressions-type-inference.md)** – **Int**, **Float**, **Bool**, **String**, **Unit**; literals; arithmetic, comparison, and logical expressions; optional type annotations and inference for **let** / **var**.

5. **[Control Flow](chapter-05-control-flow.md)** – **if** expression, **for-in** loop, **range** (including custom steps), and a classic **FizzBuzz** (pure function + for-in over 1..=30).

6. **[Custom Record Types and Pattern Matching](chapter-06-records-and-pattern-matching.md)** – Record types (inline `{ field: Type, ... }`), record literals, field access, and **match** with record patterns (destructuring, literals, wildcards).

7. **[Intro to Generic Types](chapter-07-Intro-to-generic-types.md)** – Generic data types, interfaces, and a Haskell-like list.

8. **[Recursion and tail-call optimization](chapter-08-recursion.md)** – Simple recursion (e.g. factorial) and tail-recursive functions with accumulators (TCO keeps the stack bounded). Examples in **examples/chapter-08-recursion**.

9. **[Interfaces — ToString, Eq, Ord](chapter-09-interfaces.md)** – Standard interfaces, **derive(ToString)** for records, custom **Eq** and **Ord** implementations, and calling interface methods. Examples in **examples/chapter-09-interfaces**.

10. **[Array functions](chapter-10-array-functions.md)** – Immutable arrays, **arrayLength**, **insertAt**, **deleteAt**, **reduce**, **reduceRight**, **filter**, and **for-in**. Examples in **examples/chapter-10-Array-functions**.

11. **[Date functions](chapter-11-date-functions.md)** – The **Date** library: now, format, parse, getters (year, month, day, time), and addSeconds.

12. **[Math functions](chapter-12-math-functions.md)** – The **Math** library: trigonometry, sqrt, pow, exp/log, rounding, min/max, and constants pi/e.

13. **[ArrayBuf](chapter-13-arraybuf.md)** – Mutable buffers (interaction-only).

14. **[Higher-kinded types](chapter-14-higher-kinded-types.md)** – **Functor&lt;F&lt;_&gt;&gt;** and implementing it for **Option**.

15. **[Monadic operators in interactions](chapter-15-monadic-operators-in-interactions.md)** – **>>=**, **>>**, **<\$>**, **<\*>** only in interactions; three examples (then, bind, fmap) invoked from **main**.

16. **[Type reference](chapter-16-type-reference.md)** – All types supported by First: primitives, arrays, records, function types, type aliases, generic types, **ADTs** (with many examples), union types, interfaces, and **type-level programming** (constrained generics, higher-kinded **F&lt;_&gt;**). Refinement and dependent types are not covered in this chapter.

17. **[Concurrency and async model](chapter-17-concurrency-and-async.md)** – **async**/ **await** (promises), **spawn**/ **join** (tasks), and **select** (channel receive/send). **Spawn/join** and **async/await** run work in a separate thread; join/await block for the result. Runnable example in **examples/chapter-17-concurrency-async**.
18. **[A JSON parser](chapter-18-A-JSON-parser.md)** – Build a small JSON parser using **ADTs**, **pattern matching**, **Option**, recursion, and immutable arrays. Runnable example in **examples/chapter-18-A-JSON-parser**.

---

## Chapters and examples

The **examples** folder has matching projects for each chapter:

| Chapter | Book chapter | Example project(s) |
|--------|---------------|--------------------|
| 1 | Getting Started | **examples/chapter-01-hello**, **examples/chapter-01-print-and-println** |
| 2 | Functions and Interactions | **examples/chapter-02-functions-and-interactions** |
| 3 | Modules, Import, and Export | **examples/chapter-03-modules** |
| 4 | Basic Types, Expressions, and Type Inference | **examples/chapter-04-basic-expressions-types** |
| 5 | Control Flow | **examples/chapter-05-control-flow** |
| 6 | Custom Record Types and Pattern Matching | **examples/chapter-06-records-pattern-matching** (sum types), **examples/chapter-06-shapes-record-literals** (record literals) |
| 7 | Intro to Generic Types | **examples/chapter-07-Intro-to-generic-types** |
| 8 | Recursion and tail-call optimization | **examples/chapter-08-recursion** |
| 9 | Interfaces (ToString, Eq, Ord) | **examples/chapter-09-interfaces** |
| 10 | Array functions | **examples/chapter-10-Array-functions** |
| 11 | Date functions | **examples/chapter-11-Date-functions** |
| 12 | Math functions | **examples/chapter-12-Math-functions** |
| 13 | ArrayBuf | **examples/chapter-13-ArrayBuf** |
| 14 | Higher-kinded types | **examples/chapter-14-HKT** |
| 15 | Monadic operators in interactions | **examples/chapter-15-monadic-operators** |
| 16 | Type reference | **examples/chapter-16-type-reference** |
| 17 | Concurrency and async model | **examples/chapter-17-concurrency-async** |
| 18 | A JSON parser | **examples/chapter-18-A-JSON-parser** |

From the repo root, run an example with: `cd examples/chapter-N-... && fir run` (or use the **run-all-local.sh** script in examples). For how to **clone the repo**, **build from source**, **run all tests**, and **run all examples**, see the main repository [README](../README.md) at the repo root.

---

## Repo layout

- **first-book/** – This book (markdown chapters).
- **examples/** – Example projects; each subdirectory (e.g. `chapter-01-hello/`) is a fir project you can build and run.
