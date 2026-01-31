# The First Programming Language Book

A practical guide to the First programming language: installation, the **fir** project manager, and the core language.

All example code lives in the repository’s **examples** directory. You can run examples with **fir** (see Chapter 1).

---

## Chapters

1. **[Getting Started](chapter-01-getting-started.md)** – Introduction, install (macOS and Linux), create the hello project with fir, and use `print` and `println`.

2. **[Functions and Interactions](chapter-02-functions-and-interactions.md)** – Pure **function**s vs side-effecting **interaction**s, when to use which, and examples.

3. **[Modules, Import, and Export](chapter-03-modules-import-export.md)** – Split code into modules, **export** functions, and **import** from other modules with a small project example.

4. **[Basic Types, Expressions, and Type Inference](chapter-04-basic-types-expressions-type-inference.md)** – **Int**, **Float**, **Bool**, **String**, **Unit**; literals; arithmetic, comparison, and logical expressions; optional type annotations and inference for **let** / **var**.

5. *(Planned)* **Arrays and Records** – Array and record types, literals, and usage.

---

## Chapters and examples

The book has **Chapters 1–4**; the **examples** folder has matching projects for each:

| Chapter | Book chapter | Example project(s) |
|--------|---------------|--------------------|
| 1 | Getting Started | **examples/chapter-1-hello**, **examples/chapter-1-print-and-println** |
| 2 | Functions and Interactions | **examples/chapter-2-functions-and-interactions** |
| 3 | Modules, Import, and Export | **examples/chapter-3-modules** |
| 4 | Basic Types, Expressions, and Type Inference | **examples/chapter-4-basic-expressions-types** |

From the repo root, run an example with: `cd examples/chapter-N-... && fir run`.

---

## Repo layout

- **first-book/** – This book (markdown chapters).
- **examples/** – Example projects; each subdirectory (e.g. `chapter-1-hello/`) is a fir project you can build and run.
