# Chapter 1: Getting Started

## Introduction to First

First is a **functional-first** programming language with strong, static typing and a clear split between pure functions and side-effecting **interactions**. You write interactions when you need I/O, mutable state, or other effects; the rest of your code stays pure. The compiler targets native code via LLVM and comes with a small runtime and standard library. The **fir** tool (included with the compiler) lets you create projects, build, and run them with simple commands.

This chapter gets you set up: installing First on macOS, creating a “Hello, World” project under **examples**, and using **print** and **println** to produce output.

---

## Installing First

**First is currently only available on macOS** and can be installed using [Homebrew](https://brew.sh).

Install the compiler and **fir** (the First project manager) with:

```bash
brew tap akkeshavan/first
brew install --HEAD first-compiler
```

> The `--HEAD` flag installs from the latest `main` branch. Once a stable release (e.g. `v0.1.0`) is tagged, you can use `brew install first-compiler` without `--HEAD`.

Then check:

```bash
firstc --help
fir
```

---

## Creating the Hello project in the examples directory

We’ll create a small “Hello” project inside the repo’s **examples** directory so it lives with the other sample code.

1. **Go to the repo and into examples**

   ```bash
   mkdir /path/to/first-tutorial
   cd /path/to/first-tutorial  // create a new directory and switch to it
   
   ```

2. **Create a new First project named `hello`**

   ```bash
   fir init hello
   ```

   This creates **examples/chapter-01-hello/** with:

   - **fir.json** – project name and entry point
   - **src/main.first** – main source file
   - **.gitignore** – ignores `build/` and similar

3. **Build the project**

   ```bash
   cd hello
   fir build
   ```

   This compiles **src/main.first** and produces an executable in **hello/build/**.

4. **Run the program**

   ```bash
   fir run
   ```

   **fir run** builds if needed, then runs the executable. You should see:

   ```
   Hello from First!
   ```

The source for this program is **examples/chapter-01-hello/src/main.first**. You can edit it and run `fir run` again to see changes.

---

## print and println

First uses two main functions for writing text to the terminal:

- **print(** *string* **)** – Writes the string to stdout with **no** newline at the end.
- **println(** *string* **)** – Writes the string to stdout and then a newline.

Both take a single string. String literals use double quotes and support escape sequences such as `\n` (newline) and `\t` (tab).

**Example: print**

```first
interaction main() -> Unit {
    print("Hello, ");
    print("World!");
}
```

Output: `Hello, World!` (no newline at the end).

**Example: println**

```first
interaction main() -> Unit {
    println("Hello, World!");
}
```

Output:

```
Hello, World!
```

**Example: newlines and tabs**

```first
interaction main() -> Unit {
    println("Line 1");
    print("Line 2\n");
    print("Col1\tCol2\n");
}
```

Output:

```
Line 1
Line 2
Col1    Col2
```

A full runnable example is in **examples/chapter-01-print-and-println**. From the repo root:

```bash
cd examples/chapter-01-print-and-println
fir run
```

You can also try the snippets above in **examples/chapter-01-hello** by editing **src/main.first** and running `fir run`.

---

## What’s next

- Change **examples/chapter-01-hello/src/main.first** to use `println` instead of `print` and add more `print`/`println` calls.
- Explore other examples under **examples/** (each subdirectory is a fir project).
- Continue with the next chapters for types, functions, and more of the language.
