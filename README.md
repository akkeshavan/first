# First Programming Language

First is a **functional-first programming language** that emphasizes pure functional programming while providing **controlled access to imperative features**. It features a strict distinction between pure functions and side-effecting interactions, strong typing with TypeScript-like expressiveness, and Haskell-style type classes.

**First is currently only available on macOS** and can be installed via [Homebrew](https://brew.sh): `brew tap akkeshavan/first && brew install --HEAD first-compiler`. 

You may  clone and build the project on Linux, but has not been tested yet. There is no installable available for Linux yet

## Key Features

- **Functional-first**: Primary programming paradigm emphasizing pure functions
- **Controlled imperative**: Imperative features (mutable state, loops) restricted to interaction functions
- **Strong typing**: Static type checking with comprehensive type inference
- **Advanced type system**: Supports refinement types, dependent types, union types, intersection types, and more
- **Haskell integration**: Seamless integration with Haskell libraries through automatic wrapper generation
- **Effect isolation**: Mutable state and loops are restricted to interaction functions only
- **Memory safety**: Guaranteed through the type system and effect restrictions

## Language Principles

- **Pure functions by default**: Functions are pure unless explicitly marked as interactions
- **Explicit side effects**: Side effects are contained within interaction functions
- **Immutable by default**: Variables are immutable unless explicitly marked as mutable
- **Effect isolation**: Mutable state and loops are restricted to interaction functions only

---

## Clone and build (from source)

To build the First compiler and runtime from source (macOS):

1. **Clone the repository**

   ```bash
   git clone https://github.com/akkeshavan/first.git
   cd first
   ```

2. **Install dependencies** (Homebrew)

   - LLVM: `brew install llvm`
   - ANTLR4 runtime and tool: `brew install antlr4-cpp-runtime antlr`
   - CMake: `brew install cmake` (if not already installed)

3. **Configure and build**

   ```bash
   mkdir build && cd build
   cmake .. -DCMAKE_BUILD_TYPE=Release
   cmake --build .
   ```

   The compiler binary is `build/bin/firstc`. The **fir** project manager is in `tools/fir`; run it from the repo (e.g. `./tools/fir build`) or add `tools` to your PATH.

## Run all tests

From the repository root, after building:

```bash
cd build
ctest --output-on-failure
```

This runs the compiler unit tests and (if enabled) runtime tests. You can also run the compiler test executable directly:

```bash
./build/bin/test_compiler
```

## Run all examples

Each subdirectory under **examples/** (e.g. **chapter-01-hello**, **chapter-03-modules**) is a First project with a **fir.json** and **src/main.first**.

**Using the local build** (compiler in `build/bin/firstc`):

```bash
./examples/run-all-local.sh
```

Run this from the repository root. It builds and runs every example using the compiler and runtime from **build/**.

**Using the Homebrew-installed compiler**:

```bash
./examples/run-all-brew.sh
```

Requires `brew install --HEAD first-compiler`. Uses the globally installed **firstc** and runs all examples.

To run a single example:

```bash
cd examples/chapter-01-hello
fir build
fir run
```

(Use `./tools/fir` if **fir** is not on your PATH.)

---

## Documentation

See the [Language Specification](./docs/First-Language-Specification.md) for complete documentation on the First programming language.

The grammar definition is available in [First.g4](./docs/First.g4) (ANTLR4 format).

## Project Status

This project is in active development. The language specification and grammar are defined, and the compiler implementation is underway.

## License

[To be determined]
