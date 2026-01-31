# Chapter 3: Modules, Import, and Export

First programs can be split into **modules**: each file can declare a module name, **export** some functions or interactions, and **import** symbols from other modules. This chapter shows how to define a small library module, export it, and use it from a main module.

---

## Module declaration

At the top of a file you can give the current compilation unit a name with **module**:

```first
module Math;
```

- **module** – keyword.
- **Math** – the module name (an identifier). This is how other files will refer to this module when importing.

If you omit **module**, the compiler treats the file as the **main** module (the default). The main module is usually the entry point of your program (e.g. the file that contains **interaction main()**).

---

## Exporting functions and interactions

Only names that you **export** are visible to other modules. Use **export** before **function** or **interaction**:

```first
module Math;

export function square(x: Int) -> Int {
    return x * x;
}

export function add(x: Int, y: Int) -> Int {
    return x + y;
}
```

- **export function** – other modules can import and call **square** and **add**.
- Any **function** or **interaction** without **export** is internal to the module and cannot be imported.

You can also export interactions (e.g. **export interaction main()**), but usually the main module exports the entry point and library modules export pure functions or helpers.

---

## Importing from a module

In another file you **import** what you need. The compiler resolves the module by name (e.g. **"Math"**) and looks for a file **Math.first** or **Math/module.first** relative to the current working directory when you run the compiler.

### Import specific symbols

```first
import { square, add } "Math";
```

- **import** – keyword.
- **{ square, add }** – list of names to import (must be exported by **Math**).
- **"Math"** – module name as a string literal.

After this, you can call **square(5)** and **add(1, 2)** in the current file; they refer to the implementations from **Math**.

### Import everything

```first
import * "Math";
```

- **\*** – import all exported symbols from **Math**. You then use them by name (e.g. **square(5)**) as if they were defined in the current file.

### Import default

```first
import "Math";
```

- Imports the module with its default export (if any). The exact meaning depends on the language; in the current implementation this form also makes the module’s exports available.

---

## Small project: Math, Compute, and Main

The example project **examples/chapter-3-modules** has three modules:

1. **Math** – a small library that exports **square** and **add**.
2. **compute** – a module in **compute.first** that imports **Math** and exports **compute()** (3² + 4²).
3. **main** – the main module (the program entry) that imports **compute** from **compute.first** and defines **interaction main()** to print the result.

**Math.first** (library):

```first
module Math;

export function square(x: Int) -> Int {
    return x * x;
}

export function add(x: Int, y: Int) -> Int {
    return x + y;
}
```

**compute.first** (imports Math, exports compute):

```first
module compute;

import { square, add } "Math";

export function compute() -> Int {
    return add(square(3), square(4));
}
```

**src/main.first** (main program – imports compute from compute.first):

```first
module main;

import { compute } "compute";

interaction main() -> Unit {
    println("3² + 4² = " + intToString(compute()));
}
```

- **compute** imports **square** and **add** from **"Math"** and exports **compute()** (3² + 4² = 9 + 16 = 25).
- **main** imports **compute** from **"compute"** (the file **compute.first**) and calls it in **main()**.

---

## How the compiler finds modules

When the compiler sees **import { square, add } "Math"**, it looks for a file that defines the module **Math**. It tries, in the **current working directory** of the compiler process:

- **Math.first**
- **Math/module.first**
- Paths derived by replacing dots in the module name with slashes (e.g. **com/example/Math.first**)

So when you run **firstc**, the working directory must be one that contains **Math.first**, **compute.first** (or **compute/module.first**), and so on. The example’s **build.sh** script copies **Math.first**, **compute.first**, and **src/main.first** into the build directory and runs **firstc** from there so that all modules and the runtime library are found.

---

## Building and running the example

From the repository root:

```bash
cd examples/chapter-3-modules
./build.sh
./build/chapter-3-modules
```

**build.sh** copies **Math.first**, **compute.first**, and **src/main.first** into the repo’s **build** directory and runs **firstc** from there so that:

1. The compiler finds **Math.first** when resolving **import "Math"** (from **compute.first**).
2. The compiler finds **compute.first** when resolving **import "compute"** (from **main.first**).
3. The linker finds **libfirst_runtime** (when run from the main project’s **build** directory as in the script).

Expected output:

```
3² + 4² = 25
```

---

## Summary

1. **module** *Name*; – gives the current file a module name.
2. **export function** / **export interaction** – makes that name visible to other modules.
3. **import { a, b } "ModuleName"** – imports specific symbols from **ModuleName**.
4. **import \* "ModuleName"** – imports all exported symbols.
5. The compiler finds **ModuleName** by looking for **ModuleName.first** (or **ModuleName/module.first**) in the current working directory when you run **firstc**; build from a directory where those files are present (or use the example’s **build.sh**).

---

## Try it

- Add another exported function in **Math.first** (e.g. **cube(x: Int) -> Int**) and import it in **compute.first**.
- Use **import \* "Math"** in **compute.first** and call **square** and **add** the same way.
