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

In another file you **import** what you need. You can refer to a module either by a simple name (e.g. **"Math"**) or by a **path** (e.g. **"./src/compute"**). The compiler looks for **ModuleName.first** or **path.first** relative to the **current working directory** when you run the compiler (e.g. when you run **fir build**, that is the project directory).

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

## Importing by location: same directory, sibling, child, parent

Module paths are resolved **relative to the project root** (the directory from which you run **fir build**). You can organise modules in different folders and refer to them by path.

Assume the main file is **src/main.first**. Then:

| Kind | Where the module lives | Example path | Import in main |
|------|------------------------|--------------|----------------|
| **Same directory** | Next to **main.first** in **src/** | **src/same_dir.first** | `import { value } "./src/same_dir";` |
| **Sibling directory** | A folder beside **src/** (e.g. **sibling/**) | **sibling/sibling_mod.first** | `import { siblingValue } "./sibling/sibling_mod";` |
| **Child directory** | A subfolder under **src/** (e.g. **src/child/**) | **src/child/child_mod.first** | `import { childValue } "./src/child/child_mod";` |
| **Parent directory** | Project root (parent of **src/**) | **parent.first** at root | `import { parentValue } "./parent";` |

- **Same directory**: the module file is in the same folder as **main.first** (e.g. **src/**). Use a path like **"./src/same_dir"** so the compiler finds **src/same_dir.first**.
- **Sibling directory**: the module is in a directory that is a sibling of **src/** (e.g. **sibling/**). Use **"./sibling/sibling_mod"** for **sibling/sibling_mod.first**.
- **Child directory**: the module is under **src/** in a subfolder (e.g. **src/child/**). Use **"./src/child/child_mod"** for **src/child/child_mod.first**.
- **Parent directory**: the module is at the project root (one level above **src/**). Use **"./parent"** for **parent.first** in the project root.

The path string is the path to the module **without** the **.first** extension; the compiler appends **.first** when looking for the file.

---

## Small project: Math, Compute, and Main (plus same/sibling/child/parent)

The example project **examples/chapter-03-modules** demonstrates:

1. **Math** – a small library at project root that exports **square** and **add**.
2. **compute** – a module in **src/compute.first** that imports **Math** and exports **compute()** (3² + 4²).
3. **main** – the main module that imports **compute** and four other modules to illustrate **same directory**, **sibling**, **child**, and **parent** imports.

**Math.first** (at project root):

```first
module Math;

export function square(x: Int) -> Int {
    return x * x;
}

export function add(x: Int, y: Int) -> Int {
    return x + y;
}
```

**src/compute.first** (imports Math, exports compute):

```first
module compute;

import { square, add } "Math";

export function compute() -> Int {
    return add(square(3), square(4));
}
```

**src/main.first** (imports from same dir, sibling, child, parent, and compute):

```first
module main;

// 1. Same directory as main (src/)
import { value } "./src/same_dir";
// 2. Sibling directory (sibling of src/)
import { siblingValue } "./sibling/sibling_mod";
// 3. Child directory (under src/)
import { childValue } "./src/child/child_mod";
// 4. Parent directory (project root)
import { parentValue } "./parent";
import { compute } "./src/compute";

interaction main() -> Unit {
    println("1. Same dir (src/):       " + intToString(value()));
    println("2. Sibling (sibling/):   " + intToString(siblingValue()));
    println("3. Child (src/child/):   " + intToString(childValue()));
    println("4. Parent (project root): " + intToString(parentValue()));
    println("Compute (3² + 4²):       " + intToString(compute()));
}
```

- **compute** imports **square** and **add** from **"Math"** (project root) and exports **compute()** (3² + 4² = 25).
- **main** imports from **./src/same_dir**, **./sibling/sibling_mod**, **./src/child/child_mod**, **./parent**, and **./src/compute**, then prints each module’s value to show that all four import locations work.

---

## How the compiler finds modules

The compiler resolves module names and paths from the **current working directory** (when you run **fir build**, that is the project directory).

- **Simple name** (e.g. **"Math"**): it looks for **Math.first**, **Math/module.first**, and dot-to-slash variants (e.g. **com/example/Math.first**). It also looks under **lib/** and **FIRST_LIB_PATH** so the standard library (e.g. **Prelude**) is found.
- **Path** (e.g. **"./src/compute"** or **"./sibling/sibling_mod"**): it looks for **./src/compute.first**, **./sibling/sibling_mod.first**, etc., relative to the current working directory.

So when you run **fir build** from **examples/chapter-03-modules**, the working directory is that folder; **./src/same_dir**, **./sibling/sibling_mod**, **./src/child/child_mod**, **./parent**, and **./src/compute** all resolve to the corresponding **.first** files, and **"Math"** resolves to **Math.first** in the same directory.

---

## Building and running the example

From the example directory (so that path-based imports resolve correctly):

```bash
cd examples/chapter-03-modules
fir build
fir run
```

Or from the repository root, using the **fir** script in **tools/**:

```bash
./tools/fir build
./tools/fir run
```

**fir build** runs **firstc** from the **project directory** so that **./src/same_dir**, **./sibling/sibling_mod**, **./src/child/child_mod**, **./parent**, **./src/compute**, and **Math** are all found.

Expected output:

```
1. Same dir (src/):       100
2. Sibling (sibling/):   200
3. Child (src/child/):   300
4. Parent (project root): 400
Compute (3² + 4²):       25
```

---

## Summary

1. **module** *Name*; – gives the current file a module name.
2. **export function** / **export interaction** – makes that name visible to other modules.
3. **import { a, b } "ModuleName"** – imports specific symbols from **ModuleName** (simple name).
4. **import { a, b } "./path/to/module"** – imports from a module file at **path/to/module.first** relative to the project root (same directory, sibling, child, or parent).
5. **import \* "ModuleName"** – imports all exported symbols.
6. The compiler resolves module names and paths from the **current working directory** (use **fir build** from the project directory so path-based imports work).

---

## Try it

- Add another exported function in **Math.first** (e.g. **cube(x: Int) -> Int**) and import it in **compute.first**.
- Use **import \* "Math"** in **compute.first** and call **square** and **add** the same way.
- Add a new module in **src/** (same directory), **sibling/** (sibling), **src/child/** (child), or project root (parent), export a function, and import it in **main.first** using the right path.
