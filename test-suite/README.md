# First compiler test suite (parsing and typechecking)

This folder contains a **data-driven test suite** for the First compiler, focused on **parsing** and **typechecking** correctness. Code generation is **not** exercised here; the existing IR tests in `tests/` cover that.

## Layout

- **parser/pass/** — Programs that must **parse** (and typecheck) successfully. Covers all valid syntax.
- **parser/fail/** — Programs that must **fail at parse time** (syntax errors).
- **typecheck/pass/** — Programs that must **typecheck** successfully (valid types, interfaces, generics, etc.).
- **typecheck/fail/** — Programs that must **fail at typecheck** (type errors, undefined names, constraint violations, etc.).

## Manifest

`manifest.txt` lists one test per line:

```
pass parser <name>
fail parser <name>
pass typecheck <name>
fail typecheck <name>
```

The runner looks for `parser/pass/<name>.first`, `parser/fail/<name>.first`, and so on.

## Running

The suite is run as part of the main test executable. From the build directory:

```bash
ctest -R CompilerTests
# or
./bin/test_compiler
```

To run only the test-suite (parser + typecheck), no codegen:

```bash
./bin/test_compiler test_suite_parser_and_typecheck
```

Note: the data-driven suite runs one full compile per manifest line (~126 tests), so it may take a minute or two.

The runner uses the path given by the `FIRST_TEST_SUITE_PATH` compile definition (set by CMake to the repo’s `test-suite` directory). Tests that need Prelude use the lib path `FIRST_TEST_SUITE_PATH/../lib`.

## For-in suite (separate)

For-in tests (`for x in range`, `for x in array`) are in **`for-in/`** with their own manifest. They are not included in the main manifest because for-in handling is still being fixed. To run only for-in tests:

```bash
FIRST_TEST_SUITE_PATH="$(pwd)/test-suite/for-in" ./bin/test_compiler test_suite_parser_and_typecheck
```

See `for-in/README.md` for details.

## Adding tests

1. Add a `.first` file in the right directory (e.g. `typecheck/fail/undefined_variable.first`).
2. Append a line to `manifest.txt` (e.g. `fail typecheck undefined_variable`).
3. Re-run the test executable (no recompile needed for manifest changes; the manifest is read at runtime, but the path is compile-time so a rebuild is needed if you moved the repo).
