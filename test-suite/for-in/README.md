# For-in test suite

This directory is a **separate test suite** for `for .. in` (ranges and arrays). These tests are not run as part of the main parser/typecheck suite because for-in handling is still being fixed (e.g. parser/typecheck may not yet support all cases).

## Layout

Same as the main suite: `manifest.txt` plus `parser/{pass,fail}/` and `typecheck/{pass,fail}/` with `.first` files.

## Tests (positive and negative)

**Parser pass (valid syntax):** for_in_range, for_in_range_inclusive, range_exclusive, range_inclusive, range_step, for_in_array, for_in_empty_body, for_in_multiple_statements.

**Parser fail (syntax errors):** for_in_missing_in, for_in_missing_var, for_in_missing_lbrace.

**Typecheck pass (valid types):** range_for_in, array_for_in, for_in_range_inclusive_typed, for_in_range_step_typed, for_in_use_loop_var, for_in_exclusive_typed.

**Typecheck fail (type/semantic errors):** for_in_in_function (for-in only in interactions), range_element_type (wrong loop variable type), for_in_over_non_iterable (e.g. for x in 42), for_in_assign_loop_var (assign to immutable loop var), for_in_array_element_wrong_type (Int used as String), for_in_over_bool (Bool not iterable).

## How to run

From the **build** directory, set the environment variable to the for-in suite (relative or absolute):

```bash
cd /path/to/first/build
FIRST_TEST_SUITE_PATH="../test-suite/for-in" ./bin/test_compiler test_suite_parser_and_typecheck
```

Or with absolute path:

```bash
FIRST_TEST_SUITE_PATH="$(pwd)/test-suite/for-in" ./bin/test_compiler test_suite_parser_and_typecheck
```

The runner prefers `FIRST_TEST_SUITE_PATH` from the environment over the CMake default, so no rebuild is needed to switch suites.

## When to merge back

Once for-in parsing and typechecking are stable and all tests here pass, you can either re-enable the for-in lines in the main `test-suite/manifest.txt` or keep this as a dedicated for-in suite and run both in CI.
