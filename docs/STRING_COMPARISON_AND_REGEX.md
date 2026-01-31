# String Comparison and Regular Expression Support

## Summary

This document describes the string comparison and regular expression features added to the First language compiler and runtime.

## Features Implemented

### 1. String Comparison Operators

String comparison operators now work with string operands, performing lexicographic comparison:

**Operators:**
- `==` (equality)
- `!=` (inequality)
- `<` (less than)
- `<=` (less than or equal)
- `>` (greater than)
- `>=` (greater than or equal)

**Example:**
```first
"apple" < "banana"    // true
"hello" == "hello"    // true
"zebra" >= "apple"    // true
```

**Implementation:**
- Compiler: `src/ir/ir_generator.cpp` - Added string comparison cases for all comparison operators
- Runtime: `runtime/src/runtime/string.cpp` - Implements `first_string_equals` and `first_string_compare` (C functions)

### 2. String Comparison Functions

Explicit functions for string comparison:

**stringEquals(s1: String, s2: String) -> Bool**
```first
stringEquals("hello", "hello")  // true
```

**stringCompare(s1: String, s2: String) -> Int**
Returns:
- `-1` if s1 < s2
- `0` if s1 == s2
- `1` if s1 > s2

```first
stringCompare("apple", "banana")  // -1
```

### 3. Regular Expression Functions

Standard regex pattern matching using C++ `<regex>` library:

**regexMatches(str: String, pattern: String) -> Int**
- Returns `1` if string matches pattern
- Returns `0` if no match
- Returns `-1` on regex error

```first
regexMatches("hello123", "[a-z]+[0-9]+")  // 1
```

**regexSearch(str: String, pattern: String) -> Int**
- Returns index of first match
- Returns `-1` if not found

```first
regexSearch("The year is 2024", "[0-9]+")  // 12
```

**regexReplace(str: String, pattern: String, replacement: String) -> String**
- Replaces first occurrence

```first
regexReplace("hello world", "world", "First")  // "hello First"
```

**regexReplaceAll(str: String, pattern: String, replacement: String) -> String**
- Replaces all occurrences

```first
regexReplaceAll("foo bar foo", "foo", "baz")  // "baz bar baz"
```

**regexExtract(str: String, pattern: String, groupIndex: Int) -> String**
- Extracts capturing group (0 = whole match, 1+ = groups)

```first
regexExtract("Price: $42.50", "\\$([0-9.]+)", 1)  // "42.50"
```

## Files Modified/Created

### Compiler
- `src/ir/ir_generator.cpp` - Added string comparison operator codegen
- `src/semantic/type_checker.cpp` - Added type signatures for string comparison and regex functions

### Runtime
- `runtime/include/first/runtime/string.h` - Added C function declarations
- `runtime/src/runtime/string.cpp` - Implemented string comparison C functions
- `runtime/include/first/runtime/regex.h` - NEW: Regex function declarations
- `runtime/src/runtime/regex.cpp` - NEW: Regex function implementations
- `runtime/src/runtime/stdlib.cpp` - Added wrapper functions exposed to First
- `runtime/CMakeLists.txt` - Added regex.cpp to build

### Documentation
- `first-book/chapter-04-basic-types-expressions-type-inference.md` - Added sections on string comparison and regex

### Examples
- `examples/chapter-4-basic-expressions-types/src/main.first` - Added string comparison and regex demonstrations

## Building

The features are automatically included when building the compiler and runtime:

```bash
cd /path/to/first
cmake --build build --target firstc
cmake --build build --target first_runtime
```

## Testing

Run the updated chapter 4 example:

```bash
cd examples/chapter-4-basic-expressions-types
fir run
```

Expected output includes:
- String comparison results
- Regex match and search results
- All basic expression types demonstrated

## Known Limitations

1. **String replace functions**: The `regexReplace` and `regexReplaceAll` functions may have issues with null pointer handling when concatenating results in First code. This is being investigated.

2. **Regex split**: The `regexSplit` function is implemented in runtime but not yet exposed or tested in First language examples (returns array of strings).

3. **Error handling**: Regex errors return -1 or nullptr. More detailed error messages could be added.

## Future Enhancements

1. Add proper error reporting for invalid regex patterns
2. Expose `regexSplit` function with array support
3. Add case-insensitive regex matching options
4. Support for common regex flags (multiline, global, etc.)
5. String interpolation with regex backreferences
