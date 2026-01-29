# Building the First Compiler

## Prerequisites

### Required Dependencies

1. **CMake** (3.20 or higher)
   ```bash
   # macOS
   brew install cmake
   
   # Ubuntu/Debian
   sudo apt-get install cmake
   ```

2. **LLVM** (15.0 or higher)
   ```bash
   # macOS
   brew install llvm
   
   # Ubuntu/Debian
   sudo apt-get install llvm-15-dev libllvm15
   ```

3. **ANTLR4 Runtime** (C++) — used only for lexing
   ```bash
   # macOS
   brew install antlr4-cpp-runtime
   
   # Ubuntu/Debian
   sudo apt-get install libantlr4-runtime-dev
   ```

4. **ANTLR4 Tool** (for generating lexer)
   ```bash
   pip install antlr4-tools
   # Or download from https://www.antlr.org/download.html
   ```

### Optional Dependencies

- **Catch2** or **Google Test** (for testing framework)
- **Doxygen** (for documentation)

- **Boehm GC** (for optional garbage-collected runtime; recommended on Apple Silicon so the runtime uses the correct libgc)
   ```bash
   # macOS (required for GC build on Apple Silicon; use Homebrew to get arm64 lib)
   brew install bdw-gc
   
   # Ubuntu/Debian
   sudo apt-get install libgc-dev
   ```
   Then build with `-DFIRST_USE_GC=ON` so stdlib string/JSON allocations are GC-managed (no explicit free).

## Building

### Standard Build

```bash
mkdir build
cd build
cmake ..
make
```

### Build Options

```bash
cmake -DBUILD_TESTS=ON -DBUILD_EXAMPLES=ON ..
```

### Running Tests

```bash
cd build
make test
# Or
ctest
```

### Installing

```bash
cd build
make install
```

## Project Structure

```
first/
├── CMakeLists.txt          # Main build configuration
├── include/                 # Header files
│   └── first/
│       ├── compiler.h
│       ├── error_reporter.h
│       └── source_location.h
├── src/                    # Source files
│   ├── main.cpp
│   ├── compiler.cpp
│   ├── error_reporter.cpp
│   └── source_location.cpp
├── tests/                  # Test files
│   ├── CMakeLists.txt
│   └── test_*.cpp
├── docs/                   # Documentation
│   ├── First.g4           # ANTLR4 grammar
│   └── ...
└── build/                  # Build directory (generated)
```

## Troubleshooting

### ANTLR4 Not Found

If CMake can't find ANTLR4:
```bash
# Set ANTLR4_ROOT if installed in non-standard location
cmake -DANTLR4_ROOT=/path/to/antlr4 ..
```

### LLVM Not Found

If CMake can't find LLVM:
```bash
# Set LLVM_DIR to point to LLVMConfig.cmake
cmake -DLLVM_DIR=/usr/lib/llvm-15/lib/cmake/llvm ..
```

### Lexer Generation Fails

Make sure `antlr4` command is in your PATH (needed to generate the `FirstLexer`
from `docs/First.g4` at configure time):
```bash
which antlr4
# If not found, add to PATH or use full path in CMakeLists.txt
```

## Development

### Adding New Source Files

1. Add source file to `src/`
2. Add header to `include/first/`
3. Update `CMakeLists.txt` with new file in `COMPILER_SOURCES`

### Running the Compiler

```bash
./build/bin/firstc --help
./build/bin/firstc --version
./build/bin/firstc examples/hello.first
```
