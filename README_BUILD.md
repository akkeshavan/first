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

### Packaging (installables for Apple and Linux, x86_64 and arm64)

Create distributable archives (TGZ, ZIP; DEB on Linux). All installables are written to the project **dist/** folder (created automatically when you run `cpack`). The package name includes OS and architecture (e.g. `first-compiler-0.1.0-Darwin-arm64.tar.gz`, `first-compiler-0.1.0-Linux-x86_64.tar.gz`).

**What's in the package (included in the installable; nothing is downloaded at install time):**
- **firstc** – the compiler binary
- **libfirst_runtime.a** – the runtime library (stdlib: I/O, math, string, array, HTTP, JSON, GC allocator, etc.)
- **include/first/runtime/** – runtime headers for linking compiled First code

The build **does** include the runtime and stdlib; users who install from the package get the compiler and runtime in one archive.

**What must already be on the target machine (not in the package):**
- **LLVM** and **ANTLR4** (C++ runtime) – required to build and run First programs
- **Boehm GC** (optional) – only if the package was built with `-DFIRST_USE_GC=ON` and you want GC-managed allocations

**Creating the Linux distribution (from macOS or Linux):** Run the Docker-based script so **dist/** gets Linux packages (TGZ, ZIP, DEB) without a native Linux machine:
```bash
./scripts/package-linux.sh   # requires Docker; builds image once, then build + cpack in container
```
This builds the compiler inside an Ubuntu 22.04 container (LLVM 15, ANTLR4 C++ runtime) and runs cpack; Linux artifacts appear in **dist/** (e.g. `first-compiler-0.1.0-Linux-x86_64.tar.gz`). Alternatively, build and run cpack natively on a Linux x86_64 or aarch64 machine.

**Quick package (current machine):**
```bash
./scripts/package.sh
# Artifacts in dist/
```

**macOS: build for a specific architecture:**
```bash
./scripts/package.sh arm64    # Apple Silicon
./scripts/package.sh x86_64  # Intel
```

**Package without Boehm GC:** `./scripts/package.sh --no-gc`

**Manual packaging:**
```bash
mkdir build-pkg && cd build-pkg
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local -DFIRST_USE_GC=ON
make -j4
cpack
# Produces first-compiler-<ver>-<OS>-<arch>.tar.gz and .zip in dist/ (and .deb on Linux)
```

**Multi-arch:** Build on each target architecture (or use CI). On macOS, use `./scripts/package.sh arm64` or `./scripts/package.sh x86_64`. On Linux, build in native x86_64 or aarch64 environments (e.g. Docker or separate machines).

**Installing from a package (global install, PATH set):**
- **macOS via Homebrew (recommended):** If you have a Homebrew tap set up, install and uninstall with:
  ```bash
  brew tap akkeshavan/first    # one-time: add the tap (repo must be named homebrew-first)
  brew install first-compiler
  # Uninstall:
  brew uninstall first-compiler
  ```
  Or install the formula file from this repo: `brew install --build-from-source ./homebrew/first-compiler.rb`. Homebrew will install LLVM, ANTLR4 runtime, and ANTLR tool as dependencies and put `firstc` on your PATH. To set up the tap so others can `brew tap akkeshavan/first && brew install first-compiler`, create a GitHub repo named `homebrew-first` and add `Formula/first-compiler.rb` (copy from this repo’s `homebrew/first-compiler.rb`).
- **macOS .pkg:** Double-click `dist/first-compiler-*-Darwin-*.pkg` or run `open dist/first-compiler-*-Darwin-*.pkg`. The installer puts the compiler and runtime under `/usr/local` (bin, lib, include). Ensure `/usr/local/bin` is in your PATH (it usually is on macOS).
- **macOS / Linux from TGZ:** Run the install script for a global install and PATH setup:
  ```bash
  # From the project (or after downloading the TGZ)
  ./scripts/install.sh [path-to-first-compiler-*.tar.gz] [/usr/local]
  # For /usr/local you may need: sudo ./scripts/install.sh dist/first-compiler-*-Darwin-arm64.tar.gz /usr/local
  ```
  The script extracts to the given prefix (default `/usr/local`) and adds the prefix `bin` to your shell profile (`.zshrc` or `.bash_profile`) if needed. Then run `firstc --help` in a new terminal.
- **Manual:** Extract the TGZ and copy `bin/`, `lib/`, `include/` into your chosen prefix (e.g. `/usr/local`); add `prefix/bin` to PATH.

**Uninstalling:** If you installed to `/usr/local` (via .pkg or install.sh), run:
```bash
sudo ./scripts/uninstall.sh /usr/local
```
Or remove by hand: `rm -f /usr/local/bin/firstc /usr/local/lib/libfirst_runtime.a` and `rm -rf /usr/local/include/first`. If install.sh added a PATH line to `~/.zshrc` or `~/.bash_profile`, remove that line. If `firstc` still runs after uninstall, you may have another `firstc` on PATH (e.g. the Haskell First compiler); run `which firstc` to see where it is and remove that binary if you want.

The compiler and runtime require LLVM and ANTLR4 (and optionally Boehm GC) to be installed on the target system when building First programs.

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
