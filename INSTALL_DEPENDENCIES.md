# Installing Dependencies for First Compiler

## macOS Installation (using Homebrew)

### Step 1: Install Homebrew (if not already installed)
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### Step 2: Install Required Dependencies

```bash
# Install CMake
brew install cmake

# Install LLVM
brew install llvm

# Install ANTLR4 C++ runtime
brew install antlr4-cpp-runtime

# Install ANTLR4 tool (Python package)
pip3 install antlr4-tools
```

### Step 3: Verify Installation

```bash
# Check CMake
cmake --version

# Check LLVM
llvm-config --version

# Check ANTLR4 tool
antlr4 --version
```

### Step 4: Set Environment Variables (if needed)

If LLVM is installed but not found, you may need to set:

```bash
# For Intel Macs
export LLVM_DIR=/usr/local/lib/cmake/llvm

# For Apple Silicon Macs
export LLVM_DIR=/opt/homebrew/lib/cmake/llvm

# Add to ~/.zshrc or ~/.bash_profile for persistence
```

## Alternative: Manual Installation

### CMake
Download from: https://cmake.org/download/
Or use: `brew install cmake`

### LLVM
Download from: https://llvm.org/releases/
Or use: `brew install llvm`

### ANTLR4
- Runtime: `brew install antlr4-cpp-runtime`
- Tool: `pip3 install antlr4-tools`

## Quick Install Script

```bash
# Run this to install all dependencies at once
brew install cmake llvm antlr4-cpp-runtime
pip3 install antlr4-tools
```

## After Installation

Once dependencies are installed, you can build:

```bash
cd /Users/anand-air/BoundlessProgramming/first
./setup.sh  # Verify setup
mkdir build && cd build
cmake ..
make
```
