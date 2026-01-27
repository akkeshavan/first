#!/bin/bash

# Setup script for First Compiler development environment

set -e

echo "First Compiler - Development Environment Setup"
echo "================================================"
echo ""

# Check for required tools
check_command() {
    if ! command -v $1 &> /dev/null; then
        echo "❌ $1 not found"
        return 1
    else
        echo "✅ $1 found: $(which $1)"
        return 0
    fi
}

echo "Checking prerequisites..."
echo ""

# Check CMake
if ! check_command cmake; then
    echo "Please install CMake:"
    echo "  macOS: brew install cmake"
    echo "  Ubuntu: sudo apt-get install cmake"
    exit 1
fi

# Check LLVM
if ! check_command llvm-config; then
    echo "⚠️  llvm-config not found in PATH"
    echo "Please ensure LLVM is installed:"
    echo "  macOS: brew install llvm"
    echo "  Ubuntu: sudo apt-get install llvm-15-dev"
    echo ""
    echo "You may need to set LLVM_DIR manually:"
    echo "  export LLVM_DIR=/path/to/llvm/lib/cmake/llvm"
fi

# Check ANTLR4 tool
if ! check_command antlr4; then
    echo "⚠️  antlr4 tool not found"
    echo "Installing via pip..."
    pip install antlr4-tools
    if ! check_command antlr4; then
        echo "Please install ANTLR4 manually:"
        echo "  pip install antlr4-tools"
        echo "  Or download from https://www.antlr.org/download.html"
    fi
fi

# Check for ANTLR4 runtime (C++)
echo ""
echo "Checking for ANTLR4 C++ runtime..."
if pkg-config --exists antlr4-runtime 2>/dev/null; then
    echo "✅ ANTLR4 runtime found via pkg-config"
else
    echo "⚠️  ANTLR4 runtime not found via pkg-config"
    echo "You may need to install it:"
    echo "  macOS: brew install antlr4-cpp-runtime"
    echo "  Ubuntu: sudo apt-get install libantlr4-runtime-dev"
fi

echo ""
echo "Setup complete!"
echo ""
echo "Next steps:"
echo "  1. mkdir build && cd build"
echo "  2. cmake .."
echo "  3. make"
echo ""
echo "If you encounter issues, see README_BUILD.md"
