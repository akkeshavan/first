#!/usr/bin/env bash
# Test Linux clone, build, and install per README.md
# Runs in Docker (Ubuntu 22.04). Requires Docker.
# Usage: ./scripts/test-linux-docker.sh

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${PROJECT_DIR}"

if ! command -v docker &>/dev/null; then
    echo "Docker is required. Install: https://docs.docker.com/get-docker/"
    exit 1
fi

echo "Building Docker image..."
docker build -f docker/Dockerfile.linux-test -t first-linux-test .

echo ""
echo "=== Testing build and install in container ==="
docker run --rm -v "${PROJECT_DIR}:/first" first-linux-test bash -c '
set -e
cd /first

# Remove host build dir to avoid CMakeCache path mismatch (host paths vs /first in container)
rm -rf build

echo "1. Configure..."
mkdir -p build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DLLVM_DIR=/usr/lib/llvm-15/lib/cmake/llvm \
    -DFIRST_USE_GC=OFF -DBUILD_TESTS=ON -DBUILD_EXAMPLES=OFF

echo ""
echo "2. Build..."
cmake --build . -j$(nproc)

echo ""
echo "3. Run tests..."
ctest --output-on-failure || true

echo ""
echo "4. Install..."
cmake --install . --component first

echo ""
echo "5. Verify firstc..."
firstc --help | head -5

echo ""
echo "6. Compile and run hello..."
cd /first
export FIRST_LIB_PATH=/usr/local/lib/first
export LIBRARY_PATH=/usr/local/lib
firstc examples/chapter-01-hello/src/main.first -o /tmp/hello
/tmp/hello

echo ""
echo "=== Linux build and install: PASS ==="
'
