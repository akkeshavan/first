#!/usr/bin/env bash
# Runs inside the Linux Docker container: configure, build, cpack.
# Expects /project to be the First project root (mounted).
set -e
cd /project
mkdir -p dist
mkdir -p build-linux
cd build-linux
cmake .. \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DLLVM_DIR="${LLVM_DIR:-/usr/lib/llvm-15/lib/cmake/llvm}" \
    -DFIRST_USE_GC=OFF \
    -DBUILD_TESTS=ON \
    -DBUILD_EXAMPLES=OFF
make -j$(nproc)
ctest --output-on-failure || true
cpack
echo "Linux packages in dist/:"
ls -la ../dist/*.tar.gz ../dist/*.zip ../dist/*.deb 2>/dev/null || true
