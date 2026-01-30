#!/usr/bin/env bash
# Build and create installable packages for First compiler (macOS and Linux, x86_64 and arm64).
# Usage:
#   ./scripts/package.sh              # build for current arch, create TGZ + ZIP
#   ./scripts/package.sh arm64        # macOS: build for arm64 only
#   ./scripts/package.sh x86_64       # macOS: build for Intel only
#   ./scripts/package.sh --no-gc      # build without Boehm GC (default: use GC if available)

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${PROJECT_DIR}"

USE_GC=ON
ARCH=""
for arg in "$@"; do
    case "$arg" in
        --no-gc) USE_GC=OFF ;;
        arm64|x86_64|aarch64) ARCH="$arg" ;;
    esac
done

# Build directory: include arch in name when cross-building
if [[ -n "$ARCH" ]]; then
    BUILD_DIR="${PROJECT_DIR}/build-${ARCH}"
else
    BUILD_DIR="${PROJECT_DIR}/build-package"
fi
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

# Configure: optional arch on macOS
if [[ "$(uname -s)" == "Darwin" && -n "$ARCH" ]]; then
    if [[ "$ARCH" == "x86_64" ]]; then
        export CMAKE_OSX_ARCHITECTURES="x86_64"
    else
        export CMAKE_OSX_ARCHITECTURES="arm64"
    fi
fi

echo "Configuring (FIRST_USE_GC=${USE_GC}, ARCH=${ARCH:-native})..."
cmake "${PROJECT_DIR}" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr/local \
    -DFIRST_USE_GC="${USE_GC}" \
    -DBUILD_TESTS=ON \
    -DBUILD_EXAMPLES=OFF

echo "Building..."
cmake --build . -j"$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"

echo "Running tests..."
ctest --output-on-failure || true

echo "Creating packages (output in dist/)..."
mkdir -p "${PROJECT_DIR}/dist"
cpack

echo "Done. Artifacts in dist/:"
ls -la "${PROJECT_DIR}/dist"/*.tar.gz "${PROJECT_DIR}/dist"/*.zip 2>/dev/null || true
ls -la "${PROJECT_DIR}/dist"/*.deb 2>/dev/null || true
