#!/usr/bin/env bash
# Build chapter-03-modules: copy sources into repo build dir so firstc finds Math.first,
# then run firstc from there (so runtime is found) and write executable here.
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BUILD_DIR="$REPO_ROOT/build"
mkdir -p "$SCRIPT_DIR/build"
cp "$SCRIPT_DIR/Math.first" "$BUILD_DIR/"
cp "$SCRIPT_DIR/compute.first" "$BUILD_DIR/"
cp "$SCRIPT_DIR/src/main.first" "$BUILD_DIR/main.first"
cd "$BUILD_DIR"
"$BUILD_DIR/bin/firstc" main.first -o "$SCRIPT_DIR/build/chapter-03-modules"
echo "Built: $SCRIPT_DIR/build/chapter-03-modules"
