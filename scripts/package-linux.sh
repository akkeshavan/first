#!/usr/bin/env bash
# Build Linux installables (TGZ, ZIP, DEB) using Docker and put them in dist/.
# Run from the project root on macOS or Linux (with Docker installed).
# Usage: ./scripts/package-linux.sh

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${PROJECT_DIR}"

if ! command -v docker &>/dev/null; then
    echo "Docker is required. Install: https://docs.docker.com/get-docker/"
    exit 1
fi

IMAGE_NAME="first-linux-build"
mkdir -p dist

echo "Building Docker image (one-time or when Dockerfile changes)..."
docker build -f docker/Dockerfile.linux -t "${IMAGE_NAME}" .

echo "Building First compiler for Linux and running cpack..."
docker run --rm \
    -v "${PROJECT_DIR}:/project" \
    -w /project \
    "${IMAGE_NAME}"

echo "Done. Linux packages in dist/:"
ls -la dist/*.tar.gz dist/*.zip 2>/dev/null || true
ls -la dist/*.deb 2>/dev/null || true
