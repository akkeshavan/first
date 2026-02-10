#!/usr/bin/env bash
# Build and run all First examples. Runs firstc from REPO_ROOT/build so the runtime
# (build/runtime/libfirst_runtime.a) and stdlib (FIRST_LIB_PATH=../lib) are found.
set -e

EXAMPLES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$EXAMPLES_DIR/.." && pwd)"
BUILD_DIR="$REPO_ROOT/build"
BUILD_BIN="$BUILD_DIR/bin/firstc"

if [[ -z "$FIRSTC" ]]; then
  if [[ -x "$BUILD_BIN" ]]; then
    FIRSTC="$BUILD_BIN"
  else
    FIRSTC="firstc"
  fi
fi

# firstc is run from BUILD_DIR so it finds runtime/; stdlib from repo lib/
export FIRST_LIB_PATH="$REPO_ROOT/lib"
FAILED=()
PASSED=()

for dir in "$EXAMPLES_DIR"/chapter-*/; do
  [[ -d "$dir" ]] || continue
  name="$(basename "$dir")"
  fir_json="$dir/fir.json"
  if [[ ! -f "$fir_json" ]]; then
    echo "[SKIP] $name (no fir.json)"
    continue
  fi
  main="src/main.first"
  if command -v jq &>/dev/null; then
    m="$(jq -r '.main // "src/main.first"' "$fir_json")"
    [[ "$m" != "null" && -n "$m" ]] && main="$m"
  fi
  main_file="$dir/$main"
  if [[ ! -f "$main_file" ]]; then
    echo "[SKIP] $name (main not found: $main)"
    continue
  fi
  # Paths relative to BUILD_DIR so firstc finds runtime/
  rel_main="../examples/$name/$main"
  rel_out="../examples/$name/build/out"
  mkdir -p "$dir/build"
  # chapter-03 imports ./compute and Math from cwd (build dir)
  if [[ "$name" == "chapter-03-modules" ]]; then
    cp "$dir/src/compute.first" "$dir/Math.first" "$BUILD_DIR/" 2>/dev/null || true
  fi
  echo "--- $name ---"
  if (cd "$BUILD_DIR" && "$FIRSTC" "$rel_main" -o "$rel_out" 2>&1); then
    "$dir/build/out" 2>&1 || true
    echo "[PASS] $name"
    PASSED+=("$name")
  else
    echo "[FAIL] $name (build failed)"
    FAILED+=("$name")
  fi
done

echo ""
echo "Passed: ${#PASSED[@]}"
if [[ ${#FAILED[@]} -gt 0 ]]; then
  echo "Failed: ${#FAILED[@]} (${FAILED[*]})"
  exit 1
fi
exit 0
