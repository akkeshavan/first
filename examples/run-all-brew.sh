#!/usr/bin/env bash
# Build and run all First examples using the globally installed compiler (brew install first-compiler).
# Requires: brew tap akkeshavan/first && brew install --HEAD first-compiler
set -e

EXAMPLES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$EXAMPLES_DIR/.." && pwd)"

# Use firstc from PATH (must be from brew or same install layout)
FIRSTC="firstc"
if ! command -v firstc &>/dev/null; then
  echo "run-all-brew.sh: firstc not found. Install with:"
  echo "  brew tap akkeshavan/first"
  echo "  brew install --HEAD first-compiler"
  exit 1
fi

# Brew installs stdlib to PREFIX/lib/first and runtime to PREFIX/lib
BREW_PREFIX="$(brew --prefix first-compiler 2>/dev/null || brew --prefix 2>/dev/null)"
if [[ -z "$BREW_PREFIX" || ! -d "$BREW_PREFIX" ]]; then
  echo "run-all-brew.sh: could not get Homebrew prefix. Is brew in PATH?"
  exit 1
fi
if [[ ! -d "$BREW_PREFIX/lib/first" ]]; then
  echo "run-all-brew.sh: $BREW_PREFIX/lib/first not found. Reinstall: brew install --HEAD first-compiler"
  exit 1
fi

export FIRST_LIB_PATH="$BREW_PREFIX/lib/first"
export LIBRARY_PATH="$BREW_PREFIX/lib${LIBRARY_PATH:+:$LIBRARY_PATH}"
# Suppress macOS "nano zone abandoned" malloc warning (harmless but noisy)
export MallocNanoZone=0

echo "Using firstc: $(command -v firstc)"
if command -v fir &>/dev/null; then
  echo "Using fir: $(command -v fir)"
else
  echo "fir: not found (optional)"
fi
echo ""

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
  mkdir -p "$dir/build"
  echo "--- $name ---"
  # chapter-03 has local imports; run firstc from example dir so paths resolve
  if [[ "$name" == "chapter-03-modules" ]]; then
    if (cd "$dir" && "$FIRSTC" "src/main.first" -o "build/out" 2>&1); then
      "$dir/build/out" 2>&1 || true
      echo "[PASS] $name"
      PASSED+=("$name")
    else
      echo "[FAIL] $name (build failed)"
      FAILED+=("$name")
    fi
    continue
  fi
  # All others: run from repo root so firstc sees examples/ paths; Prelude from FIRST_LIB_PATH
  rel_main="examples/$name/$main"
  rel_out="examples/$name/build/out"
  if (cd "$REPO_ROOT" && "$FIRSTC" "$rel_main" -o "$rel_out" 2>&1); then
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
