#!/usr/bin/env bash
# Install First compiler and runtime globally from a release TGZ.
# Usage:
#   ./install.sh [path-to-.tar.gz] [install-prefix]
# Default: use first-compiler-*-Darwin-*.tar.gz in dist/ or current dir, prefix /usr/local
# After install, firstc will be in <prefix>/bin; PATH is updated in your shell profile if needed.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

TGZ="${1:-}"
PREFIX="${2:-/usr/local}"

if [[ -z "$TGZ" ]]; then
  for d in "${PROJECT_DIR}/dist" "${PROJECT_DIR}" .; do
    if compgen -G "${d}/first-compiler-"*"-Darwin-"*".tar.gz" >/dev/null 2>&1; then
      TGZ="$(ls -t "${d}"/first-compiler-*-Darwin-*.tar.gz 2>/dev/null | head -1)"
      break
    fi
  done
fi

if [[ -z "$TGZ" || ! -f "$TGZ" ]]; then
  echo "Usage: $0 [path-to-first-compiler-*.tar.gz] [install-prefix]"
  echo "Default prefix: /usr/local"
  echo "No first-compiler-*-Darwin-*.tar.gz found in dist/ or current dir."
  exit 1
fi

echo "Installing from: $TGZ"
echo "Prefix: $PREFIX"
if [[ ! -w "$PREFIX" ]] && [[ -d "$PREFIX" ]]; then
  echo "No write access to $PREFIX. Run with: sudo $0 $TGZ $PREFIX"
  exit 1
fi
echo "This will install firstc and the runtime to $PREFIX."
read -r -p "Continue? [y/N] " reply
case "$reply" in
  [yY]|[yY][eE][sS]) ;;
  *) exit 0 ;;
esac

# TGZ layout is usr/local/bin, usr/local/lib, usr/local/include (from CPack)
# Extract to a temp dir then copy into PREFIX
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
tar -xzf "$TGZ" -C "$tmp"

# TGZ has one top-level dir (e.g. first-compiler-0.1.0-Darwin-arm64) with bin/, lib/, include/
top="$(ls "$tmp")"
src="$tmp/$top"
for dir in bin lib include; do
  if [[ -d "$src/$dir" ]]; then
    mkdir -p "$PREFIX/$dir"
    cp -R "$src/$dir"/* "$PREFIX/$dir/" 2>/dev/null || cp -R "$src/$dir"/* "$PREFIX/$dir"
  fi
done

echo "Installed firstc to $PREFIX/bin/firstc"

# Ensure PREFIX/bin is in PATH for the user's shell
add_path_line() {
  local file="$1"
  local line="$2"
  if [[ -f "$file" ]]; then
    if grep -qF "$line" "$file" 2>/dev/null; then
      return
    fi
  else
    touch "$file"
  fi
  echo "$line" >> "$file"
  echo "Added to PATH in $file"
}

BIN_PATH="$PREFIX/bin"
if [[ ":$PATH:" != *":$BIN_PATH:"* ]]; then
  if [[ -n "$HOME" ]]; then
    if [[ -n "${ZSH_VERSION:-}" ]] || [[ -f "$HOME/.zshrc" ]]; then
      add_path_line "$HOME/.zshrc" "export PATH=\"$BIN_PATH:\$PATH\""
    fi
    if [[ -n "${BASH_VERSION:-}" ]] || [[ -f "$HOME/.bash_profile" ]]; then
      add_path_line "$HOME/.bash_profile" "export PATH=\"$BIN_PATH:\$PATH\""
    fi
  fi
  echo "Run: export PATH=\"$BIN_PATH:\$PATH\" (or open a new terminal)"
else
  echo "PATH already includes $BIN_PATH"
fi

echo "Done. Run 'firstc --help' to verify."
