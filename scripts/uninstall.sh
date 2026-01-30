#!/usr/bin/env bash
# Remove First compiler and runtime from a prefix (default /usr/local).
# Usage: ./scripts/uninstall.sh [prefix]
# For /usr/local you may need: sudo ./scripts/uninstall.sh /usr/local

set -e
PREFIX="${1:-/usr/local}"

echo "Uninstalling First compiler from $PREFIX"
echo "This will remove:"
echo "  $PREFIX/bin/firstc"
echo "  $PREFIX/lib/libfirst_runtime.a"
echo "  $PREFIX/include/first/"
read -r -p "Continue? [y/N] " reply
case "$reply" in
  [yY]|[yY][eE][sS]) ;;
  *) exit 0 ;;
esac

rm -f "$PREFIX/bin/firstc"
rm -f "$PREFIX/lib/libfirst_runtime.a"
rm -rf "$PREFIX/include/first"
echo "Done. First compiler and runtime removed from $PREFIX"

# If firstc still appears on PATH, it's another install (e.g. Haskell First compiler)
if command -v firstc >/dev/null 2>&1; then
  echo ""
  echo "Note: 'firstc' is still available at: $(command -v firstc)"
  echo "That is a different install (e.g. Haskell First compiler), not this one."
  echo "To remove it, delete that file or adjust your PATH."
  echo "Open a new terminal so your shell picks up the change."
fi
echo "You can remove the PATH line from ~/.zshrc or ~/.bash_profile if you added it via install.sh."
