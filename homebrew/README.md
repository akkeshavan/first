# Homebrew installer for First compiler

## What gets installed

- **firstc** → `HOMEBREW_PREFIX/bin/firstc`
- **Stdlib** (Prelude.first, etc.) → `HOMEBREW_PREFIX/lib/first/`
- **Runtime** (libfirst_runtime.a) → `HOMEBREW_PREFIX/lib/`
- **fir** (if present) → `HOMEBREW_PREFIX/bin/fir`

After install, `firstc` finds Prelude and the runtime via the “lib next to binary” paths, so it works from any directory.

## Install (users)

```bash
brew tap akkeshavan/first
brew install --HEAD first-compiler
```

> `--HEAD` is required until a stable release (e.g. `v0.1.0`) is tagged. See "Adding a stable release" below.

## One-time tap setup (maintainers)

To publish the Homebrew tap so others can install via `brew tap`:

1. **Create the tap locally** (if not already done):
   ```bash
   brew tap-new akkeshavan/first
   ```

2. **Copy the formula** into the tap:
   ```bash
   cp homebrew/first-compiler.rb $(brew --repository akkeshavan/first)/Formula/
   ```

3. **Push the tap to GitHub** (repo name must be `homebrew-first` for tap `akkeshavan/first`):
   ```bash
   brew install gh
   gh repo create akkeshavan/homebrew-first --push --public --source "$(brew --repository akkeshavan/first)"
   ```

4. **To update the formula** after changes:
   ```bash
   cp homebrew/first-compiler.rb $(brew --repository akkeshavan/first)/Formula/
   cd $(brew --repository akkeshavan/first) && git add Formula/first-compiler.rb && git commit -m "Update first-compiler" && git push
   ```

## Adding a stable release

When you have a release tag (e.g. `v0.1.0`):

1. In the formula, add (before `head`):

   ```ruby
   url "https://github.com/akkeshavan/first/archive/refs/tags/v#{version}.tar.gz"
   sha256 "<output of: curl -sL <that_url> | shasum -a 256>"
   ```

2. Then users can run `brew install first-compiler` (no `--HEAD`) and get the tagged version; `brew install --HEAD first-compiler` still gets latest main.

## Uninstall

```bash
brew uninstall first-compiler
```
