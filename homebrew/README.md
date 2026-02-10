# Homebrew installer for First compiler

## What gets installed

- **firstc** → `HOMEBREW_PREFIX/bin/firstc`
- **Stdlib** (Prelude.first, etc.) → `HOMEBREW_PREFIX/lib/first/`
- **Runtime** (libfirst_runtime.a) → `HOMEBREW_PREFIX/lib/`
- **fir** (if present) → `HOMEBREW_PREFIX/bin/fir`

After install, `firstc` finds Prelude and the runtime via the “lib next to binary” paths, so it works from any directory.

## Install from local formula (no tap)

From the repo root:

```bash
brew install --build-from-source ./homebrew/first-compiler.rb
```

Or build from the latest Git (HEAD):

```bash
brew install --HEAD ./homebrew/first-compiler.rb
```

(With a local path, HEAD uses the current directory as source.)

## Install from a tap

1. **Create a tap repo** (one-time): e.g. GitHub repo `homebrew-first` (or `akkeshavan/first`). Inside it, add the formula as:

   ```
   homebrew-first/
   └── Formula/
       └── first-compiler.rb   # copy from this repo's homebrew/first-compiler.rb
   ```

2. **Users install:**

   ```bash
   brew tap akkeshavan/first    # or your-username/first
   brew install --HEAD first-compiler
   ```

   Without a stable `url` in the formula, `--HEAD` is required so Homebrew clones the repo.

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
