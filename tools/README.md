# fir – First project manager

**fir** is the project manager for First (like npm for Node, cargo for Rust). It creates new projects and gives you simple commands to build and run.

## Setup

Use **fir** from this repo (e.g. after building the compiler), or install it next to `firstc` when you install the First compiler (e.g. copy `tools/fir` to the same prefix as `firstc`).

From the repo:
```bash
export PATH="/path/to/first/tools:$PATH"
# or: ln -s /path/to/first/tools/fir /usr/local/bin/fir
```

## Commands

| Command | Description |
|--------|-------------|
| `fir init [dirname]` | Create a new First project. Default dir: `first-app`. |
| `fir build` | Compile the project (uses `fir.json` and `main` entry). |
| `fir run` | Build if needed, then run the executable. |

## Quick start

```bash
fir init my-app
cd my-app
fir build
fir run
```

## Project layout (after `fir init`)

```
my-app/
├── fir.json          # name, main entry point
├── src/
│   └── main.first    # entry point
├── build/            # generated (fir build)
│   └── my-app        # executable
└── .gitignore
```

## fir.json

- **name** – Project name (used for the executable).
- **main** – Entry point file (e.g. `src/main.first`).

The runtime (stdlib, libfirst_runtime.a) is **not** copied into the project; `fir build` uses the installed First compiler and runtime (from PATH and `LIBRARY_PATH`). Install firstc via Homebrew, the .pkg, or build from source.
