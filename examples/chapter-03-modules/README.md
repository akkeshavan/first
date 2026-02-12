# Chapter 3: Modules – import examples

This example shows four ways to import modules by location relative to `src/main.first`:

| # | Case | Module path | Import in main | File |
|---|------|-------------|----------------|------|
| 1 | **Same directory** as main | `src/` | `"./src/same_dir"` | `src/same_dir.first` |
| 2 | **Sibling directory** (sibling of `src/`) | `sibling/` | `"./sibling/sibling_mod"` | `sibling/sibling_mod.first` |
| 3 | **Child directory** (under `src/`) | `src/child/` | `"./src/child/child_mod"` | `src/child/child_mod.first` |
| 4 | **Parent directory** (project root) | project root | `"./parent"` | `parent.first` |

Paths are resolved relative to the **project root** when you run `fir build` (fir runs `firstc` from the project directory).

## Build and run

From this directory:

```bash
fir build
fir run
```

Or from the repo root:

```bash
./tools/fir build
./tools/fir run
```

From the examples directory, `./run-all-local.sh` also builds and runs this project.
