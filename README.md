# First Programming Language

First is a **functional-first programming language** that emphasizes pure functional programming while providing **controlled access to imperative features**. It features a strict distinction between pure functions and side-effecting interactions, strong typing with TypeScript-like expressiveness, and Haskell-style type classes.

## Key Features

- **Functional-first**: Primary programming paradigm emphasizing pure functions
- **Controlled imperative**: Imperative features (mutable state, loops) restricted to interaction functions
- **Strong typing**: Static type checking with comprehensive type inference
- **Advanced type system**: Supports refinement types, dependent types, union types, intersection types, and more
- **Haskell integration**: Seamless integration with Haskell libraries through automatic wrapper generation
- **Effect isolation**: Mutable state and loops are restricted to interaction functions only
- **Memory safety**: Guaranteed through the type system and effect restrictions

## Language Principles

- **Pure functions by default**: Functions are pure unless explicitly marked as interactions
- **Explicit side effects**: Side effects are contained within interaction functions
- **Immutable by default**: Variables are immutable unless explicitly marked as mutable
- **Effect isolation**: Mutable state and loops are restricted to interaction functions only

## Documentation

See the [Language Specification](./docs/First-Language-Specification.md) for complete documentation on the First programming language.

The grammar definition is available in [First.g4](./docs/First.g4) (ANTLR4 format).

## Project Status

This project is in active development. The language specification and grammar are defined, and the compiler implementation is underway.

## License

[To be determined]
