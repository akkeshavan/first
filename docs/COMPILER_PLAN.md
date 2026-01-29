# First Programming Language - LLVM Compiler Implementation Plan

## Overview

This document outlines a phased plan for implementing an LLVM-based compiler for the First programming language. The compiler will translate First source code into LLVM IR, which can then be compiled to native machine code.

## Architecture Overview

```
First Source (.first)
    ↓
[Phase 1] Lexical Analysis (ANTLR4)
    ↓
[Phase 2] Syntax Analysis (AST)
    ↓
[Phase 3] Semantic Analysis (Type Checking, Inference)
    ↓
[Phase 4] IR Generation (LLVM IR)
    ↓
[Phase 5] Optimization
    ↓
[Phase 6] Code Generation (Native Binary)
```

---

## Phase 1: Foundation & Lexical Analysis

**Goal**: Set up project structure and implement lexer/parser

### Step 1.1: Project Setup
**Verification**: Project builds and runs basic tests
- [ ] Initialize project structure (C++/Rust/Python with LLVM bindings)
- [ ] Set up build system (CMake/Meson/Cargo)
- [ ] Integrate ANTLR4 runtime
- [ ] Create basic test harness
- [ ] Set up CI/CD pipeline

**Deliverable**: `firstc` binary that accepts `--version` and `--help`

### Step 1.2: ANTLR4 Grammar Integration
**Verification**: Can parse all valid First syntax constructs
- [ ] Integrate First.g4 grammar file
- [ ] Generate lexer/parser from grammar
- [ ] Test tokenization of all keywords, operators, literals
- [ ] Test parsing of simple programs (hello world, arithmetic)
- [ ] Test parsing of complex constructs (nested functions, ADTs)

**Deliverable**: Parser that generates parse tree for valid First programs

### Step 1.3: Error Reporting Infrastructure
**Verification**: Clear, helpful error messages for syntax errors
- [ ] Implement source location tracking
- [ ] Create error message formatting system
- [ ] Test error reporting for common syntax errors
- [ ] Add support for multiple errors per file

**Deliverable**: Parser with comprehensive error reporting

---

## Phase 2: Abstract Syntax Tree (AST) Construction

**Goal**: Build a well-structured AST from parse tree

### Step 2.1: AST Node Definitions
**Verification**: AST can represent all First language constructs
- [ ] Define AST node base class/interface
- [ ] Implement nodes for:
  - Expressions (literals, operators, function calls, etc.)
  - Statements (declarations, assignments, control flow)
  - Types (primitives, arrays, records, ADTs, generics)
  - Functions and interactions
  - Modules and imports
- [ ] Add source location information to all nodes

**Deliverable**: Complete AST node hierarchy

### Step 2.2: Parse Tree to AST Conversion
**Verification**: All parse tree constructs convert correctly
- [ ] Implement visitor/transformer for parse tree
- [ ] Convert expressions preserving operator precedence
- [ ] Convert type declarations (including complex types)
- [ ] Convert function/interaction declarations
- [ ] Convert module system constructs
- [ ] Handle comments and whitespace appropriately

**Deliverable**: AST generator from parse tree

### Step 2.3: AST Validation
**Verification**: Invalid AST structures are caught early
- [ ] Validate AST structure (no null children, proper nesting)
- [ ] Check for obvious semantic issues (e.g., function called as type)
- [ ] Verify AST serialization/deserialization works

**Deliverable**: AST validator

---

## Phase 3: Semantic Analysis & Type System

**Goal**: Implement type checking, inference, and semantic validation

### Step 3.1: Symbol Table Infrastructure
**Verification**: Can track and resolve symbols correctly
- [ ] Implement scoped symbol table (block, function, module scopes)
- [ ] Support for function overloading
- [ ] Support for generic type parameters
- [ ] Module symbol resolution
- [ ] Import resolution

**Deliverable**: Symbol table with scoping support

### Step 3.2: Basic Type System
**Verification**: Type checking works for primitive types
- [ ] Implement type representation (Int, Float, Bool, String, Unit)
- [ ] Type equality and subtyping
- [ ] Type inference for simple expressions
- [ ] Type checking for variable declarations
- [ ] Type checking for function calls

**Deliverable**: Type checker for primitive types

### Step 3.3: Composite Types
**Verification**: Arrays, records, and basic generics work
- [ ] Array type checking and inference
- [ ] Record type checking (field access, construction)
- [ ] Generic type parameter handling
- [ ] Generic type instantiation
- [ ] Type checking for generic functions

**Deliverable**: Type checker for composite types

### Step 3.4: Function & Interaction Type System
**Verification**: Function types and purity tracking work
- [ ] Function type representation
- [ ] Interaction type representation
- [ ] Purity tracking (function vs interaction)
- [ ] Type checking for function calls
- [ ] Higher-order function support
- [ ] Closure capture analysis

**Deliverable**: Function type system with purity tracking

### Step 3.5: Algebraic Data Types & Pattern Matching
**Verification**: ADTs and pattern matching type-check correctly
- [ ] ADT type representation (sum types)
- [ ] Constructor type checking
- [ ] Pattern matching type checking
- [ ] Exhaustiveness checking for match expressions
- [ ] Pattern variable binding

**Deliverable**: ADT and pattern matching type checker

### Step 3.6: Advanced Type Features (Simplified)
**Verification**: Basic support for advanced types (runtime checks where needed)
- [ ] Union types (tagged unions at runtime)
- [ ] Intersection types (compile-time only)
- [ ] Refinement types (runtime predicate checks)
- [ ] Dependent types (simplified, compile-time constants only)
- [ ] Existential types (type erasure)

**Deliverable**: Advanced type system (with runtime checks)

### Step 3.7: Interface & Implementation System
**Verification**: Type classes/interfaces work
- [ ] Interface definition and constraint checking
- [ ] Implementation resolution
- [ ] Constraint propagation
- [ ] Interface method dispatch

**Deliverable**: Type class system

### Step 3.8: Semantic Restrictions Enforcement
**Verification**: Pure function restrictions are enforced
- [ ] Check for mutable variables in pure functions
- [ ] Check for while loops in pure functions
- [ ] Check for monadic operators in pure functions
- [ ] Check for I/O operations in pure functions
- [ ] Clear error messages for violations

**Deliverable**: Semantic restriction checker

### Step 3.9: Module System
**Verification**: Imports and exports work correctly
- [ ] Module resolution
- [ ] Export visibility checking
- [ ] Circular dependency detection
- [ ] Module-level type checking

**Deliverable**: Module system implementation

---

## Phase 4: Runtime System

**Goal**: Implement runtime support for First language features

### Step 4.1: Memory Management Runtime
**Verification**: Reference counting works correctly
- [ ] Design reference counting system
- [ ] Implement RC for all heap-allocated objects
- [ ] Implement cycle detection (optional, for Phase 5)
- [ ] Implement memory allocation/deallocation
- [ ] Test with simple programs

**Deliverable**: Reference counting runtime library

### Step 4.2: String Runtime
**Verification**: String operations work correctly
- [ ] UTF-8 string representation
- [ ] String concatenation, slicing
- [ ] String comparison
- [ ] String-to-number conversions

**Deliverable**: String runtime library

### Step 4.3: Array Runtime
**Verification**: Array operations work correctly
- [ ] Array allocation and deallocation
- [ ] Array indexing (bounds checking)
- [ ] Array length
- [ ] Array operations (map, filter, reduce)

**Deliverable**: Array runtime library

### Step 4.4: Record Runtime
**Verification**: Record operations work correctly
- [ ] Record allocation
- [ ] Field access
- [ ] Record copying (for immutability)

**Deliverable**: Record runtime library

### Step 4.5: ADT Runtime
**Verification**: ADTs work correctly
- [ ] Tagged union representation
- [ ] Constructor allocation
- [ ] Pattern matching runtime support
- [ ] Destructuring

**Deliverable**: ADT runtime library

### Step 4.6: Error Handling Runtime
**Verification**: Result and Option types work
- [ ] Result<T, E> representation
- [ ] Option<T> representation
- [ ] Error propagation
- [ ] Unwrap operations

**Deliverable**: Error handling runtime

### Step 4.7: I/O Runtime
**Verification**: Basic I/O operations work
- [ ] Print function
- [ ] Input function
- [ ] File read/write (basic)
- [ ] Error handling for I/O

**Deliverable**: I/O runtime library

---

## Phase 5: LLVM IR Generation

**Goal**: Generate LLVM IR from validated AST

### Step 5.1: LLVM Setup & Infrastructure
**Verification**: Can generate and compile simple LLVM IR
- [ ] Set up LLVM API bindings
- [ ] Create LLVM context and module
- [ ] Implement basic IR generation utilities
- [ ] Test with simple arithmetic program

**Deliverable**: LLVM IR generation infrastructure

### Step 5.2: Primitive Type Code Generation
**Verification**: Primitive types compile correctly
- [ ] Generate IR for Int (i64)
- [ ] Generate IR for Float (double)
- [ ] Generate IR for Bool (i1)
- [ ] Generate IR for Unit (void)
- [ ] Generate IR for String (runtime string type)

**Deliverable**: Primitive type IR generation

### Step 5.3: Expression Code Generation
**Verification**: All expression types generate correct IR
- [ ] Literal code generation
- [ ] Arithmetic operations (+, -, *, /, %)
- [ ] Comparison operations (<, <=, >, >=, ==, !=)
- [ ] Logical operations (&&, ||, !)
- [ ] Variable access
- [ ] Function calls
- [ ] Operator precedence handling

**Deliverable**: Expression IR generator

### Step 5.4: Control Flow Code Generation
**Verification**: Control flow generates correct IR
- [ ] If/else statements
- [ ] While loops
- [ ] Return statements
- [ ] Short-circuit evaluation for && and ||

**Deliverable**: Control flow IR generator

### Step 5.5: Function Code Generation
**Verification**: Functions compile to LLVM functions
- [ ] Function declaration IR generation
- [ ] Parameter handling
- [ ] Local variable allocation
- [ ] Return value handling
- [ ] Function call IR generation
- [ ] Recursive function support

**Deliverable**: Function IR generator

### Step 5.6: Interaction Code Generation
**Verification**: Interactions compile correctly
- [ ] Interaction function IR generation
- [ ] Mutable variable handling
- [ ] Assignment operations
- [ ] I/O operation calls
- [ ] Side effect tracking

**Deliverable**: Interaction IR generator

### Step 5.7: Array Code Generation
**Verification**: Arrays compile correctly
- [ ] Array allocation IR
- [ ] Array indexing IR
- [ ] Array bounds checking
- [ ] Array operations (map, filter, etc.)

**Deliverable**: Array IR generator

### Step 5.8: Record Code Generation
**Verification**: Records compile correctly
- [ ] Record type definition
- [ ] Record allocation
- [ ] Field access IR
- [ ] Record update (immutable copy)

**Deliverable**: Record IR generator

### Step 5.9: ADT Code Generation
**Verification**: ADTs compile correctly
- [ ] ADT type definition (tagged unions)
- [ ] Constructor IR generation
- [ ] Pattern matching IR generation
- [ ] Destructuring IR

**Deliverable**: ADT IR generator

### Step 5.10: Pattern Matching Code Generation
**Verification**: Pattern matching generates efficient IR
- [ ] Simple pattern matching (constructors, literals)
- [ ] Nested pattern matching
- [ ] Pattern variable binding
- [ ] Exhaustiveness checking (already done in Phase 3)

**Deliverable**: Pattern matching IR generator

### Step 5.11: Closure Code Generation
**Verification**: Closures work correctly
- [ ] Closure representation (function pointer + captured environment)
- [ ] Closure allocation
- [ ] Closure invocation
- [ ] Capture analysis and code generation

**Deliverable**: Closure IR generator

### Step 5.12: Generic Code Generation
**Verification**: Generics compile correctly
- [ ] Generic function monomorphization
- [ ] Type parameter substitution
- [ ] Generic type instantiation

**Deliverable**: Generic code generator

### Step 5.13: Module & Linking
**Verification**: Multiple modules link correctly
- [x] Module-level IR generation
  - `IRGenerator::visitProgram()` sets module identifier
  - External declarations generated for imported symbols
- [x] Symbol export/import
  - `FunctionDecl`, `InteractionDecl`, `TypeDecl` have `isExported` flag
  - Exported symbols use `ExternalLinkage`, others use `InternalLinkage`
  - `ModuleResolver::getExportedSymbols()` extracts exported symbols
  - `IRGenerator::visitImportDecl()` generates external declarations
- [x] Linking multiple modules
  - `Compiler::linkModules()` static method implemented using LLVM `Linker` API
  - Module IR cloning and ownership management in place
- [x] Runtime library linking
  - `Compiler::linkRuntimeLibrary()` implemented
  - Automatically searches for runtime library in common locations
  - Gracefully handles missing runtime library (optional)
- [x] Automatic module loading
  - `ModuleResolver::loadModule()` reads and compiles module files
  - `ModuleResolver::findModulePath()` searches file system for modules
  - Compiler instances stored to keep ASTs alive
  - Recursive loading prevented

**Deliverable**: Module linker

**Status**: Complete. Multi-module compile path works: main module and imported modules are type-checked, IR is generated for each, and LLVM linker merges them. `test_multimodule_end_to_end_linking` passes (Main.first imports Math.first, `square` is linked as a definition). Previously: segfault from `ModuleResolver::resolveImports()` clearing `importStack_` during recursive resolution—fixed by clearing the stack only at top-level (`clearImportStack()` before `resolveImports()`).

### Step 5.14: Monadic Operators (Simplified)
**Verification**: Monadic operators compile (desugared)
- [x] Desugar `>>=` to function calls
- [x] Desugar `>>` to function calls
- [x] Desugar `<$>` to function calls
- [x] Desugar `<*>` to function calls
- [ ] Do notation desugaring

**Deliverable**: Monadic operator desugaring

---

**Current focus**: Phase 6 → 7 → 8; remaining Phase 9 steps (full async/await, channel, task, select runtime + IR) after that.

---

## Phase 6: Optimization

**Goal**: Optimize generated LLVM IR

### Step 6.1: LLVM Optimization Passes
**Verification**: Optimizations improve code quality
- [x] Enable standard LLVM optimization passes (O2 pipeline via PassBuilder after IR + linking)
- [x] Dead code elimination (included in O2 pipeline)
- [x] Constant folding (included in O2 pipeline)
- [x] Inlining (for small pure functions) (included in O2 pipeline)
- [x] Loop optimizations (included in O2 pipeline)
- [x] Register allocation optimization (done at codegen; O2 improves SSA)

**Deliverable**: Optimized IR generation

### Step 6.2: First-Specific Optimizations
**Verification**: Language-specific optimizations work
- [x] Pure function optimization (no side effects): FunctionDecl gets ReadOnly; enables CSE/inlining
- [x] Immutability optimizations: pointer parameters of pure functions get ReadOnly attribute
- [x] Tail call optimization: return f(...) emitted as tail call when return expr is direct FunctionCallExpr
- [x] Pattern matching optimization: match lowered to branches; O2 optimizes (switch/merge); no custom pass yet
- [x] Reference counting optimization: no explicit retain/release in IR yet; when added, O2 + future pass can eliminate redundant pairs

**Deliverable**: First-specific optimizer

---

## Phase 7: Code Generation & Linking

**Goal**: Generate native executables

### Step 7.1: Native Code Generation
**Verification**: Can generate native binaries
- [x] LLVM to native code compilation (writeObjectToFile via TargetMachine + LegacyPassManager)
- [x] Target platform support (x86_64, ARM64 via native target; X86/AArch64 components linked)
- [x] Object file generation
- [x] Executable linking (linkToExecutable invokes clang++ to link .o + libfirst_runtime)

**Deliverable**: Native code generator

### Step 7.2: Runtime Library Integration
**Verification**: Runtime libraries link correctly
- [x] Compile runtime libraries to object files (first_runtime built as static lib)
- [x] Link runtime with generated code (-L runtime/ or -L ../runtime, -lfirst_runtime)
- [x] Test complete program execution (firstc -o exe file.first produces runnable exe)

**Deliverable**: Integrated runtime system

### Step 7.3: Standard Library
Standard libraries required for linux and MACOS (apple silicon) distribution.
**Verification**: Standard library functions work
- [x] Core I/O: `print(s)`, `println(s)`, `readLine()`, `readFile(path)`, `writeFile(path, content)` — C linkage in runtime (io.cpp + stdlib.cpp), type checker + IR built-ins
- [x] Array operations: `arrayLength(arr)` — compiler intrinsic (metadata) or `first_array_length`; map/filter/reduce require closure ABI (runtime C++ has FirstArray::map/filter/reduce)
- [x] String operations: `stringLength`, `stringConcat`, `stringSlice`, `stringToInt`, `stringToFloat`, `intToString`, `floatToString` — C linkage in stdlib.cpp
- [x] Math functions: `sin`, `cos`, `sqrt`, `abs`, `floor`, `ceil`, `min`, `max`, `minInt`, `maxInt` — C linkage (first_*), libm
- [x] I/O operations: print, println, readLine, readFile, writeFile
- [x] Socket library: `socketConnect(host, port)`, `socketSend(fd, str)`, `socketRecv(fd)`, `socketClose(fd)` — C linkage (POSIX)
- [x] HTTP lib: `httpGet(url)`, `httpPost(url, body)` — C linkage (stub; full impl via socket)
- [x] JSON: `jsonPrettify(str)`, `jsonStringifyInt`, `jsonStringifyFloat`, `jsonStringifyString` — C linkage (prettify + stringify)

**Deliverable**: Standard library implementation (runtime stdlib.h/stdlib.cpp; type checker inferStdlibCall; IR getStdlibSig + arrayLength intrinsic)

---

## Phase 8: Testing & Validation

**Goal**: Ensure compiler correctness and completeness

### Step 8.1: Unit Tests
**Verification**: All components have unit tests
- [ ] Lexer/parser tests
- [ ] AST construction tests
- [ ] Type checker tests
- [ ] IR generation tests
- [ ] Runtime tests

**Deliverable**: Comprehensive test suite

### Step 8.2: Integration Tests
**Verification**: End-to-end compilation works
- [ ] Test compilation of language examples
- [ ] Test all language features
- [ ] Test error cases
- [ ] Test performance benchmarks

**Deliverable**: Integration test suite

### Step 8.3: Language Compliance Tests
**Verification**: Compiler matches language specification
- [ ] Test all language features from spec
- [ ] Test semantic restrictions
- [ ] Test edge cases
- [ ] Test error messages

**Deliverable**: Compliance test suite

---

## Phase 9: Advanced Features (Future)

**Goal**: Implement advanced language features

### Step 9.1: Refinement Type Runtime Checks
- [x] Runtime predicate evaluation (IR emits predicate eval and branch to trap on failure)
- [x] Refinement type validation (type checker unwraps to base; IR uses base type and checks at entry)
- [x] Error reporting for failed refinements (runtime __first_refinement_fail prints message to stderr and aborts)

### Step 9.2: Dependent Type Support (Limited)
- [ ] Compile-time constant evaluation (optional; indices can be literals or identifiers)
- [x] Dependent type checking (simplified): IndexedType equality/assignability by base type + index expr equality
- [x] Indexed type support: BaseType[indexList] parsed, IndexedType AST, type-checked, IR lowers to base type
- [x] Dependent function (Pi): (x: T) -> R parsed, type-checked, IR lowers to function pointer
- [x] Dependent pair (Sigma): (x: T) * B parsed, type-checked, IR lowers to struct { T; B }
- [x] Forall type: forall T U. Type parsed, type-checked, IR lowers to body type (instantiation at use)
- [x] Existential type: exists x: T. Body parsed, type-checked, IR lowers to struct { T; Body }

### Step 9.3: Concurrency Support
- [x] Async/await: parsing, AST (AsyncExpr, AwaitExpr), type-checker and IR stubs (full Promise runtime + IR lowering pending)
- [x] Channel: select receive/send syntax parsed; type/IR stubs (Channel type + runtime pending)
- [x] Task spawning: parsing, AST (SpawnExpr, JoinExpr), type-checker and IR stubs (full Task runtime + IR pending)
- [x] Select statement: parsing, AST (SelectStmt, SelectExpr, SelectBranch), type-checker and IR stubs (full multi-branch select IR pending)

### Step 9.4: Garbage Collection (Alternative to RC)
- [ ] Implement mark-and-sweep GC
- [ ] Or integrate with Boehm GC
- [ ] Performance comparison with RC

### Step 9.5: Debugging Support
- [ ] Debug symbol generation
- [ ] Source-level debugging
- [ ] Stack traces
- [ ] Error messages with source locations

---

## Implementation Notes

### Technology Choices

**Recommended Stack:**
- **Language**: C++ (for LLVM integration) or Rust (for safety)
- **Parser**: ANTLR4 (grammar already defined)
- **LLVM**: LLVM 15+ API
- **Build System**: CMake (C++) or Cargo (Rust)

### Key Design Decisions

1. **Memory Management**: Start with reference counting, add cycle detection later
2. **Advanced Types**: Use runtime checks for refinement types initially
3. **Dependent Types**: Simplified support (compile-time constants only)
4. **Monadic Operators**: Desugar to function calls rather than custom runtime
5. **Pure Functions**: Track at compile-time, optimize at IR level

### Verification Strategy

Each step should have:
- **Unit tests** for the specific feature
- **Integration tests** showing it works with other features
- **Example programs** demonstrating the feature
- **Error case tests** showing proper error handling

### Success Criteria

Phase completion criteria:
- All tests pass
- Example programs compile and run correctly
- Error messages are clear and helpful
- Performance is reasonable (within 2x of C for simple programs)

---

## Timeline Estimate

- **Phase 1**: 2-3 weeks
- **Phase 2**: 2-3 weeks
- **Phase 3**: 6-8 weeks (largest phase)
- **Phase 4**: 4-5 weeks
- **Phase 5**: 8-10 weeks (complex)
- **Phase 6**: 2-3 weeks
- **Phase 7**: 2-3 weeks
- **Phase 8**: 3-4 weeks
- **Phase 9**: Ongoing

**Total**: ~30-40 weeks for a basic working compiler

---

## Risk Mitigation

1. **Complex Type System**: Start with simplified versions, add complexity incrementally
2. **Performance**: Profile early, optimize hotspots
3. **LLVM API Changes**: Pin LLVM version, test upgrades carefully
4. **Memory Management**: Start simple (RC), optimize later
5. **Error Messages**: Invest in good error reporting from the start

---

## Resources

- LLVM Documentation: https://llvm.org/docs/
- ANTLR4 Documentation: https://www.antlr.org/
- First Language Specification: `First-Language-Specification.md`
- First Grammar: `First.g4`
