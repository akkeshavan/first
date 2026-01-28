# First Compiler - Backlog

**Last Updated:** January 27, 2026  
**Current Status:** Phase 5.13 (Module & Linking) - Core implementation complete, debugging segfault

This document tracks all remaining work, improvements, and future enhancements for the First compiler project.

---

## High Priority - Pattern Matching & ADT Improvements

### Pattern Matching Refinements
- [ ] **Constructor Index Lookup**: Map constructor names to their respective tag indices
  - Currently assumes constructor index is 0
  - Need to integrate with ADT type information from semantic analyzer
  - Location: `src/ir/ir_generator.cpp:1614`

- [ ] **Multi-Argument Constructor Destructuring**: Properly extract struct fields from ADT payload
  - Currently only handles single-argument constructors
  - Need to extract struct fields and bind to multiple argument patterns
  - Location: `src/ir/ir_generator.cpp:1691-1702`

- [ ] **Constructor Payload Storage**: Store constructor arguments in ADT payload instead of null pointer
  - Currently stores null pointer as placeholder
  - Need to allocate and store actual constructor argument values
  - Location: `src/ir/ir_generator.cpp:1387`

- [ ] **Record Pattern Matching**: Support pattern matching on record types
  - Extract field values and bind to field patterns
  - Match on specific field values

- [ ] **Array Pattern Matching**: Support pattern matching on array types
  - Match on array length
  - Destructure array elements into patterns

- [ ] **Exhaustiveness Checking**: Verify all ADT constructors are covered in match expressions
  - Currently reports error but doesn't prevent compilation
  - Should be done in semantic analysis phase
  - Location: `src/semantic/type_checker.cpp:843`

---

## Phase 5: LLVM IR Generation - Remaining Tasks

### Phase 5.8: Record Code Generation (Partially Complete)
- [x] Record type IR generation (struct types)
- [x] Record literal IR generation (stack allocation)
- [x] Field access IR generation (GEP + load)
- [ ] **Record Updates**: Implement IR generation for immutable record copies with modified fields
  - Create new record with some fields changed
  - Preserve immutability semantics

- [ ] **Record Type Parsing in Function Parameters**: Grammar/AST builder support for record type parameters
  - Support `(rec: {x: Int, y: Int})` style record type parameters
  - Location: `src/ast/builder.cpp:486`

### Phase 5.9: ADT Code Generation (Partially Complete)
- [x] ADT type IR generation (tagged unions)
- [x] Constructor call IR generation (basic)
- [x] Pattern matching IR generation (refined)
- [ ] **Complete Constructor Payload Storage**: Store actual constructor arguments
- [ ] **Complete Pattern Matching**: All pattern types fully supported
- [ ] **Constructor Index Mapping**: Map names to indices

### Phase 5.10: Pattern Matching Code Generation (Partially Complete)
- [x] Basic pattern matching IR (variable, wildcard, literal, constructor, as-patterns)
- [ ] **Nested Pattern Matching**: Support nested patterns in match expressions
- [ ] **Pattern Guard Support**: Support `when` clauses in match cases
- [ ] **Pattern Optimization**: Optimize pattern matching IR (jump tables, etc.)

### Phase 5.11: Closure Code Generation
- [ ] **Closure Representation**: Function pointer + captured environment
- [ ] **Closure Allocation**: Allocate closures on heap with captured variables
- [ ] **Closure Invocation**: Generate IR for closure calls
- [ ] **Capture Analysis**: Determine which variables need to be captured
- [ ] **Lambda Expression Support**: Parse and generate IR for lambda expressions
  - Location: `src/ast/builder.cpp:469`

### Phase 5.12: Generic Code Generation
- [ ] **Generic Function Monomorphization**: Generate specialized versions for each type instantiation
- [ ] **Type Parameter Substitution**: Replace type parameters with concrete types
- [ ] **Generic Type Instantiation**: Generate IR for instantiated generic types
- [ ] **Generic Type Parsing**: Support generic type syntax in AST builder
  - Location: `src/ast/builder.cpp:486, 501`

### Phase 5.13: Module & Linking
- [x] **Module-Level IR Generation**: Generate IR for entire modules
  - `IRGenerator::visitProgram()` sets module identifier from AST
  - Visits type declarations and imports
- [x] **Symbol Export/Import**: Handle exported and imported symbols
  - `FunctionDecl`, `InteractionDecl`, `TypeDecl` have `isExported` flag
  - Exported symbols use `ExternalLinkage`, others use `InternalLinkage`
  - `IRGenerator::visitImportDecl()` generates external declarations
  - `IRGenerator::generateExternalDeclaration()` creates external function declarations
  - `ModuleResolver::getExportedSymbols()` extracts exported symbols
  - `ModuleResolver::getFunction()` / `getInteraction()` retrieve declarations
- [x] **Linking Multiple Modules**: Link multiple `.first` files together
  - `Compiler::linkModules()` static method implemented using LLVM `Linker` API
  - Module IR cloning and ownership management
  - Infrastructure ready (full linking deferred to avoid recursion during compilation)
- [x] **Runtime Library Linking**: Link with runtime library
  - `Compiler::linkRuntimeLibrary()` implemented
  - Automatically searches for runtime library in common locations
  - Uses `llvm::parseIRFile()` to load bitcode
  - Gracefully handles missing runtime library (optional linking)
- [x] **Module Resolution Implementation**: Complete module resolver
  - `ModuleResolver::findModulePath()` searches file system for module files
  - Supports multiple path patterns (direct file, directory/module.first, dot-separated paths)
  - `ModuleResolver::loadModule()` reads, parses, and compiles module files
  - `ModuleResolver::resolveImport()` handles import resolution with circular dependency detection
  - Compiler instances stored in `ModuleCompilerStorage` to keep ASTs alive
  - Recursive module loading prevented via `compileFromStringNoModules()` with `resolveModules=false`
- [ ] **Fix segmentation fault**: Debug and fix segfault during test execution
  - Issue likely related to module loading or compiler storage
  - Tests compile successfully but crash at runtime

### Phase 5.14: Monadic Operators (Simplified)
- [ ] **Desugar `>>=`**: Convert to function calls
- [ ] **Desugar `>>`**: Convert to function calls
- [ ] **Desugar `<$>`**: Convert to function calls
- [ ] **Desugar `<*>`**: Convert to function calls
- [ ] **Do Notation Desugaring**: Convert do-notation to function calls
- [ ] **Monadic Operator Parsing**: Support in AST builder
  - Location: `src/ast/builder.cpp:329`

---

## Phase 6: Optimization

### Step 6.1: LLVM Optimization Passes
- [ ] Enable standard LLVM optimization passes
- [ ] Dead code elimination
- [ ] Constant folding
- [ ] Function inlining (for small pure functions)
- [ ] Loop optimizations
- [ ] Register allocation optimization

### Step 6.2: First-Specific Optimizations
- [ ] **Pure Function Optimization**: Leverage purity information for optimizations
- [ ] **Immutability Optimizations**: Optimize based on immutability guarantees
- [ ] **Tail Call Optimization**: Convert tail calls to jumps
- [ ] **Pattern Matching Optimization**: Optimize match expressions (jump tables, etc.)
- [ ] **Reference Counting Optimization**: Reduce unnecessary ref count operations

---

## Phase 7: Code Generation & Linking

### Step 7.1: Native Code Generation
- [ ] LLVM to native code compilation
- [ ] Target platform support (x86_64, ARM64)
- [ ] Object file generation
- [ ] Executable linking

### Step 7.2: Runtime Library Integration
- [ ] Compile runtime libraries to object files
- [ ] Link runtime with generated code
- [ ] Test complete program execution

### Step 7.3: Standard Library
- [ ] Implement core standard library functions
- [ ] Array operations (map, filter, reduce, etc.) - runtime integration
  - Location: `docs/STATUS.md:308`
- [ ] String operations
- [ ] Math functions
- [ ] I/O operations (enhanced)

---

## Phase 8: Testing & Validation

### Step 8.1: Additional Unit Tests
- [ ] Pattern matching IR generation tests (comprehensive)
- [ ] ADT IR generation tests (comprehensive)
- [ ] Record IR generation tests (comprehensive)
- [ ] Closure IR generation tests
- [ ] Generic code generation tests
- [ ] Module linking tests

### Step 8.2: Integration Tests
- [ ] Test compilation of language examples
- [ ] Test all language features end-to-end
- [ ] Test error cases comprehensively
- [ ] Test performance benchmarks

### Step 8.3: Language Compliance Tests
- [ ] Test all language features from spec
- [ ] Test semantic restrictions
- [ ] Test edge cases
- [ ] Test error messages quality

---

## Phase 9: Advanced Features

### Step 9.1: Refinement Type Runtime Checks
- [ ] Runtime predicate evaluation
- [ ] Refinement type validation
- [ ] Error reporting for failed refinements

### Step 9.2: Dependent Type Support (Limited)
- [ ] Compile-time constant evaluation
- [ ] Dependent type checking (simplified)
- [ ] Indexed type support

### Step 9.3: Concurrency Support
- [ ] Async/await implementation
- [ ] Channel implementation
- [ ] Task spawning
- [ ] Select statement
- [ ] Grammar support for async/await/select
  - Location: `docs/First.g4:530-534`

### Step 9.4: Garbage Collection (Alternative to RC)
- [ ] Implement mark-and-sweep GC (optional)
- [ ] Or integrate with Boehm GC
- [ ] Performance comparison with RC

### Step 9.5: Debugging Support
- [ ] Debug symbol generation
- [ ] Source-level debugging
- [ ] Stack traces
- [ ] Enhanced error messages with source locations

---

## Code Quality & Infrastructure Improvements

### AST Builder Improvements
- [ ] **Match Statement Support**: Parse `matchStmt` from grammar
  - Location: `src/ast/builder.cpp:567`
- [ ] **Constructor Call Parsing**: Support constructor calls in expressions
  - Location: `src/ast/builder.cpp:469`
- [ ] **Lambda Expression Parsing**: Support lambda syntax
  - Location: `src/ast/builder.cpp:469`
- [ ] **Conditional Expression Parsing**: Support ternary expressions
  - Location: `src/ast/builder.cpp:469`
- [ ] **Top-Level Declarations**: Build module-level declarations
  - Location: `src/ast/builder.cpp:26`

### Type Checker Improvements
- [ ] **Array Literal Type Inference**: Infer types for array literals
  - Location: `src/semantic/type_checker.cpp:235`
- [ ] **Record Construction Type Checking**: Type check record literals
  - Location: `src/semantic/type_checker.cpp:235`
- [ ] **Pattern Type Checking**: Implement comprehensive pattern type checking
  - Location: `src/semantic/type_checker.cpp:822`
- [ ] **ADT Constructor Lookup**: Proper constructor resolution
  - Location: `src/semantic/type_checker.cpp:794`
- [ ] **Type Name Formatting**: Better type error messages
  - Location: `src/semantic/type_checker.cpp:672`
- [ ] **Purity Tracking**: Properly track function/interaction purity
  - Location: `src/semantic/type_checker.cpp:451`

### Semantic Checker Improvements
- [ ] **While Loop Checking**: Check while loops in pure functions
  - Location: `src/semantic/semantic_checker.cpp:54, 101`
- [ ] **Assignment Checking**: Check assignments in pure functions
  - Location: `src/semantic/semantic_checker.cpp:55`
- [ ] **Monadic Operator Checking**: Check monadic operators in pure functions
  - Location: `src/semantic/semantic_checker.cpp:117`

### IR Generator Improvements
- [ ] **Type Declaration IR**: Generate IR for type declarations
  - Location: `src/ir/ir_generator.cpp:62`
- [ ] **Import Declaration IR**: Handle import declarations
  - Location: `src/ir/ir_generator.cpp:62`
- [ ] **Function Type IR**: Support function types in IR
  - Location: `src/ir/ir_generator.cpp:346`
- [ ] **Array Type Improvements**: Use proper struct types for arrays (future)
  - Location: `src/ir/ir_generator.cpp:376`
- [ ] **ADT Payload Union**: Compute union of all constructor argument types
  - Location: `src/ir/ir_generator.cpp:410`
- [ ] **Return Type Inference**: Determine return type from context for match expressions
  - Location: `src/ir/ir_generator.cpp:1491`
- [ ] **Value Representation Handling**: Better handling of primitives vs pointers
  - Location: `src/ir/ir_generator.cpp:1425`

### Runtime Improvements
- [ ] **Array Operations Integration**: Full runtime integration for map, filter, reduce
  - Location: `docs/STATUS.md:308`
- [ ] **Heap Allocation**: Support for large/dynamic arrays
  - Location: `docs/STATUS.md:309`
- [ ] **Reference-Counted Arrays**: Full RC support for arrays
  - Location: `docs/STATUS.md:310`

---

## Documentation & Developer Experience

### Documentation
- [ ] **API Documentation**: Generate API docs for compiler internals
- [ ] **Language Tutorial**: Create tutorial for First language
- [ ] **Compiler Internals Guide**: Document compiler architecture
- [ ] **Contributing Guide**: Guidelines for contributors
- [ ] **Performance Guide**: Optimization tips and best practices

### Developer Experience
- [ ] **Better Error Messages**: More helpful error messages with suggestions
- [ ] **Error Recovery**: Continue parsing after errors to find more issues
- [ ] **IDE Integration**: Language server protocol (LSP) support
- [ ] **Syntax Highlighting**: Editor plugins for syntax highlighting
- [ ] **Debugging Tools**: Better debugging support for generated code

---

## Testing & Quality Assurance

### Test Coverage
- [ ] **Increase Test Coverage**: Aim for >90% code coverage
- [ ] **Property-Based Testing**: Add property-based tests for type system
- [ ] **Fuzzing**: Fuzz testing for parser and type checker
- [ ] **Performance Tests**: Benchmark suite for compiler performance

### Code Quality
- [ ] **Static Analysis**: Integrate static analysis tools (clang-tidy, etc.)
- [ ] **Code Formatting**: Consistent code formatting (clang-format)
- [ ] **Memory Safety**: Address any memory safety issues
- [ ] **Thread Safety**: Ensure thread safety where needed

---

## Known Issues & Bugs

### Current Issues
1. **Parser Test Path Resolution**: ✅ Fixed - test now detects correct path automatically
2. **Assignment Statements**: ✅ Fixed - now generates correct IR
3. **Mutable Variables**: ✅ Fixed - `var` declarations work correctly
4. **Array Operations**: Partially complete - higher-level operations pending runtime integration

### Potential Issues
- [ ] **Constructor Index Lookup**: Currently hardcoded to 0
- [ ] **Pattern Matching Exhaustiveness**: Error reported but compilation continues
- [ ] **Module Resolution**: Stubbed implementation needs completion
- [ ] **Type Inference Edge Cases**: May have issues with complex type inference

---

## Future Enhancements (Nice to Have)

### Language Features
- [ ] **Type Inference Improvements**: Better inference for complex types
- [ ] **Type Aliases**: Support for type aliases
- [ ] **Type Casting**: Explicit type casting operations
- [ ] **String Interpolation**: Enhanced string formatting
- [ ] **Macros**: Compile-time macro system

### Compiler Features
- [ ] **Incremental Compilation**: Only recompile changed modules
- [ ] **Parallel Compilation**: Compile multiple modules in parallel
- [ ] **Caching**: Cache compilation results
- [ ] **Watch Mode**: Auto-recompile on file changes

### Tooling
- [ ] **Package Manager**: Package management for First libraries
- [ ] **Build System Integration**: Integration with build systems
- [ ] **Cross-Compilation**: Support for cross-compilation
- [ ] **WebAssembly Target**: Compile to WebAssembly

---

## Notes

- Items marked with ✅ are completed
- Items marked with [ ] are pending
- Priority is roughly: High Priority → Phase 5 → Phase 6 → Phase 7 → Phase 8 → Phase 9 → Improvements
- Some items may be moved between phases as implementation progresses
- This backlog should be reviewed and updated regularly
