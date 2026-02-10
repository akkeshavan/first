# Backlog: Higher-kinded types (Option B: explicit kind annotation)

**Status:** Implemented (v1: kind `* -> *` only, explicit annotation `F : * -> *`).

## Summary

Support higher-kinded type parameters so interfaces can be parameterized by type constructors (e.g. `Functor<F : * -> *>` with `implementation Functor<Option>`).

## Syntax (Option B)

- **Kind annotation:** In a generic parameter list, use `F : * -> *` to declare that `F` is a type constructor (kind `* -> *`).
- **Example:** `interface Functor<F : * -> *> { map: forall A B. function(function(A) -> B, F<A>) -> F<B>; }`
- **Implementation:** `implementation Functor<List> { ... }` — pass the type constructor by name (`List`, not `List<Int>`).

## Implemented

- **AST:** `GenericParamKind` enum (Star, StarArrowStar); `GenericParam` has `kind`; `InterfaceDecl` uses `vector<GenericParam>`.
- **Grammar:** `genericParam: IDENTIFIER (COLON (IDENTIFIER | kindAnnotation))?` with `kindAnnotation: MUL ARROW MUL`.
- **Parser:** Hand-written parser and ANTLR builder both parse `F : * -> *` and set `kind = StarArrowStar`.
- **Type checker:** `substituteType` substitutes HKT param (e.g. `F`) with type constructor ref (`GenericType("List")`), so `F<A>` → `List<A>`. `typeImplementsInterface` and `getImplementationMemberFunctionName` match by type constructor name for HKT interfaces (e.g. `List<Int>` implements `Functor` when `implementation Functor<List>` exists). Helpers: `getTypeConstructorName`, `findInterface`.
- **Prelude:** `interface Functor<F : * -> *>` with member `map` (forall A B. ...).
- **Example:** `examples/chapter-14-HKT` with `implementation Functor<Option> { }`.

## Scope (v1)

- Only kind `* -> *`. No `(* -> *) -> *` or higher.
- Type constructor argument to an implementation is given by name (e.g. `List`, `Option`), represented as `GenericType("List")` in the AST.

## Possible follow-ups

- Require implementation to provide all interface members (e.g. `map`) and type-check their types against the substituted signature.
- Support constraints on HKT params (e.g. `F : Functor` in another interface).
