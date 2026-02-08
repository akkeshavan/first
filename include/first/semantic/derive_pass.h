#pragma once

namespace first {
namespace ast {
    class Program;
}
namespace semantic {

// Generates implementations for types with #derive(ToString, Eq, Ord).
// For ToString on record types: emits a helper function and ImplementationDecl.
// Modifies the program in place (adds functions and implementations).
void runDerivePass(ast::Program* program);

} // namespace semantic
} // namespace first
