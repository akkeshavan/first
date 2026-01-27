#include "first/ast/types.h"
#include "first/ast/visitor.h"

namespace first {
namespace ast {

void PrimitiveType::accept(ASTVisitor& visitor) {
    // Types don't need visitor methods yet, but we need to implement the pure virtual
    // This will be extended when we add type visitor methods
    (void)visitor; // Suppress unused parameter warning
}

void ArrayType::accept(ASTVisitor& visitor) {
    // Types don't need visitor methods yet
    (void)visitor; // Suppress unused parameter warning
}

} // namespace ast
} // namespace first
