#include "first/ast/types.h"
#include "first/ast/expressions.h"
#include "first/ast/visitor.h"

namespace first {
namespace ast {

RefinementType::RefinementType(const SourceLocation& location,
                               const std::string& variableName,
                               std::unique_ptr<Type> baseType,
                               std::shared_ptr<Expr> predicate)
    : Type(location), variableName_(variableName),
      baseType_(std::move(baseType)), predicate_(std::move(predicate)) {}

RefinementType::~RefinementType() = default;

IndexedType::IndexedType(const SourceLocation& location,
                         std::unique_ptr<Type> baseType,
                         std::vector<std::shared_ptr<Expr>> indices)
    : Type(location), baseType_(std::move(baseType)), indices_(std::move(indices)) {}

IndexedType::~IndexedType() = default;

void IndexedType::accept(ASTVisitor& visitor) {
    (void)visitor;
}

DependentFunctionType::DependentFunctionType(const SourceLocation& location,
                                             const std::string& paramName,
                                             std::unique_ptr<Type> paramType,
                                             std::unique_ptr<Type> returnType)
    : Type(location), paramName_(paramName), paramType_(std::move(paramType)),
      returnType_(std::move(returnType)) {}

DependentFunctionType::~DependentFunctionType() = default;

void DependentFunctionType::accept(ASTVisitor& visitor) {
    (void)visitor;
}

DependentPairType::DependentPairType(const SourceLocation& location,
                                     const std::string& varName,
                                     std::unique_ptr<Type> varType,
                                     std::unique_ptr<Type> bodyType)
    : Type(location), varName_(varName), varType_(std::move(varType)),
      bodyType_(std::move(bodyType)) {}

DependentPairType::~DependentPairType() = default;

void DependentPairType::accept(ASTVisitor& visitor) {
    (void)visitor;
}

ForallType::ForallType(const SourceLocation& location,
                       std::vector<std::string> typeVars,
                       std::unique_ptr<Type> bodyType)
    : Type(location), typeVars_(std::move(typeVars)), bodyType_(std::move(bodyType)) {}

ForallType::~ForallType() = default;

void ForallType::accept(ASTVisitor& visitor) {
    (void)visitor;
}

ExistentialType::ExistentialType(const SourceLocation& location,
                                 const std::string& varName,
                                 std::unique_ptr<Type> varType,
                                 std::unique_ptr<Type> bodyType)
    : Type(location), varName_(varName), varType_(std::move(varType)),
      bodyType_(std::move(bodyType)) {}

ExistentialType::~ExistentialType() = default;

void ExistentialType::accept(ASTVisitor& visitor) {
    (void)visitor;
}

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
