#include "first/ast/expressions.h"
#include "first/ast/declarations.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"

namespace first {
namespace ast {

LambdaExpr::LambdaExpr(const SourceLocation& location,
                       std::vector<std::unique_ptr<Parameter>> parameters,
                       std::unique_ptr<Type> returnType,
                       std::vector<std::unique_ptr<Stmt>> body)
    : Expr(location), parameters_(std::move(parameters)), 
      returnType_(std::move(returnType)), body_(std::move(body)) {}

} // namespace ast
} // namespace first
