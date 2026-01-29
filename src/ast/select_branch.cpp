#include "first/ast/expressions.h"
#include "first/ast/types.h"
#include "first/ast/statements.h"

namespace first {
namespace ast {

SelectBranch::SelectBranch(SourceLocation location, Kind kind,
                           std::unique_ptr<Expr> channelExpr,
                           std::string varName,
                           std::unique_ptr<Expr> valueExpr,
                           std::unique_ptr<Stmt> statement)
    : location_(location), kind_(kind), channelExpr_(std::move(channelExpr)),
      varName_(std::move(varName)), valueExpr_(std::move(valueExpr)),
      statement_(std::move(statement)) {}

SelectBranch::~SelectBranch() = default;

} // namespace ast
} // namespace first
