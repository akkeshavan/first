#include "first/ast/expressions.h"
#include "first/ast/match_case.h"
#include "first/ast/patterns.h"

namespace first {
namespace ast {

MatchExpr::MatchExpr(const SourceLocation& location,
                     std::unique_ptr<Expr> matchedExpr,
                     std::vector<std::unique_ptr<MatchCase>> cases)
    : Expr(location), matchedExpr_(std::move(matchedExpr)), cases_(std::move(cases)) {}

Expr* MatchExpr::getMatchedExpr() const {
    return matchedExpr_.get();
}

const std::vector<std::unique_ptr<MatchCase>>& MatchExpr::getCases() const {
    return cases_;
}

} // namespace ast
} // namespace first
