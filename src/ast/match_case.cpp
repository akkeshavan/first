#include "first/ast/match_case.h"
#include "first/ast/patterns.h"
#include "first/ast/expressions.h"

namespace first {
namespace ast {

MatchCase::MatchCase(std::unique_ptr<Pattern> pattern, 
                     std::unique_ptr<Expr> guard,
                     std::unique_ptr<Expr> body)
    : pattern_(std::move(pattern)), guard_(std::move(guard)), body_(std::move(body)) {}

Pattern* MatchCase::getPattern() const {
    return pattern_.get();
}

Expr* MatchCase::getGuard() const {
    return guard_.get();
}

Expr* MatchCase::getBody() const {
    return body_.get();
}

bool MatchCase::hasGuard() const {
    return guard_ != nullptr;
}

} // namespace ast
} // namespace first
