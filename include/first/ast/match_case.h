#pragma once

#include <memory>

namespace first {
namespace ast {

// Forward declarations
class Pattern;
class Expr;

// Match case: pattern => expression/statement
class MatchCase {
public:
    MatchCase(std::unique_ptr<Pattern> pattern, std::unique_ptr<Expr> body);
    
    Pattern* getPattern() const;
    Expr* getBody() const;

private:
    std::unique_ptr<Pattern> pattern_;
    std::unique_ptr<Expr> body_;
};

} // namespace ast
} // namespace first
