#pragma once

#include <memory>

namespace first {
namespace ast {

// Forward declarations
class Pattern;
class Expr;

// Match case: pattern (when guard)? => expression/statement
class MatchCase {
public:
    MatchCase(std::unique_ptr<Pattern> pattern, 
              std::unique_ptr<Expr> guard,
              std::unique_ptr<Expr> body);
    
    Pattern* getPattern() const;
    Expr* getGuard() const;  // nullptr if no guard
    Expr* getBody() const;
    bool hasGuard() const;

private:
    std::unique_ptr<Pattern> pattern_;
    std::unique_ptr<Expr> guard_;  // Optional guard expression
    std::unique_ptr<Expr> body_;
};

} // namespace ast
} // namespace first
