#pragma once

#include "first/ast/node.h"
#include "first/ast/visitor.h"
#include "first/ast/types.h"
#include <string>
#include <vector>
#include <memory>

namespace first {
namespace ast {

// Forward declarations
class Expr;
class LiteralExpr;

// Base class for patterns
class Pattern : public ASTNode {
public:
    Pattern(const SourceLocation& location) : ASTNode(location) {}
    virtual ~Pattern() = default;
};

// Variable pattern (binds to identifier)
class VariablePattern : public Pattern {
public:
    VariablePattern(const SourceLocation& location, const std::string& name)
        : Pattern(location), name_(name) {}
    
    const std::string& getName() const { return name_; }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "VariablePattern"; }

private:
    std::string name_;
};

// Literal pattern (matches literal value)
class LiteralPattern : public Pattern {
public:
    LiteralPattern(const SourceLocation& location, std::unique_ptr<LiteralExpr> literal)
        : Pattern(location), literal_(std::move(literal)) {}
    
    LiteralExpr* getLiteral() const { return literal_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "LiteralPattern"; }

private:
    std::unique_ptr<LiteralExpr> literal_;
};

// Constructor pattern (matches ADT constructor)
class ConstructorPattern : public Pattern {
public:
    ConstructorPattern(const SourceLocation& location,
                       const std::string& constructorName,
                       std::vector<std::unique_ptr<Pattern>> arguments)
        : Pattern(location), constructorName_(constructorName), arguments_(std::move(arguments)) {}
    
    const std::string& getConstructorName() const { return constructorName_; }
    const std::vector<std::unique_ptr<Pattern>>& getArguments() const { return arguments_; }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "ConstructorPattern"; }

private:
    std::string constructorName_;
    std::vector<std::unique_ptr<Pattern>> arguments_;
};

// Wildcard pattern (matches anything)
class WildcardPattern : public Pattern {
public:
    WildcardPattern(const SourceLocation& location)
        : Pattern(location) {}
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "WildcardPattern"; }
};

// As pattern (pattern @ identifier)
class AsPattern : public Pattern {
public:
    AsPattern(const SourceLocation& location,
              std::unique_ptr<Pattern> pattern,
              const std::string& name)
        : Pattern(location), pattern_(std::move(pattern)), name_(name) {}
    
    Pattern* getPattern() const { return pattern_.get(); }
    const std::string& getName() const { return name_; }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "AsPattern"; }

private:
    std::unique_ptr<Pattern> pattern_;
    std::string name_;
};

} // namespace ast
} // namespace first
