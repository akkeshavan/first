#pragma once

#include "first/ast/node.h"
#include "first/ast/visitor.h"
#include "first/ast/expressions.h"
#include <string>
#include <vector>
#include <memory>

namespace first {
namespace ast {

// Forward declarations
class Type;

// Base class for statements
class Stmt : public ASTNode {
public:
    Stmt(const SourceLocation& location) : ASTNode(location) {}
};

// Variable declaration statement
class VariableDecl : public Stmt {
public:
    enum class Mutability {
        Immutable,  // let
        Mutable     // var
    };

    VariableDecl(const SourceLocation& location, 
                 const std::string& name,
                 Mutability mutability,
                 std::unique_ptr<Type> type,
                 std::unique_ptr<Expr> initializer)
        : Stmt(location), name_(name), mutability_(mutability),
          type_(std::move(type)), initializer_(std::move(initializer)) {}

    const std::string& getName() const { return name_; }
    Mutability getMutability() const { return mutability_; }
    Type* getType() const { return type_.get(); }
    Expr* getInitializer() const { return initializer_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitVariableDecl(this);
    }
    
    std::string getNodeType() const override { return "VariableDecl"; }

private:
    std::string name_;
    Mutability mutability_;
    std::unique_ptr<Type> type_;
    std::unique_ptr<Expr> initializer_;
};

// Return statement
class ReturnStmt : public Stmt {
public:
    ReturnStmt(const SourceLocation& location, std::unique_ptr<Expr> value)
        : Stmt(location), value_(std::move(value)) {}

    Expr* getValue() const { return value_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitReturnStmt(this);
    }
    
    std::string getNodeType() const override { return "ReturnStmt"; }

private:
    std::unique_ptr<Expr> value_;
};

// Expression statement (expression used as statement)
class ExprStmt : public Stmt {
public:
    ExprStmt(const SourceLocation& location, std::unique_ptr<Expr> expr)
        : Stmt(location), expr_(std::move(expr)) {}

    Expr* getExpr() const { return expr_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitExprStmt(this);
    }
    
    std::string getNodeType() const override { return "ExprStmt"; }

private:
    std::unique_ptr<Expr> expr_;
};

// If statement
class IfStmt : public Stmt {
public:
    IfStmt(const SourceLocation& location,
           std::unique_ptr<Expr> condition,
           std::unique_ptr<Stmt> thenBranch,
           std::unique_ptr<Stmt> elseBranch)
        : Stmt(location), condition_(std::move(condition)),
          thenBranch_(std::move(thenBranch)), elseBranch_(std::move(elseBranch)) {}

    Expr* getCondition() const { return condition_.get(); }
    Stmt* getThenBranch() const { return thenBranch_.get(); }
    Stmt* getElseBranch() const { return elseBranch_.get(); }
    bool hasElse() const { return elseBranch_ != nullptr; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitIfStmt(this);
    }
    
    std::string getNodeType() const override { return "IfStmt"; }

private:
    std::unique_ptr<Expr> condition_;
    std::unique_ptr<Stmt> thenBranch_;
    std::unique_ptr<Stmt> elseBranch_;
};

// Assignment statement
class AssignmentStmt : public Stmt {
public:
    AssignmentStmt(const SourceLocation& location,
                   std::unique_ptr<Expr> target,
                   std::unique_ptr<Expr> value)
        : Stmt(location), target_(std::move(target)), value_(std::move(value)) {}

    Expr* getTarget() const { return target_.get(); }
    Expr* getValue() const { return value_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitAssignmentStmt(this);
    }
    
    std::string getNodeType() const override { return "AssignmentStmt"; }

private:
    std::unique_ptr<Expr> target_;
    std::unique_ptr<Expr> value_;
};

// For-in statement: for x in iterable { body }
class ForInStmt : public Stmt {
public:
    ForInStmt(const SourceLocation& location,
              const std::string& variableName,
              std::unique_ptr<Expr> iterable,
              std::vector<std::unique_ptr<Stmt>> body)
        : Stmt(location), variableName_(variableName),
          iterable_(std::move(iterable)), body_(std::move(body)) {}

    const std::string& getVariableName() const { return variableName_; }
    Expr* getIterable() const { return iterable_.get(); }
    const std::vector<std::unique_ptr<Stmt>>& getBody() const { return body_; }

    void accept(ASTVisitor& visitor) override {
        visitor.visitForInStmt(this);
    }
    std::string getNodeType() const override { return "ForInStmt"; }

private:
    std::string variableName_;
    std::unique_ptr<Expr> iterable_;
    std::vector<std::unique_ptr<Stmt>> body_;
};

// Select statement: select { receive/send/else branches }
class SelectStmt : public Stmt {
public:
    SelectStmt(const SourceLocation& location,
               std::vector<std::unique_ptr<SelectBranch>> branches)
        : Stmt(location), branches_(std::move(branches)) {}

    const std::vector<std::unique_ptr<SelectBranch>>& getBranches() const { return branches_; }
    void accept(ASTVisitor& visitor) override { visitor.visitSelectStmt(this); }
    std::string getNodeType() const override { return "SelectStmt"; }
private:
    std::vector<std::unique_ptr<SelectBranch>> branches_;
};

// Block expression (Rust-style): { stmt* expr? } — value is last expr or Unit
class BlockExpr : public Expr {
public:
    BlockExpr(const SourceLocation& location,
              std::vector<std::unique_ptr<Stmt>> statements,
              std::unique_ptr<Expr> valueExpr)
        : Expr(location), statements_(std::move(statements)), valueExpr_(std::move(valueExpr)) {}

    const std::vector<std::unique_ptr<Stmt>>& getStatements() const { return statements_; }
    Expr* getValueExpr() const { return valueExpr_.get(); }
    bool hasValueExpr() const { return valueExpr_ != nullptr; }

    void accept(ASTVisitor& visitor) override { visitor.visitBlockExpr(this); }
    std::string getNodeType() const override { return "BlockExpr"; }
private:
    std::vector<std::unique_ptr<Stmt>> statements_;
    std::unique_ptr<Expr> valueExpr_;
};

} // namespace ast
} // namespace first
