#pragma once

#include "first/ast/node.h"
#include "first/ast/visitor.h"
#include "first/ast/types.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include <string>
#include <vector>
#include <memory>

namespace first {
namespace ast {

// Forward declarations
class Stmt;
class Parameter;

// Function parameter
class Parameter : public ASTNode {
public:
    Parameter(const SourceLocation& location, 
              const std::string& name,
              std::unique_ptr<Type> type)
        : ASTNode(location), name_(name), type_(std::move(type)) {}

    const std::string& getName() const { return name_; }
    Type* getType() const { return type_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        // Parameters don't need visitor methods yet
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "Parameter"; }

private:
    std::string name_;
    std::unique_ptr<Type> type_;
};

// Function declaration
class FunctionDecl : public ASTNode {
public:
    FunctionDecl(const SourceLocation& location,
                 const std::string& name,
                 std::vector<std::string> genericParams,
                 std::vector<std::unique_ptr<Parameter>> parameters,
                 std::unique_ptr<Type> returnType,
                 std::vector<std::unique_ptr<Stmt>> body,
                 bool isExported = false)
        : ASTNode(location), name_(name), genericParams_(std::move(genericParams)),
          parameters_(std::move(parameters)), returnType_(std::move(returnType)),
          body_(std::move(body)), isExported_(isExported) {}

    const std::string& getName() const { return name_; }
    const std::vector<std::string>& getGenericParams() const { return genericParams_; }
    const std::vector<std::unique_ptr<Parameter>>& getParameters() const { return parameters_; }
    Type* getReturnType() const { return returnType_.get(); }
    const std::vector<std::unique_ptr<Stmt>>& getBody() const { return body_; }
    bool isSignature() const { return body_.empty(); } // Function signature without body
    bool isExported() const { return isExported_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitFunctionDecl(this);
    }
    
    std::string getNodeType() const override { return "FunctionDecl"; }

private:
    std::string name_;
    std::vector<std::string> genericParams_;
    std::vector<std::unique_ptr<Parameter>> parameters_;
    std::unique_ptr<Type> returnType_;
    std::vector<std::unique_ptr<Stmt>> body_;
    bool isExported_;
};

// Interaction declaration  
class InteractionDecl : public ASTNode {
public:
    InteractionDecl(const SourceLocation& location,
                    const std::string& name,
                    std::vector<std::string> genericParams,
                    std::vector<std::unique_ptr<Parameter>> parameters,
                    std::unique_ptr<Type> returnType,
                    std::vector<std::unique_ptr<Stmt>> body,
                    bool isExported = false)
        : ASTNode(location), name_(name), genericParams_(std::move(genericParams)),
          parameters_(std::move(parameters)), returnType_(std::move(returnType)),
          body_(std::move(body)), isExported_(isExported) {}

    const std::string& getName() const { return name_; }
    const std::vector<std::string>& getGenericParams() const { return genericParams_; }
    const std::vector<std::unique_ptr<Parameter>>& getParameters() const { return parameters_; }
    Type* getReturnType() const { return returnType_.get(); }
    const std::vector<std::unique_ptr<Stmt>>& getBody() const { return body_; }
    bool isExported() const { return isExported_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitInteractionDecl(this);
    }
    
    std::string getNodeType() const override { return "InteractionDecl"; }

private:
    std::string name_;
    std::vector<std::string> genericParams_;
    std::vector<std::unique_ptr<Parameter>> parameters_;
    std::unique_ptr<Type> returnType_;
    std::vector<std::unique_ptr<Stmt>> body_;
    bool isExported_;
};

// Type declaration
class TypeDecl : public ASTNode {
public:
    TypeDecl(const SourceLocation& location, bool isExported = false)
        : ASTNode(location), isExported_(isExported) {}
    
    bool isExported() const { return isExported_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitTypeDecl(this);
    }
    
    std::string getNodeType() const override { return "TypeDecl"; }

private:
    bool isExported_;
};

// Import declaration
class ImportDecl : public ASTNode {
public:
    enum class ImportKind {
        All,        // import * "module"
        Specific,   // import { a, b, c } "module"
        Default     // import "module"
    };
    
    ImportDecl(const SourceLocation& location,
               ImportKind kind,
               const std::string& moduleName,
               std::vector<std::string> symbols = {})
        : ASTNode(location), kind_(kind), moduleName_(moduleName), symbols_(std::move(symbols)) {}
    
    ImportKind getKind() const { return kind_; }
    const std::string& getModuleName() const { return moduleName_; }
    const std::vector<std::string>& getSymbols() const { return symbols_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitImportDecl(this);
    }
    
    std::string getNodeType() const override { return "ImportDecl"; }

private:
    ImportKind kind_;
    std::string moduleName_;
    std::vector<std::string> symbols_; // For Specific imports
};

} // namespace ast
} // namespace first
