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

// Kind of a type parameter: * (concrete type) or * -> * (type constructor)
enum class GenericParamKind {
    Star,         // default: type parameter stands for a type (e.g. Int, List<Int>)
    StarArrowStar // higher-kinded: parameter stands for a type constructor (e.g. List, Option)
};

// Generic parameter with optional interface constraint (e.g. T : Eq) or Scala-like kind (e.g. F<_>, F<_, _>)
struct GenericParam {
    std::string name;
    std::string constraint;  // interface name when kind is Star; empty when higher-kinded
    GenericParamKind kind = GenericParamKind::Star;
    // For higher-kinded params: number of type arguments (F<_> => 1, F<_, _> => 2). Default 1.
    int kindArity = 1;

    bool isHigherKinded() const { return kind == GenericParamKind::StarArrowStar; }
};

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
                 std::vector<GenericParam> genericParams,
                 std::vector<std::unique_ptr<Parameter>> parameters,
                 std::unique_ptr<Type> returnType,
                 std::vector<std::unique_ptr<Stmt>> body,
                 bool isExported = false)
        : ASTNode(location), name_(name), genericParams_(std::move(genericParams)),
          parameters_(std::move(parameters)), returnType_(std::move(returnType)),
          body_(std::move(body)), isExported_(isExported) {}

    const std::string& getName() const { return name_; }
    const std::vector<GenericParam>& getGenericParams() const { return genericParams_; }
    std::vector<std::string> getGenericParamNames() const {
        std::vector<std::string> names;
        for (const auto& p : genericParams_) names.push_back(p.name);
        return names;
    }
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
    std::vector<GenericParam> genericParams_;
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
                    std::vector<GenericParam> genericParams,
                    std::vector<std::unique_ptr<Parameter>> parameters,
                    std::unique_ptr<Type> returnType,
                    std::vector<std::unique_ptr<Stmt>> body,
                    bool isExported = false)
        : ASTNode(location), name_(name), genericParams_(std::move(genericParams)),
          parameters_(std::move(parameters)), returnType_(std::move(returnType)),
          body_(std::move(body)), isExported_(isExported) {}

    const std::string& getName() const { return name_; }
    const std::vector<GenericParam>& getGenericParams() const { return genericParams_; }
    std::vector<std::string> getGenericParamNames() const {
        std::vector<std::string> names;
        for (const auto& p : genericParams_) names.push_back(p.name);
        return names;
    }
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
    std::vector<GenericParam> genericParams_;
    std::vector<std::unique_ptr<Parameter>> parameters_;
    std::unique_ptr<Type> returnType_;
    std::vector<std::unique_ptr<Stmt>> body_;
    bool isExported_;
};

// Type declaration: type Name = Type; or type Name<T,...> = Type; (alias or sum type)
// May have #derive(ToString, Eq, Ord) to generate implementations.
class TypeDecl : public ASTNode {
public:
    TypeDecl(const SourceLocation& location,
             const std::string& typeName,
             std::unique_ptr<Type> type,
             bool isExported = false,
             std::vector<std::string> typeParams = {},
             std::vector<std::string> derivedInterfaces = {})
        : ASTNode(location), typeName_(typeName), type_(std::move(type)),
          isExported_(isExported), typeParams_(std::move(typeParams)),
          derivedInterfaces_(std::move(derivedInterfaces)) {}

    const std::string& getTypeName() const { return typeName_; }
    Type* getType() const { return type_.get(); }
    const std::vector<std::string>& getTypeParams() const { return typeParams_; }
    bool isGeneric() const { return !typeParams_.empty(); }
    const std::vector<std::string>& getDerivedInterfaces() const { return derivedInterfaces_; }
    void setDerivedInterfaces(std::vector<std::string> v) { derivedInterfaces_ = std::move(v); }
    bool derives(const std::string& iface) const {
        for (const auto& d : derivedInterfaces_) if (d == iface) return true;
        return false;
    }

    bool isExported() const { return isExported_; }

    void accept(ASTVisitor& visitor) override {
        visitor.visitTypeDecl(this);
    }

    std::string getNodeType() const override { return "TypeDecl"; }

private:
    std::string typeName_;
    std::unique_ptr<Type> type_;
    bool isExported_;
    std::vector<std::string> typeParams_;
    std::vector<std::string> derivedInterfaces_;
};

// Interface member: name and type (e.g. show: function(T) -> String)
class InterfaceMember {
public:
    InterfaceMember(const SourceLocation& location,
                    const std::string& name,
                    std::unique_ptr<Type> type)
        : location_(location), name_(name), type_(std::move(type)) {}

    const SourceLocation& getLocation() const { return location_; }
    const std::string& getName() const { return name_; }
    Type* getType() const { return type_.get(); }

private:
    SourceLocation location_;
    std::string name_;
    std::unique_ptr<Type> type_;
};

// Interface declaration (typeclass-style): interface Name<T> { ... } or interface Name<F : * -> *> { ... }
class InterfaceDecl : public ASTNode {
public:
    InterfaceDecl(const SourceLocation& location,
                  const std::string& name,
                  std::vector<GenericParam> genericParams,
                  std::vector<std::unique_ptr<InterfaceMember>> members)
        : ASTNode(location), name_(name), genericParams_(std::move(genericParams)),
          members_(std::move(members)) {}

    const std::string& getName() const { return name_; }
    const std::vector<GenericParam>& getGenericParams() const { return genericParams_; }
    const std::vector<std::unique_ptr<InterfaceMember>>& getMembers() const { return members_; }

    InterfaceMember* getMember(const std::string& name) const {
        for (const auto& m : members_) {
            if (m->getName() == name) return m.get();
        }
        return nullptr;
    }

    void accept(ASTVisitor& visitor) override {
        visitor.visitInterfaceDecl(this);
    }

    std::string getNodeType() const override { return "InterfaceDecl"; }

private:
    std::string name_;
    std::vector<GenericParam> genericParams_;
    std::vector<std::unique_ptr<InterfaceMember>> members_;
};

// Implementation member: name = expression (method implementation)
class ImplementationMember {
public:
    ImplementationMember(const SourceLocation& location,
                         const std::string& name,
                         std::unique_ptr<Expr> value)
        : location_(location), name_(name), value_(std::move(value)) {}

    const SourceLocation& getLocation() const { return location_; }
    const std::string& getName() const { return name_; }
    Expr* getValue() const { return value_.get(); }

private:
    SourceLocation location_;
    std::string name_;
    std::unique_ptr<Expr> value_;
};

// Implementation declaration: implementation Interface<Type> { member = value; ... }
class ImplementationDecl : public ASTNode {
public:
    ImplementationDecl(const SourceLocation& location,
                       const std::string& interfaceName,
                       std::vector<std::unique_ptr<Type>> typeArgs,
                       std::vector<std::unique_ptr<ImplementationMember>> members)
        : ASTNode(location), interfaceName_(interfaceName), typeArgs_(std::move(typeArgs)),
          members_(std::move(members)) {}

    const std::string& getInterfaceName() const { return interfaceName_; }
    const std::vector<std::unique_ptr<Type>>& getTypeArgs() const { return typeArgs_; }
    const std::vector<std::unique_ptr<ImplementationMember>>& getMembers() const { return members_; }

    ImplementationMember* getMember(const std::string& name) const {
        for (const auto& m : members_) {
            if (m->getName() == name) return m.get();
        }
        return nullptr;
    }

    void accept(ASTVisitor& visitor) override {
        visitor.visitImplementationDecl(this);
    }

    std::string getNodeType() const override { return "ImplementationDecl"; }

private:
    std::string interfaceName_;
    std::vector<std::unique_ptr<Type>> typeArgs_;
    std::vector<std::unique_ptr<ImplementationMember>> members_;
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
