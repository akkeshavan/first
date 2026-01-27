#pragma once

#include "first/ast/node.h"
#include "first/ast/visitor.h"
#include <string>
#include <vector>
#include <memory>

namespace first {
namespace ast {

// Base class for types
class Type : public ASTNode {
public:
    Type(const SourceLocation& location) : ASTNode(location) {}
    virtual ~Type() = default;
    
    // Type is abstract - derived types must implement accept
    // We don't implement it here because Type is meant to be abstract
};

// Primitive type
class PrimitiveType : public Type {
public:
    enum class Kind {
        Int,
        Float,
        Bool,
        String,
        Unit
    };

    PrimitiveType(const SourceLocation& location, Kind kind)
        : Type(location), kind_(kind) {}

    Kind getKind() const { return kind_; }
    
    void accept(ASTVisitor& visitor) override;
    std::string getNodeType() const override { return "PrimitiveType"; }

private:
    Kind kind_;
};

// Array type
class ArrayType : public Type {
public:
    ArrayType(const SourceLocation& location, std::unique_ptr<Type> elementType)
        : Type(location), elementType_(std::move(elementType)) {}

    Type* getElementType() const { return elementType_.get(); }
    
    void accept(ASTVisitor& visitor) override;
    std::string getNodeType() const override { return "ArrayType"; }

private:
    std::unique_ptr<Type> elementType_;
};

// Function type: function(ParamTypes) -> ReturnType
class FunctionType : public Type {
public:
    FunctionType(const SourceLocation& location,
                 std::vector<std::unique_ptr<Type>> paramTypes,
                 std::unique_ptr<Type> returnType,
                 bool isInteraction = false)
        : Type(location), paramTypes_(std::move(paramTypes)), 
          returnType_(std::move(returnType)), isInteraction_(isInteraction) {}

    const std::vector<std::unique_ptr<Type>>& getParamTypes() const { return paramTypes_; }
    Type* getReturnType() const { return returnType_.get(); }
    bool isInteraction() const { return isInteraction_; }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { 
        return isInteraction_ ? "InteractionType" : "FunctionType"; 
    }

private:
    std::vector<std::unique_ptr<Type>> paramTypes_;
    std::unique_ptr<Type> returnType_;
    bool isInteraction_; // true for interaction, false for pure function
};

// Generic type parameter
class GenericType : public Type {
public:
    GenericType(const SourceLocation& location, const std::string& name)
        : Type(location), name_(name) {}

    const std::string& getName() const { return name_; }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "GenericType"; }

private:
    std::string name_;
};

// Type with generic parameters (e.g., Option<T>)
class ParameterizedType : public Type {
public:
    ParameterizedType(const SourceLocation& location,
                      const std::string& baseName,
                      std::vector<std::unique_ptr<Type>> typeArgs)
        : Type(location), baseName_(baseName), typeArgs_(std::move(typeArgs)) {}

    const std::string& getBaseName() const { return baseName_; }
    const std::vector<std::unique_ptr<Type>>& getTypeArgs() const { return typeArgs_; }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "ParameterizedType"; }

private:
    std::string baseName_;
    std::vector<std::unique_ptr<Type>> typeArgs_;
};

// Forward declaration for ADT constructor
class Constructor;

// Algebraic Data Type (sum type)
class ADTType : public Type {
public:
    ADTType(const SourceLocation& location,
            const std::string& name,
            std::vector<std::unique_ptr<Constructor>> constructors)
        : Type(location), name_(name), constructors_(std::move(constructors)) {}
    
    const std::string& getName() const { return name_; }
    const std::vector<std::unique_ptr<Constructor>>& getConstructors() const { return constructors_; }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "ADTType"; }

private:
    std::string name_;
    std::vector<std::unique_ptr<Constructor>> constructors_;
};

// ADT Constructor
class Constructor {
public:
    Constructor(const SourceLocation& location,
                const std::string& name,
                std::vector<std::unique_ptr<Type>> argumentTypes)
        : location_(location), name_(name), argumentTypes_(std::move(argumentTypes)) {}
    
    const SourceLocation& getLocation() const { return location_; }
    const std::string& getName() const { return name_; }
    const std::vector<std::unique_ptr<Type>>& getArgumentTypes() const { return argumentTypes_; }

private:
    SourceLocation location_;
    std::string name_;
    std::vector<std::unique_ptr<Type>> argumentTypes_;
};

} // namespace ast
} // namespace first
