#pragma once

#include "first/ast/node.h"
#include "first/ast/visitor.h"
#include <string>
#include <vector>
#include <memory>
#include <utility>
#include <cstddef>

namespace first {
namespace ast {

class Expr;

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
        Unit,
        Null,
        ArrayBuf
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

// Record field definition
class RecordField {
public:
    RecordField(const SourceLocation& location,
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

// Record type (struct/product type)
class RecordType : public Type {
public:
    RecordType(const SourceLocation& location,
               std::vector<std::unique_ptr<RecordField>> fields)
        : Type(location), fields_(std::move(fields)) {}

    const std::vector<std::unique_ptr<RecordField>>& getFields() const { return fields_; }
    
    // Find field by name
    RecordField* getField(const std::string& name) const {
        for (const auto& field : fields_) {
            if (field->getName() == name) {
                return field.get();
            }
        }
        return nullptr;
    }
    
    // Get field index by name
    size_t getFieldIndex(const std::string& name) const {
        for (size_t i = 0; i < fields_.size(); ++i) {
            if (fields_[i]->getName() == name) {
                return i;
            }
        }
        return fields_.size(); // Not found
    }
    
    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    
    std::string getNodeType() const override { return "RecordType"; }

private:
    std::vector<std::unique_ptr<RecordField>> fields_;
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

// Refinement type: {{variable: BaseType where predicate}}
// Values of this type satisfy the predicate at runtime (checked on entry).
class RefinementType : public Type {
public:
    RefinementType(const SourceLocation& location,
                   const std::string& variableName,
                   std::unique_ptr<Type> baseType,
                   std::shared_ptr<Expr> predicate);
    ~RefinementType() override;

    const std::string& getVariableName() const { return variableName_; }
    Type* getBaseType() const { return baseType_.get(); }
    Expr* getPredicate() const { return predicate_.get(); }
    std::shared_ptr<Expr> getPredicateShared() const { return predicate_; }

    void accept(ASTVisitor& visitor) override {
        (void)visitor;
    }
    std::string getNodeType() const override { return "RefinementType"; }

private:
    std::string variableName_;
    std::unique_ptr<Type> baseType_;
    std::shared_ptr<Expr> predicate_;
};

// Indexed type: BaseType[index1, index2, ...] (e.g. Vector[n], Array<Int>[n])
// Indices are expressions (literals or identifiers for type/term parameters).
class IndexedType : public Type {
public:
    IndexedType(const SourceLocation& location,
                std::unique_ptr<Type> baseType,
                std::vector<std::shared_ptr<Expr>> indices);
    ~IndexedType() override;

    Type* getBaseType() const { return baseType_.get(); }
    const std::vector<std::shared_ptr<Expr>>& getIndices() const { return indices_; }

    void accept(ASTVisitor& visitor) override;
    std::string getNodeType() const override { return "IndexedType"; }

private:
    std::unique_ptr<Type> baseType_;
    std::vector<std::shared_ptr<Expr>> indices_;
};

// Dependent function type (Pi type): (paramName: ParamType) -> ReturnType
// The return type can reference the parameter name (e.g. (n: Int) -> Vector[n]).
class DependentFunctionType : public Type {
public:
    DependentFunctionType(const SourceLocation& location,
                         const std::string& paramName,
                         std::unique_ptr<Type> paramType,
                         std::unique_ptr<Type> returnType);
    ~DependentFunctionType() override;

    const std::string& getParamName() const { return paramName_; }
    Type* getParamType() const { return paramType_.get(); }
    Type* getReturnType() const { return returnType_.get(); }

    void accept(ASTVisitor& visitor) override;
    std::string getNodeType() const override { return "DependentFunctionType"; }

private:
    std::string paramName_;
    std::unique_ptr<Type> paramType_;
    std::unique_ptr<Type> returnType_;
};

// Dependent pair type (Sigma type): (varName: VarType) * BodyType
// The body type can reference the variable name (e.g. (n: Int) * Array<Int>[n]).
class DependentPairType : public Type {
public:
    DependentPairType(const SourceLocation& location,
                      const std::string& varName,
                      std::unique_ptr<Type> varType,
                      std::unique_ptr<Type> bodyType);
    ~DependentPairType() override;

    const std::string& getVarName() const { return varName_; }
    Type* getVarType() const { return varType_.get(); }
    Type* getBodyType() const { return bodyType_.get(); }

    void accept(ASTVisitor& visitor) override;
    std::string getNodeType() const override { return "DependentPairType"; }

private:
    std::string varName_;
    std::unique_ptr<Type> varType_;
    std::unique_ptr<Type> bodyType_;
};

// Forall type: forall T U. Type (polymorphic type; body can reference T, U)
class ForallType : public Type {
public:
    ForallType(const SourceLocation& location,
               std::vector<std::string> typeVars,
               std::unique_ptr<Type> bodyType);
    ~ForallType() override;

    const std::vector<std::string>& getTypeVars() const { return typeVars_; }
    Type* getBodyType() const { return bodyType_.get(); }

    void accept(ASTVisitor& visitor) override;
    std::string getNodeType() const override { return "ForallType"; }

private:
    std::vector<std::string> typeVars_;
    std::unique_ptr<Type> bodyType_;
};

// Existential type: exists x: VarType. BodyType (body can reference x)
class ExistentialType : public Type {
public:
    ExistentialType(const SourceLocation& location,
                    const std::string& varName,
                    std::unique_ptr<Type> varType,
                    std::unique_ptr<Type> bodyType);
    ~ExistentialType() override;

    const std::string& getVarName() const { return varName_; }
    Type* getVarType() const { return varType_.get(); }
    Type* getBodyType() const { return bodyType_.get(); }

    void accept(ASTVisitor& visitor) override;
    std::string getNodeType() const override { return "ExistentialType"; }

private:
    std::string varName_;
    std::unique_ptr<Type> varType_;
    std::unique_ptr<Type> bodyType_;
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
