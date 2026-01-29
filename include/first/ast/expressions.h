#pragma once

#include "first/ast/node.h"
#include "first/ast/visitor.h"
#include <string>
#include <vector>
#include <memory>

namespace first {
namespace ast {

// Forward declarations
class Pattern;
class MatchCase;
class Parameter;
class Stmt;
class Type;

// Base class for expressions
class Expr : public ASTNode {
public:
    Expr(const SourceLocation& location) : ASTNode(location) {}
};

// Literal expression node
class LiteralExpr : public Expr {
public:
    enum class LiteralType {
        Int,
        Float,
        Bool,
        String,
        Null
    };

    LiteralExpr(const SourceLocation& location, LiteralType type, const std::string& value)
        : Expr(location), type_(type), value_(value) {}

    LiteralType getType() const { return type_; }
    const std::string& getValue() const { return value_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitLiteralExpr(this);
    }
    
    std::string getNodeType() const override { return "LiteralExpr"; }

private:
    LiteralType type_;
    std::string value_;
};

// Binary operator expression
class BinaryExpr : public Expr {
public:
    enum class Op {
        Add, Sub, Mul, Div, Mod,
        Eq, Ne, Lt, Le, Gt, Ge,
        And, Or
    };

    BinaryExpr(const SourceLocation& location, Op op, 
               std::unique_ptr<Expr> left, std::unique_ptr<Expr> right)
        : Expr(location), op_(op), left_(std::move(left)), right_(std::move(right)) {}

    Op getOp() const { return op_; }
    Expr* getLeft() const { return left_.get(); }
    Expr* getRight() const { return right_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitBinaryExpr(this);
    }
    
    std::string getNodeType() const override { return "BinaryExpr"; }

private:
    Op op_;
    std::unique_ptr<Expr> left_;
    std::unique_ptr<Expr> right_;
};

// Unary operator expression
class UnaryExpr : public Expr {
public:
    enum class Op {
        Neg,    // Unary minus
        Not     // Logical not
    };

    UnaryExpr(const SourceLocation& location, Op op, std::unique_ptr<Expr> operand)
        : Expr(location), op_(op), operand_(std::move(operand)) {}

    Op getOp() const { return op_; }
    Expr* getOperand() const { return operand_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitUnaryExpr(this);
    }
    
    std::string getNodeType() const override { return "UnaryExpr"; }

private:
    Op op_;
    std::unique_ptr<Expr> operand_;
};

// Variable reference expression
class VariableExpr : public Expr {
public:
    VariableExpr(const SourceLocation& location, const std::string& name)
        : Expr(location), name_(name) {}

    const std::string& getName() const { return name_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitVariableExpr(this);
    }
    
    std::string getNodeType() const override { return "VariableExpr"; }

private:
    std::string name_;
};

// Function call expression
class FunctionCallExpr : public Expr {
public:
    FunctionCallExpr(const SourceLocation& location, const std::string& name,
                     std::vector<std::unique_ptr<Expr>> args)
        : Expr(location), name_(name), args_(std::move(args)) {}

    const std::string& getName() const { return name_; }
    const std::vector<std::unique_ptr<Expr>>& getArgs() const { return args_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitFunctionCallExpr(this);
    }
    
    std::string getNodeType() const override { return "FunctionCallExpr"; }

private:
    std::string name_;
    std::vector<std::unique_ptr<Expr>> args_;
};

// Constructor call expression (for ADT constructors)
class ConstructorExpr : public Expr {
public:
    ConstructorExpr(const SourceLocation& location,
                    const std::string& constructorName,
                    std::vector<std::unique_ptr<Expr>> arguments)
        : Expr(location), constructorName_(constructorName), arguments_(std::move(arguments)) {}
    
    const std::string& getConstructorName() const { return constructorName_; }
    const std::vector<std::unique_ptr<Expr>>& getArguments() const { return arguments_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitConstructorExpr(this);
    }
    
    std::string getNodeType() const override { return "ConstructorExpr"; }

private:
    std::string constructorName_;
    std::vector<std::unique_ptr<Expr>> arguments_;
};

// Match expression (pattern matching)
class MatchExpr : public Expr {
public:
    MatchExpr(const SourceLocation& location,
              std::unique_ptr<Expr> matchedExpr,
              std::vector<std::unique_ptr<MatchCase>> cases);
    
    Expr* getMatchedExpr() const;
    const std::vector<std::unique_ptr<MatchCase>>& getCases() const;
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitMatchExpr(this);
    }
    
    std::string getNodeType() const override { return "MatchExpr"; }

private:
    std::unique_ptr<Expr> matchedExpr_;
    std::vector<std::unique_ptr<MatchCase>> cases_;
};

// Array literal expression: [expr1, expr2, ...]
class ArrayLiteralExpr : public Expr {
public:
    ArrayLiteralExpr(const SourceLocation& location,
                     std::vector<std::unique_ptr<Expr>> elements)
        : Expr(location), elements_(std::move(elements)) {}

    const std::vector<std::unique_ptr<Expr>>& getElements() const { return elements_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitArrayLiteralExpr(this);
    }
    
    std::string getNodeType() const override { return "ArrayLiteralExpr"; }

private:
    std::vector<std::unique_ptr<Expr>> elements_;
};

// Array indexing expression: arr[index]
class ArrayIndexExpr : public Expr {
public:
    ArrayIndexExpr(const SourceLocation& location,
                   std::unique_ptr<Expr> array,
                   std::unique_ptr<Expr> index)
        : Expr(location), array_(std::move(array)), index_(std::move(index)) {}

    Expr* getArray() const { return array_.get(); }
    Expr* getIndex() const { return index_.get(); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitArrayIndexExpr(this);
    }
    
    std::string getNodeType() const override { return "ArrayIndexExpr"; }

private:
    std::unique_ptr<Expr> array_;
    std::unique_ptr<Expr> index_;
};

// Record literal expression: {field1: value1, field2: value2}
class RecordLiteralExpr : public Expr {
public:
    struct Field {
        std::string name;
        std::unique_ptr<Expr> value;
        
        Field(const std::string& n, std::unique_ptr<Expr> v)
            : name(n), value(std::move(v)) {}
    };

    RecordLiteralExpr(const SourceLocation& location,
                      std::vector<Field> fields)
        : Expr(location), fields_(std::move(fields)) {}

    const std::vector<Field>& getFields() const { return fields_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitRecordLiteralExpr(this);
    }
    
    std::string getNodeType() const override { return "RecordLiteralExpr"; }

private:
    std::vector<Field> fields_;
};

// Field access expression: record.field
class FieldAccessExpr : public Expr {
public:
    FieldAccessExpr(const SourceLocation& location,
                    std::unique_ptr<Expr> record,
                    const std::string& fieldName)
        : Expr(location), record_(std::move(record)), fieldName_(fieldName) {}

    Expr* getRecord() const { return record_.get(); }
    const std::string& getFieldName() const { return fieldName_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitFieldAccessExpr(this);
    }
    
    std::string getNodeType() const override { return "FieldAccessExpr"; }

private:
    std::unique_ptr<Expr> record_;
    std::string fieldName_;
};

// Lambda expression (closure): (params) => body or function(params) body
// Forward declarations for Parameter and Stmt are in the forward declarations section
class LambdaExpr : public Expr {
public:
    LambdaExpr(const SourceLocation& location,
               std::vector<std::unique_ptr<Parameter>> parameters,
               std::unique_ptr<Type> returnType,
               std::vector<std::unique_ptr<Stmt>> body);
    
    const std::vector<std::unique_ptr<Parameter>>& getParameters() const { return parameters_; }
    Type* getReturnType() const { return returnType_.get(); }
    const std::vector<std::unique_ptr<Stmt>>& getBody() const { return body_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitLambdaExpr(this);
    }
    
    std::string getNodeType() const override { return "LambdaExpr"; }

private:
    std::vector<std::unique_ptr<Parameter>> parameters_;
    std::unique_ptr<Type> returnType_;
    std::vector<std::unique_ptr<Stmt>> body_;
};

} // namespace ast
} // namespace first
