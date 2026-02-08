#pragma once

#include "first/ast/node.h"
#include "first/ast/types.h"
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

    // When set by type checker: use this function for ==/!= (Eq) or for </<=/>/>= (Ord) instead of built-in
    const std::string& getEqFunctionName() const { return eqFunctionName_; }
    void setEqFunctionName(std::string name) { eqFunctionName_ = std::move(name); }
    const std::string& getCompareFunctionName() const { return compareFunctionName_; }
    void setCompareFunctionName(std::string name) { compareFunctionName_ = std::move(name); }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitBinaryExpr(this);
    }
    
    std::string getNodeType() const override { return "BinaryExpr"; }

private:
    Op op_;
    std::unique_ptr<Expr> left_;
    std::unique_ptr<Expr> right_;
    std::string eqFunctionName_;       // non-empty => use this for == and !=
    std::string compareFunctionName_; // non-empty => use this for <, <=, >, >=
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

    /** Inferred type arguments for generic calls (set by type checker). Empty if not a generic call. */
    const std::vector<std::unique_ptr<Type>>& getInferredTypeArgs() const { return inferredTypeArgs_; }
    void setInferredTypeArgs(std::vector<std::unique_ptr<Type>> args) {
        inferredTypeArgs_ = std::move(args);
    }
    bool hasInferredTypeArgs() const { return !inferredTypeArgs_.empty(); }

    void accept(ASTVisitor& visitor) override {
        visitor.visitFunctionCallExpr(this);
    }
    
    std::string getNodeType() const override { return "FunctionCallExpr"; }

private:
    std::string name_;
    std::vector<std::unique_ptr<Expr>> args_;
    std::vector<std::unique_ptr<Type>> inferredTypeArgs_;
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

// Range expression: start..end (step 1) or start, second..end (step = second - first). Haskell-style.
// .. = exclusive end, ..= = inclusive end.
class RangeExpr : public Expr {
public:
    RangeExpr(const SourceLocation& location,
              std::unique_ptr<Expr> start,
              std::unique_ptr<Expr> end,
              bool inclusive,
              std::unique_ptr<Expr> stepHint = nullptr)
        : Expr(location), start_(std::move(start)), end_(std::move(end)), inclusive_(inclusive),
          stepHint_(std::move(stepHint)) {}

    Expr* getStart() const { return start_.get(); }
    Expr* getEnd() const { return end_.get(); }
    bool isInclusive() const { return inclusive_; }
    /// If set, step = stepHint - start (Haskell first, second..last). Otherwise step = 1.
    Expr* getStepHint() const { return stepHint_.get(); }
    bool hasStepHint() const { return stepHint_ != nullptr; }

    void accept(ASTVisitor& visitor) override {
        visitor.visitRangeExpr(this);
    }
    std::string getNodeType() const override { return "RangeExpr"; }

private:
    std::unique_ptr<Expr> start_;
    std::unique_ptr<Expr> end_;
    bool inclusive_;
    std::unique_ptr<Expr> stepHint_;
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

// Method call expression (interface only): receiver.methodName(args) -> methodName(receiver, args...)
class MethodCallExpr : public Expr {
public:
    MethodCallExpr(const SourceLocation& location,
                  std::unique_ptr<Expr> receiver,
                  const std::string& methodName,
                  std::vector<std::unique_ptr<Expr>> args)
        : Expr(location), receiver_(std::move(receiver)), methodName_(methodName), args_(std::move(args)) {}

    Expr* getReceiver() const { return receiver_.get(); }
    const std::string& getMethodName() const { return methodName_; }
    const std::vector<std::unique_ptr<Expr>>& getArgs() const { return args_; }

    /** Inferred type of receiver (set by type checker) for IR dispatch. */
    const std::vector<std::unique_ptr<Type>>& getInferredTypeArgs() const { return inferredTypeArgs_; }
    void setInferredTypeArgs(std::vector<std::unique_ptr<Type>> args) {
        inferredTypeArgs_ = std::move(args);
    }
    bool hasInferredTypeArgs() const { return !inferredTypeArgs_.empty(); }

    void accept(ASTVisitor& visitor) override {
        visitor.visitMethodCallExpr(this);
    }

    std::string getNodeType() const override { return "MethodCallExpr"; }

private:
    std::unique_ptr<Expr> receiver_;
    std::string methodName_;
    std::vector<std::unique_ptr<Expr>> args_;
    std::vector<std::unique_ptr<Type>> inferredTypeArgs_;
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

// Concurrency: async expr (returns Promise<T,E>), await expr (unwraps Promise)
class AsyncExpr : public Expr {
public:
    AsyncExpr(const SourceLocation& location, std::unique_ptr<Expr> operand)
        : Expr(location), operand_(std::move(operand)) {}

    Expr* getOperand() const { return operand_.get(); }
    void accept(ASTVisitor& visitor) override { visitor.visitAsyncExpr(this); }
    std::string getNodeType() const override { return "AsyncExpr"; }
private:
    std::unique_ptr<Expr> operand_;
};

class AwaitExpr : public Expr {
public:
    AwaitExpr(const SourceLocation& location, std::unique_ptr<Expr> operand)
        : Expr(location), operand_(std::move(operand)) {}

    Expr* getOperand() const { return operand_.get(); }
    void accept(ASTVisitor& visitor) override { visitor.visitAwaitExpr(this); }
    std::string getNodeType() const override { return "AwaitExpr"; }
private:
    std::unique_ptr<Expr> operand_;
};

// spawn expr (returns Task<T>), join expr (unwraps Task)
class SpawnExpr : public Expr {
public:
    SpawnExpr(const SourceLocation& location, std::unique_ptr<Expr> operand)
        : Expr(location), operand_(std::move(operand)) {}

    Expr* getOperand() const { return operand_.get(); }
    void accept(ASTVisitor& visitor) override { visitor.visitSpawnExpr(this); }
    std::string getNodeType() const override { return "SpawnExpr"; }
private:
    std::unique_ptr<Expr> operand_;
};

class JoinExpr : public Expr {
public:
    JoinExpr(const SourceLocation& location, std::unique_ptr<Expr> operand)
        : Expr(location), operand_(std::move(operand)) {}

    Expr* getOperand() const { return operand_.get(); }
    void accept(ASTVisitor& visitor) override { visitor.visitJoinExpr(this); }
    std::string getNodeType() const override { return "JoinExpr"; }
private:
    std::unique_ptr<Expr> operand_;
};

// Select branch: receive (<- channel => x: stmt) | send (channel <- value: stmt) | else: stmt
class SelectBranch {
public:
    enum class Kind { Receive, Send, Else };
    SelectBranch(SourceLocation location, Kind kind,
                 std::unique_ptr<Expr> channelExpr,
                 std::string varName,
                 std::unique_ptr<Expr> valueExpr,
                 std::unique_ptr<Stmt> statement);
    ~SelectBranch();  // Defined in select_branch.cpp (needs complete Stmt type)
    const SourceLocation& getLocation() const { return location_; }
    Kind getKind() const { return kind_; }
    Expr* getChannelExpr() const { return channelExpr_.get(); }
    const std::string& getVarName() const { return varName_; }
    Expr* getValueExpr() const { return valueExpr_.get(); }
    Stmt* getStatement() const { return statement_.get(); }
private:
    SourceLocation location_;
    Kind kind_;
    std::unique_ptr<Expr> channelExpr_;
    std::string varName_;
    std::unique_ptr<Expr> valueExpr_;
    std::unique_ptr<Stmt> statement_;
};

// Select expression: select { branches }
class SelectExpr : public Expr {
public:
    SelectExpr(const SourceLocation& location,
               std::vector<std::unique_ptr<SelectBranch>> branches)
        : Expr(location), branches_(std::move(branches)) {}
    const std::vector<std::unique_ptr<SelectBranch>>& getBranches() const { return branches_; }
    void accept(ASTVisitor& visitor) override { visitor.visitSelectExpr(this); }
    std::string getNodeType() const override { return "SelectExpr"; }
private:
    std::vector<std::unique_ptr<SelectBranch>> branches_;
};

// If expression (Rust-style): if (cond) thenBranch else elseBranch — value is branch value
// elseBranch can be another IfExpr (else if) or BlockExpr or any Expr
class IfExpr : public Expr {
public:
    IfExpr(const SourceLocation& location,
           std::unique_ptr<Expr> condition,
           std::unique_ptr<Expr> thenBranch,
           std::unique_ptr<Expr> elseBranch)
        : Expr(location), condition_(std::move(condition)),
          thenBranch_(std::move(thenBranch)), elseBranch_(std::move(elseBranch)) {}

    Expr* getCondition() const { return condition_.get(); }
    Expr* getThenBranch() const { return thenBranch_.get(); }
    Expr* getElseBranch() const { return elseBranch_.get(); }

    void accept(ASTVisitor& visitor) override { visitor.visitIfExpr(this); }
    std::string getNodeType() const override { return "IfExpr"; }
private:
    std::unique_ptr<Expr> condition_;
    std::unique_ptr<Expr> thenBranch_;
    std::unique_ptr<Expr> elseBranch_;
};

} // namespace ast
} // namespace first
