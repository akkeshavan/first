#include "first/ir/ir_generator.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/declarations.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/Support/raw_ostream.h>
#include <iostream>

namespace first {
namespace ir {

IRGenerator::IRGenerator(ErrorReporter& errorReporter, const std::string& moduleName)
    : errorReporter_(errorReporter)
    , module_(std::make_unique<llvm::Module>(moduleName, context_))
    , builder_(std::make_unique<llvm::IRBuilder<>>(context_))
    , currentFunction_(nullptr)
    , currentValue_(nullptr)
{
}

IRGenerator::~IRGenerator() = default;

bool IRGenerator::generate(ast::Program* program) {
    if (!program) {
        errorReporter_.error(
            SourceLocation(1, 1, "unknown"),
            "Cannot generate IR: AST is null"
        );
        return false;
    }
    
    // Visit the program node to generate IR
    program->accept(*this);
    
    return !errorReporter_.hasErrors();
}

void IRGenerator::printIR() const {
    if (module_) {
        module_->print(llvm::errs(), nullptr);
    }
}

void IRGenerator::visitProgram(ast::Program* node) {
    // Generate IR for all declarations in the program
    for (const auto& func : node->getFunctions()) {
        func->accept(*this);
    }
    
    for (const auto& interaction : node->getInteractions()) {
        interaction->accept(*this);
    }
    
    // TODO: Generate IR for type declarations and imports
}

void IRGenerator::visitFunctionDecl(ast::FunctionDecl* node) {
    // Get function name
    std::string funcName = node->getName();
    
    // Convert parameter types
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : node->getParameters()) {
        llvm::Type* paramType = convertType(param->getType());
        if (!paramType) {
            errorReporter_.error(
                node->getLocation(),
                "Failed to convert parameter type for function: " + funcName
            );
            return;
        }
        paramTypes.push_back(paramType);
    }
    
    // Convert return type
    llvm::Type* returnType = convertType(node->getReturnType());
    if (!returnType) {
        errorReporter_.error(
            node->getLocation(),
            "Failed to convert return type for function: " + funcName
        );
        return;
    }
    
    // Create function type
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    
    // Create function
    llvm::Function* func = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        funcName,
        module_.get()
    );
    
    // Set parameter names
    unsigned idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            arg.setName(node->getParameters()[idx]->getName());
        }
        idx++;
    }
    
    // Generate function body
    enterFunction(func);
    
    // Create entry block
    llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(context_, "entry", func);
    builder_->SetInsertPoint(entryBlock);
    
    // Store parameters in local variables (need to allocate space for them)
    idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            std::string paramName = node->getParameters()[idx]->getName();
            // Allocate space for parameter
            llvm::AllocaInst* alloca = builder_->CreateAlloca(arg.getType(), nullptr, paramName);
            builder_->CreateStore(&arg, alloca);
            setVariable(paramName, alloca);
        }
        idx++;
    }
    
    // Generate statements
    const auto& body = node->getBody();
    bool hasReturn = false;
    for (const auto& stmt : body) {
        generateStatement(stmt.get());
        // Check if this was a return statement
        if (dynamic_cast<ast::ReturnStmt*>(stmt.get())) {
            hasReturn = true;
            // Don't break - continue to check for errors, but we know we have a return
        }
    }
    
    // If function doesn't have a return statement, add one
    if (!hasReturn) {
        if (returnType->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            // Return default value for non-void functions
            if (returnType->isIntegerTy(64)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true)));
            } else if (returnType->isDoubleTy()) {
                builder_->CreateRet(llvm::ConstantFP::get(context_, llvm::APFloat(0.0)));
            } else if (returnType->isIntegerTy(1)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(1, 0)));
            } else {
                errorReporter_.error(
                    node->getLocation(),
                    "Function " + funcName + " missing return statement"
                );
            }
        }
    }
    
    exitFunction();
}

void IRGenerator::visitInteractionDecl(ast::InteractionDecl* node) {
    // Interactions are similar to functions but allow side effects
    // For now, generate IR the same way as functions
    // Get function name
    std::string funcName = node->getName();
    
    // Convert parameter types
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : node->getParameters()) {
        llvm::Type* paramType = convertType(param->getType());
        if (!paramType) {
            errorReporter_.error(
                node->getLocation(),
                "Failed to convert parameter type for interaction: " + funcName
            );
            return;
        }
        paramTypes.push_back(paramType);
    }
    
    // Convert return type
    llvm::Type* returnType = convertType(node->getReturnType());
    if (!returnType) {
        errorReporter_.error(
            node->getLocation(),
            "Failed to convert return type for interaction: " + funcName
        );
        return;
    }
    
    // Create function type
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    
    // Create function
    llvm::Function* func = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        funcName,
        module_.get()
    );
    
    // Set parameter names
    unsigned idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            arg.setName(node->getParameters()[idx]->getName());
        }
        idx++;
    }
    
    // Generate function body
    enterFunction(func);
    
    // Create entry block
    llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(context_, "entry", func);
    builder_->SetInsertPoint(entryBlock);
    
    // Store parameters in local variables (allocate space for them)
    idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            std::string paramName = node->getParameters()[idx]->getName();
            // Allocate space for parameter
            llvm::AllocaInst* alloca = builder_->CreateAlloca(arg.getType(), nullptr, paramName);
            builder_->CreateStore(&arg, alloca);
            setVariable(paramName, alloca);
        }
        idx++;
    }
    
    // Generate statements
    const auto& body = node->getBody();
    bool hasReturn = false;
    for (const auto& stmt : body) {
        generateStatement(stmt.get());
        // Check if this was a return statement
        if (dynamic_cast<ast::ReturnStmt*>(stmt.get())) {
            hasReturn = true;
        }
    }
    
    // If function doesn't have a return statement, add one
    if (!hasReturn) {
        if (returnType->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            // Return default value for non-void functions
            if (returnType->isIntegerTy(64)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true)));
            } else if (returnType->isDoubleTy()) {
                builder_->CreateRet(llvm::ConstantFP::get(context_, llvm::APFloat(0.0)));
            } else if (returnType->isIntegerTy(1)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(1, 0)));
            } else {
                errorReporter_.error(
                    node->getLocation(),
                    "Interaction " + funcName + " missing return statement"
                );
            }
        }
    }
    
    exitFunction();
}

void IRGenerator::visitLiteralExpr(ast::LiteralExpr* node) {
    currentValue_ = evaluateLiteral(node);
}

void IRGenerator::visitBinaryExpr(ast::BinaryExpr* node) {
    currentValue_ = evaluateBinary(node);
}

void IRGenerator::visitUnaryExpr(ast::UnaryExpr* node) {
    currentValue_ = evaluateUnary(node);
}

void IRGenerator::visitVariableExpr(ast::VariableExpr* node) {
    currentValue_ = evaluateVariable(node);
}

void IRGenerator::visitFunctionCallExpr(ast::FunctionCallExpr* node) {
    currentValue_ = evaluateFunctionCall(node);
}

void IRGenerator::visitArrayLiteralExpr(ast::ArrayLiteralExpr* node) {
    currentValue_ = evaluateArrayLiteral(node);
}

void IRGenerator::visitArrayIndexExpr(ast::ArrayIndexExpr* node) {
    currentValue_ = evaluateArrayIndex(node);
}

void IRGenerator::visitVariableDecl(ast::VariableDecl* node) {
    // Handled by generateStatement
}

void IRGenerator::visitReturnStmt(ast::ReturnStmt* node) {
    // Handled by generateStatement
}

void IRGenerator::visitExprStmt(ast::ExprStmt* node) {
    // Handled by generateStatement
}

llvm::Type* IRGenerator::convertType(ast::Type* type) {
    if (!type) {
        return nullptr;
    }
    
    // Dispatch to specific type converters
    if (auto* primType = dynamic_cast<ast::PrimitiveType*>(type)) {
        return convertPrimitiveType(primType);
    } else if (auto* arrType = dynamic_cast<ast::ArrayType*>(type)) {
        return convertArrayType(arrType);
    }
    
    // TODO: Handle other types (FunctionType, ADTType, etc.)
    errorReporter_.error(
        type->getLocation(),
        "Unsupported type for IR generation"
    );
    return nullptr;
}

llvm::Type* IRGenerator::convertPrimitiveType(ast::PrimitiveType* type) {
    switch (type->getKind()) {
        case ast::PrimitiveType::Kind::Int:
            return llvm::Type::getInt64Ty(context_);
        case ast::PrimitiveType::Kind::Float:
            return llvm::Type::getDoubleTy(context_);
        case ast::PrimitiveType::Kind::Bool:
            return llvm::Type::getInt1Ty(context_);
        case ast::PrimitiveType::Kind::String:
            // String is a pointer to runtime string type
            // For now, use i8* (will be replaced with proper runtime type)
            return llvm::PointerType::get(llvm::Type::getInt8Ty(context_), 0);
        case ast::PrimitiveType::Kind::Unit:
            return llvm::Type::getVoidTy(context_);
        default:
            return nullptr;
    }
}

llvm::Type* IRGenerator::convertArrayType(ast::ArrayType* type) {
    // Arrays are represented as pointers to runtime array objects
    // Runtime arrays are reference-counted objects, so we use i8* as opaque pointer
    // TODO: In the future, we could use a proper struct type for arrays
    return llvm::PointerType::get(llvm::Type::getInt8Ty(context_), 0);
}

void IRGenerator::enterFunction(llvm::Function* func) {
    currentFunction_ = func;
    localVars_.clear();
}

void IRGenerator::exitFunction() {
    currentFunction_ = nullptr;
    localVars_.clear();
    arrayMetadata_.clear();
}

llvm::Value* IRGenerator::getVariable(const std::string& name) {
    auto it = localVars_.find(name);
    if (it != localVars_.end()) {
        return it->second;
    }
    return nullptr;
}

void IRGenerator::setVariable(const std::string& name, llvm::Value* value) {
    localVars_[name] = value;
}

llvm::Value* IRGenerator::evaluateExpr(ast::Expr* expr) {
    if (!expr) {
        return nullptr;
    }
    expr->accept(*this);
    return currentValue_;
}

llvm::Value* IRGenerator::evaluateLiteral(ast::LiteralExpr* expr) {
    std::string valueStr = expr->getValue();
    
    switch (expr->getType()) {
        case ast::LiteralExpr::LiteralType::Int: {
            int64_t value = std::stoll(valueStr);
            return llvm::ConstantInt::get(context_, llvm::APInt(64, value, true));
        }
        case ast::LiteralExpr::LiteralType::Float: {
            double value = std::stod(valueStr);
            return llvm::ConstantFP::get(context_, llvm::APFloat(value));
        }
        case ast::LiteralExpr::LiteralType::Bool: {
            bool value = (valueStr == "true");
            return llvm::ConstantInt::get(context_, llvm::APInt(1, value ? 1 : 0));
        }
        case ast::LiteralExpr::LiteralType::String: {
            // String literals need runtime allocation
            // For now, create a global string constant
            llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context_, valueStr);
            llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
                *module_,
                strConstant->getType(),
                true, // isConstant
                llvm::GlobalValue::PrivateLinkage,
                strConstant,
                ".str"
            );
            return builder_->CreateBitCast(globalStr, llvm::PointerType::get(llvm::Type::getInt8Ty(context_), 0));
        }
        default:
            return nullptr;
    }
}

llvm::Value* IRGenerator::evaluateBinary(ast::BinaryExpr* expr) {
    llvm::Value* left = evaluateExpr(expr->getLeft());
    llvm::Value* right = evaluateExpr(expr->getRight());
    
    if (!left || !right) {
        return nullptr;
    }
    
    // Get types
    llvm::Type* leftType = left->getType();
    llvm::Type* rightType = right->getType();
    
    switch (expr->getOp()) {
        case ast::BinaryExpr::Op::Add:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateAdd(left, right, "addtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                // Promote to float if either operand is float
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFAdd(left, right, "addtmp");
            }
            break;
        case ast::BinaryExpr::Op::Sub:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateSub(left, right, "subtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFSub(left, right, "subtmp");
            }
            break;
        case ast::BinaryExpr::Op::Mul:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateMul(left, right, "multmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFMul(left, right, "multmp");
            }
            break;
        case ast::BinaryExpr::Op::Div:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateSDiv(left, right, "divtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFDiv(left, right, "divtmp");
            }
            break;
        case ast::BinaryExpr::Op::Eq:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpEQ(left, right, "eqtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOEQ(left, right, "eqtmp");
            }
            break;
        case ast::BinaryExpr::Op::Ne:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpNE(left, right, "netmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpONE(left, right, "netmp");
            }
            break;
        case ast::BinaryExpr::Op::Lt:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSLT(left, right, "lttmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOLT(left, right, "lttmp");
            }
            break;
        case ast::BinaryExpr::Op::Le:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSLE(left, right, "letmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOLE(left, right, "letmp");
            }
            break;
        case ast::BinaryExpr::Op::Gt:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSGT(left, right, "gttmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOGT(left, right, "gttmp");
            }
            break;
        case ast::BinaryExpr::Op::Ge:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSGE(left, right, "getmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOGE(left, right, "getmp");
            }
            break;
        case ast::BinaryExpr::Op::Mod:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateSRem(left, right, "modtmp");
            } else {
                errorReporter_.error(
                    expr->getLocation(),
                    "Modulo operator only supported for integers"
                );
                return nullptr;
            }
        case ast::BinaryExpr::Op::And:
            // Logical AND - use short-circuit evaluation
            return evaluateShortCircuitAnd(expr);
        case ast::BinaryExpr::Op::Or:
            // Logical OR - use short-circuit evaluation
            return evaluateShortCircuitOr(expr);
        default:
            errorReporter_.error(
                expr->getLocation(),
                "Unsupported binary operator"
            );
            return nullptr;
    }
    
    return nullptr;
}

llvm::Value* IRGenerator::evaluateUnary(ast::UnaryExpr* expr) {
    llvm::Value* operand = evaluateExpr(expr->getOperand());
    if (!operand) {
        return nullptr;
    }
    
    switch (expr->getOp()) {
        case ast::UnaryExpr::Op::Neg:
            if (operand->getType()->isIntegerTy()) {
                return builder_->CreateNeg(operand, "negtmp");
            } else if (operand->getType()->isFloatingPointTy()) {
                return builder_->CreateFNeg(operand, "negtmp");
            }
            break;
        case ast::UnaryExpr::Op::Not:
            if (operand->getType()->isIntegerTy(1)) {
                return builder_->CreateNot(operand, "nottmp");
            }
            break;
        default:
            errorReporter_.error(
                expr->getLocation(),
                "Unsupported unary operator"
            );
            return nullptr;
    }
    
    return nullptr;
}

llvm::Value* IRGenerator::evaluateVariable(ast::VariableExpr* expr) {
    std::string name = expr->getName();
    llvm::Value* alloca = getVariable(name);
    
    if (!alloca) {
        errorReporter_.error(
            expr->getLocation(),
            "Undefined variable: " + name
        );
        return nullptr;
    }
    
    // Check if this variable has array metadata
    auto arrayIt = arrayMetadata_.find(alloca);
    if (arrayIt != arrayMetadata_.end()) {
        // This is an array variable - return the alloca directly
        // (arrays are represented as pointers, so the alloca itself is the value)
        // Cast to i8* to match array representation
        return builder_->CreateBitCast(
            alloca,
            llvm::PointerType::get(llvm::Type::getInt8Ty(context_), 0),
            name + "_ptr"
        );
    }
    
    // Load value from alloca
    // In newer LLVM versions, CreateLoad takes the pointer type directly
    return builder_->CreateLoad(alloca->getType(), alloca, name);
}

llvm::Value* IRGenerator::evaluateFunctionCall(ast::FunctionCallExpr* expr) {
    std::string funcName = expr->getName();
    
    // Look up function in module
    llvm::Function* func = module_->getFunction(funcName);
    if (!func) {
        errorReporter_.error(
            expr->getLocation(),
            "Undefined function: " + funcName
        );
        return nullptr;
    }
    
    // Evaluate arguments
    std::vector<llvm::Value*> args;
    const auto& argExprs = expr->getArgs();
    for (const auto& argExpr : argExprs) {
        llvm::Value* argValue = evaluateExpr(argExpr.get());
        if (!argValue) {
            return nullptr;
        }
        args.push_back(argValue);
    }
    
    // Create call instruction
    return builder_->CreateCall(func, args, "calltmp");
}

void IRGenerator::generateStatement(ast::Stmt* stmt) {
    if (!stmt) {
        return;
    }
    
    if (auto* varDecl = dynamic_cast<ast::VariableDecl*>(stmt)) {
        generateVariableDecl(varDecl);
    } else if (auto* returnStmt = dynamic_cast<ast::ReturnStmt*>(stmt)) {
        generateReturnStmt(returnStmt);
    } else if (auto* exprStmt = dynamic_cast<ast::ExprStmt*>(stmt)) {
        generateExprStmt(exprStmt);
    }
}

void IRGenerator::generateVariableDecl(ast::VariableDecl* stmt) {
    // Evaluate initializer expression (if present)
    llvm::Value* initValue = nullptr;
    if (stmt->getInitializer()) {
        initValue = evaluateExpr(stmt->getInitializer());
        if (!initValue) {
            errorReporter_.error(
                stmt->getLocation(),
                "Failed to evaluate variable initializer"
            );
            return;
        }
    } else {
        // No initializer - use default value based on type
        // This is mainly for mutable variables that can be assigned later
        if (stmt->getType()) {
            llvm::Type* varType = convertType(stmt->getType());
            if (varType) {
                if (varType->isIntegerTy(64)) {
                    initValue = llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
                } else if (varType->isDoubleTy()) {
                    initValue = llvm::ConstantFP::get(context_, llvm::APFloat(0.0));
                } else if (varType->isIntegerTy(1)) {
                    initValue = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
                } else {
                    errorReporter_.error(
                        stmt->getLocation(),
                        "Variable declaration missing initializer"
                    );
                    return;
                }
            } else {
                errorReporter_.error(
                    stmt->getLocation(),
                    "Variable declaration missing initializer and type"
                );
                return;
            }
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "Variable declaration missing initializer"
            );
            return;
        }
    }
    
    // Allocate space for variable (alloca instruction)
    llvm::Type* varType = initValue->getType();
    llvm::AllocaInst* alloca = builder_->CreateAlloca(varType, nullptr, stmt->getName());
    
    // Store initial value
    builder_->CreateStore(initValue, alloca);
    
    // Register variable in symbol table (store the alloca, not the value)
    // When accessing, we'll load from the alloca
    setVariable(stmt->getName(), alloca);
    
    // If this is an array variable, preserve metadata
    // Check if initValue has array metadata
    auto arrayIt = arrayMetadata_.find(initValue);
    if (arrayIt != arrayMetadata_.end()) {
        // Copy metadata for the stored value
        // Note: The stored value is the alloca, but we need to track the original array
        // For now, we'll track it by the alloca pointer
        ArrayMetadata metadata = arrayIt->second;
        // The metadata.alloca points to the original array allocation
        // We need to update it to point to our variable's alloca
        metadata.alloca = alloca;
        arrayMetadata_[alloca] = metadata;
    }
}

void IRGenerator::generateReturnStmt(ast::ReturnStmt* stmt) {
    ast::Expr* returnExpr = stmt->getValue();
    if (returnExpr) {
        llvm::Value* returnValue = evaluateExpr(returnExpr);
        if (returnValue) {
            builder_->CreateRet(returnValue);
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "Failed to evaluate return expression"
            );
        }
    } else {
        // Void return (no value)
        builder_->CreateRetVoid();
    }
}

void IRGenerator::generateExprStmt(ast::ExprStmt* stmt) {
    // Evaluate expression but discard result
    evaluateExpr(stmt->getExpr());
}

void IRGenerator::visitIfStmt(ast::IfStmt* node) {
    generateIfStmt(node);
}

void IRGenerator::visitWhileStmt(ast::WhileStmt* node) {
    generateWhileStmt(node);
}

void IRGenerator::generateIfStmt(ast::IfStmt* stmt) {
    if (!stmt || !currentFunction_) {
        return;
    }
    
    // Evaluate condition
    llvm::Value* condValue = evaluateExpr(stmt->getCondition());
    if (!condValue) {
        return;
    }
    
    // Convert condition to boolean if needed
    llvm::Type* condType = condValue->getType();
    if (!condType->isIntegerTy(1)) {
        // Convert to boolean: compare with zero/false
        if (condType->isIntegerTy()) {
            condValue = builder_->CreateICmpNE(
                condValue,
                llvm::ConstantInt::get(condType, 0),
                "ifcond"
            );
        } else if (condType->isFloatingPointTy()) {
            condValue = builder_->CreateFCmpONE(
                condValue,
                llvm::ConstantFP::get(condType, 0.0),
                "ifcond"
            );
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "If condition must be a boolean, integer, or floating-point value"
            );
            return;
        }
    }
    
    // Get current function and basic blocks
    llvm::Function* func = currentFunction_;
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context_, "then", func);
    llvm::BasicBlock* elseBB = stmt->hasElse() 
        ? llvm::BasicBlock::Create(context_, "else", func)
        : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "ifcont", func);
    
    // Create conditional branch
    if (elseBB) {
        builder_->CreateCondBr(condValue, thenBB, elseBB);
    } else {
        builder_->CreateCondBr(condValue, thenBB, mergeBB);
    }
    
    // Generate then branch
    builder_->SetInsertPoint(thenBB);
    generateStatement(stmt->getThenBranch());
    // If then branch doesn't terminate (no return), branch to merge
    if (!thenBB->getTerminator()) {
        builder_->CreateBr(mergeBB);
    }
    
    // Generate else branch if present
    if (elseBB) {
        builder_->SetInsertPoint(elseBB);
        generateStatement(stmt->getElseBranch());
        // If else branch doesn't terminate, branch to merge
        if (!elseBB->getTerminator()) {
            builder_->CreateBr(mergeBB);
        }
    }
    
    // Set insert point to merge block
    builder_->SetInsertPoint(mergeBB);
}

void IRGenerator::generateWhileStmt(ast::WhileStmt* stmt) {
    if (!stmt || !currentFunction_) {
        return;
    }
    
    llvm::Function* func = currentFunction_;
    
    // Create basic blocks
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context_, "whilecond", func);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context_, "whilebody", func);
    llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(context_, "whileend", func);
    
    // Branch to condition block
    builder_->CreateBr(condBB);
    
    // Generate condition check
    builder_->SetInsertPoint(condBB);
    llvm::Value* condValue = evaluateExpr(stmt->getCondition());
    if (!condValue) {
        return;
    }
    
    // Convert condition to boolean if needed
    llvm::Type* condType = condValue->getType();
    if (!condType->isIntegerTy(1)) {
        if (condType->isIntegerTy()) {
            condValue = builder_->CreateICmpNE(
                condValue,
                llvm::ConstantInt::get(condType, 0),
                "whilecond"
            );
        } else if (condType->isFloatingPointTy()) {
            condValue = builder_->CreateFCmpONE(
                condValue,
                llvm::ConstantFP::get(condType, 0.0),
                "whilecond"
            );
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "While condition must be a boolean, integer, or floating-point value"
            );
            return;
        }
    }
    
    // Branch based on condition
    builder_->CreateCondBr(condValue, bodyBB, afterBB);
    
    // Generate loop body
    builder_->SetInsertPoint(bodyBB);
    generateStatement(stmt->getBody());
    // If body doesn't terminate, branch back to condition
    if (!bodyBB->getTerminator()) {
        builder_->CreateBr(condBB);
    }
    
    // Set insert point to after loop
    builder_->SetInsertPoint(afterBB);
}

llvm::Value* IRGenerator::evaluateShortCircuitAnd(ast::BinaryExpr* expr) {
    if (!expr || !currentFunction_) {
        return nullptr;
    }
    
    llvm::Function* func = currentFunction_;
    llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
    
    // Create basic blocks
    llvm::BasicBlock* rightBB = llvm::BasicBlock::Create(context_, "andright", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "andmerge", func);
    
    // Evaluate left operand
    llvm::Value* left = evaluateExpr(expr->getLeft());
    if (!left) {
        return nullptr;
    }
    
    // Convert left to boolean if needed
    llvm::Type* leftType = left->getType();
    if (!leftType->isIntegerTy(1)) {
        if (leftType->isIntegerTy()) {
            left = builder_->CreateICmpNE(
                left,
                llvm::ConstantInt::get(leftType, 0),
                "andleftbool"
            );
        } else if (leftType->isFloatingPointTy()) {
            left = builder_->CreateFCmpONE(
                left,
                llvm::ConstantFP::get(leftType, 0.0),
                "andleftbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical AND requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    // Branch: if left is false, go to merge (result is false)
    // Otherwise, evaluate right operand
    builder_->CreateCondBr(left, rightBB, mergeBB);
    
    // Evaluate right operand
    builder_->SetInsertPoint(rightBB);
    llvm::Value* right = evaluateExpr(expr->getRight());
    if (!right) {
        return nullptr;
    }
    
    // Convert right to boolean if needed
    llvm::Type* rightType = right->getType();
    if (!rightType->isIntegerTy(1)) {
        if (rightType->isIntegerTy()) {
            right = builder_->CreateICmpNE(
                right,
                llvm::ConstantInt::get(rightType, 0),
                "andrightbool"
            );
        } else if (rightType->isFloatingPointTy()) {
            right = builder_->CreateFCmpONE(
                right,
                llvm::ConstantFP::get(rightType, 0.0),
                "andrightbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical AND requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    builder_->CreateBr(mergeBB);
    
    // Merge block: phi node selects result
    builder_->SetInsertPoint(mergeBB);
    llvm::PHINode* phi = builder_->CreatePHI(llvm::Type::getInt1Ty(context_), 2, "andtmp");
    phi->addIncoming(llvm::ConstantInt::getFalse(context_), currentBB);
    phi->addIncoming(right, rightBB);
    
    return phi;
}

llvm::Value* IRGenerator::evaluateShortCircuitOr(ast::BinaryExpr* expr) {
    if (!expr || !currentFunction_) {
        return nullptr;
    }
    
    llvm::Function* func = currentFunction_;
    llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
    
    // Create basic blocks
    llvm::BasicBlock* rightBB = llvm::BasicBlock::Create(context_, "orright", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "ormerge", func);
    
    // Evaluate left operand
    llvm::Value* left = evaluateExpr(expr->getLeft());
    if (!left) {
        return nullptr;
    }
    
    // Convert left to boolean if needed
    llvm::Type* leftType = left->getType();
    if (!leftType->isIntegerTy(1)) {
        if (leftType->isIntegerTy()) {
            left = builder_->CreateICmpNE(
                left,
                llvm::ConstantInt::get(leftType, 0),
                "orleftbool"
            );
        } else if (leftType->isFloatingPointTy()) {
            left = builder_->CreateFCmpONE(
                left,
                llvm::ConstantFP::get(leftType, 0.0),
                "orleftbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical OR requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    // Branch: if left is true, go to merge (result is true)
    // Otherwise, evaluate right operand
    builder_->CreateCondBr(left, mergeBB, rightBB);
    
    // Evaluate right operand
    builder_->SetInsertPoint(rightBB);
    llvm::Value* right = evaluateExpr(expr->getRight());
    if (!right) {
        return nullptr;
    }
    
    // Convert right to boolean if needed
    llvm::Type* rightType = right->getType();
    if (!rightType->isIntegerTy(1)) {
        if (rightType->isIntegerTy()) {
            right = builder_->CreateICmpNE(
                right,
                llvm::ConstantInt::get(rightType, 0),
                "orrightbool"
            );
        } else if (rightType->isFloatingPointTy()) {
            right = builder_->CreateFCmpONE(
                right,
                llvm::ConstantFP::get(rightType, 0.0),
                "orrightbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical OR requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    builder_->CreateBr(mergeBB);
    
    // Merge block: phi node selects result
    builder_->SetInsertPoint(mergeBB);
    llvm::PHINode* phi = builder_->CreatePHI(llvm::Type::getInt1Ty(context_), 2, "ortmp");
    phi->addIncoming(left, currentBB);
    phi->addIncoming(right, rightBB);
    
    return phi;
}

void IRGenerator::visitAssignmentStmt(ast::AssignmentStmt* node) {
    generateAssignmentStmt(node);
}

llvm::Value* IRGenerator::evaluateArrayLiteral(ast::ArrayLiteralExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    const auto& elements = expr->getElements();
    
    // For empty arrays, we need element type from context
    // For now, if empty, we can't determine type - this will be handled by type checker
    if (elements.empty()) {
        errorReporter_.error(
            expr->getLocation(),
            "Empty array literals require explicit type annotation"
        );
        return nullptr;
    }
    
    // Evaluate all element expressions
    std::vector<llvm::Value*> elementValues;
    llvm::Type* elementType = nullptr;
    
    for (const auto& element : elements) {
        llvm::Value* elemValue = evaluateExpr(element.get());
        if (!elemValue) {
            return nullptr;
        }
        
        // Determine element type from first element
        if (!elementType) {
            elementType = elemValue->getType();
        }
        
        elementValues.push_back(elemValue);
    }
    
    if (!elementType) {
        errorReporter_.error(
            expr->getLocation(),
            "Failed to determine array element type"
        );
        return nullptr;
    }
    
    // Arrays are represented as pointers to runtime array objects (i8*)
    // For now, we'll create a simple implementation that:
    // 1. Allocates space for array elements on the stack (temporary solution)
    // 2. Stores elements into the allocated space
    // 3. Returns a pointer to the array
    
    // TODO: Proper implementation would:
    // 1. Declare runtime makeArray functions
    // 2. Call runtime allocation functions
    // 3. Store elements using runtime methods
    
    // Temporary: Create a constant array on the stack
    // This is a simplified approach - full implementation needs runtime integration
    size_t arraySize = elementValues.size();
    
    // Create array type: [N x elementType]
    llvm::ArrayType* arrayType = llvm::ArrayType::get(elementType, arraySize);
    
    // Allocate array on stack
    llvm::AllocaInst* arrayAlloca = builder_->CreateAlloca(arrayType, nullptr, "arrtmp");
    
    // Store each element
    for (size_t i = 0; i < arraySize; ++i) {
        // Get pointer to element i
        llvm::Value* indices[] = {
            llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
            llvm::ConstantInt::get(context_, llvm::APInt(64, i))
        };
        llvm::Value* elemPtr = builder_->CreateGEP(arrayType, arrayAlloca, indices, "arrelem");
        
        // Store element value
        builder_->CreateStore(elementValues[i], elemPtr);
    }
    
    // Store metadata for this array (keyed by the alloca, not the bitcast)
    ArrayMetadata metadata;
    metadata.elementType = elementType;
    metadata.size = arraySize;
    metadata.alloca = arrayAlloca;
    
    // Store metadata keyed by alloca
    arrayMetadata_[arrayAlloca] = metadata;
    
    // Return pointer to array (cast to i8* for now)
    llvm::Value* arrayPtr = builder_->CreateBitCast(
        arrayAlloca,
        llvm::PointerType::get(llvm::Type::getInt8Ty(context_), 0),
        "arrptr"
    );
    
    // Also store metadata keyed by the bitcast pointer for direct array literals
    arrayMetadata_[arrayPtr] = metadata;
    
    return arrayPtr;
}

llvm::Value* IRGenerator::evaluateArrayIndex(ast::ArrayIndexExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    // Evaluate array expression
    llvm::Value* arrayValue = evaluateExpr(expr->getArray());
    if (!arrayValue) {
        return nullptr;
    }
    
    // Evaluate index expression
    llvm::Value* indexValue = evaluateExpr(expr->getIndex());
    if (!indexValue) {
        return nullptr;
    }
    
    // Ensure index is integer type
    if (!indexValue->getType()->isIntegerTy()) {
        // Convert to integer if needed
        if (indexValue->getType()->isFloatingPointTy()) {
            indexValue = builder_->CreateFPToSI(
                indexValue,
                llvm::Type::getInt64Ty(context_),
                "indextmp"
            );
        } else {
            errorReporter_.error(
                expr->getIndex()->getLocation(),
                "Array index must be an integer or floating-point number"
            );
            return nullptr;
        }
    }
    
    // Ensure index is 64-bit
    if (indexValue->getType()->getIntegerBitWidth() != 64) {
        indexValue = builder_->CreateSExt(
            indexValue,
            llvm::Type::getInt64Ty(context_),
            "indexext"
        );
    }
    
    // Look up array metadata
    // arrayValue might be a direct array pointer (i8*) or an alloca
    auto it = arrayMetadata_.find(arrayValue);
    
    // If not found, arrayValue might be a bitcast from an alloca
    // Try to find the original alloca by checking local variables
    if (it == arrayMetadata_.end()) {
        // Check if arrayValue is a bitcast that came from a variable
        // We need to trace back: if arrayValue is i8*, it might have been
        // created from an alloca stored in a variable
        for (const auto& varPair : localVars_) {
            llvm::Value* varAlloca = varPair.second;
            auto varIt = arrayMetadata_.find(varAlloca);
            if (varIt != arrayMetadata_.end()) {
                // Found array variable - use its metadata
                it = varIt;
                break;
            }
        }
    }
    
    if (it == arrayMetadata_.end()) {
        // Array not found in metadata
        errorReporter_.error(
            expr->getLocation(),
            "Array indexing: array type information not available"
        );
        return nullptr;
    }
    
    ArrayMetadata& metadata = it->second;
    llvm::Type* elementType = metadata.elementType;
    size_t arraySize = metadata.size;
    llvm::AllocaInst* arrayAlloca = metadata.alloca;
    
    // Get current basic block
    llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
    
    // Bounds checking
    llvm::BasicBlock* inBoundsBB = llvm::BasicBlock::Create(context_, "inbounds", currentFunction_);
    llvm::BasicBlock* outOfBoundsBB = llvm::BasicBlock::Create(context_, "outofbounds", currentFunction_);
    llvm::BasicBlock* continueBB = llvm::BasicBlock::Create(context_, "boundscont", currentFunction_);
    
    // Check: index >= 0 && index < size
    llvm::Value* zero = llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
    llvm::Value* sizeConst = llvm::ConstantInt::get(context_, llvm::APInt(64, arraySize));
    
    llvm::Value* geZero = builder_->CreateICmpSGE(indexValue, zero, "gezero");
    llvm::Value* ltSize = builder_->CreateICmpSLT(indexValue, sizeConst, "ltsize");
    llvm::Value* inBounds = builder_->CreateAnd(geZero, ltSize, "inbounds");
    
    builder_->CreateCondBr(inBounds, inBoundsBB, outOfBoundsBB);
    
    // In bounds: load element
    builder_->SetInsertPoint(inBoundsBB);
    
    // Cast array pointer back to array type
    llvm::ArrayType* arrayType = llvm::ArrayType::get(elementType, arraySize);
    llvm::Value* typedPtr = builder_->CreateBitCast(
        arrayAlloca,
        arrayType->getPointerTo(),
        "typedarr"
    );
    
    // Get pointer to element
    llvm::Value* indices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        indexValue
    };
    llvm::Value* elemPtr = builder_->CreateGEP(arrayType, typedPtr, indices, "arrelem");
    
    // Load element value
    llvm::Value* elemValue = builder_->CreateLoad(elementType, elemPtr, "arrelemval");
    builder_->CreateBr(continueBB);
    
    // Out of bounds: error (for now, return default value)
    builder_->SetInsertPoint(outOfBoundsBB);
    llvm::Value* defaultValue = nullptr;
    if (elementType->isIntegerTy(64)) {
        defaultValue = llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
    } else if (elementType->isDoubleTy()) {
        defaultValue = llvm::ConstantFP::get(context_, llvm::APFloat(0.0));
    } else if (elementType->isIntegerTy(1)) {
        defaultValue = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
    } else {
        errorReporter_.error(
            expr->getLocation(),
            "Array index out of bounds"
        );
        return nullptr;
    }
    builder_->CreateBr(continueBB);
    
    // Continue: phi node to merge results
    builder_->SetInsertPoint(continueBB);
    llvm::PHINode* phi = builder_->CreatePHI(elementType, 2, "arrindex");
    phi->addIncoming(elemValue, inBoundsBB);
    phi->addIncoming(defaultValue, outOfBoundsBB);
    
    return phi;
}

void IRGenerator::generateAssignmentStmt(ast::AssignmentStmt* stmt) {
    if (!stmt) {
        return;
    }
    
    // Evaluate the value expression
    llvm::Value* value = evaluateExpr(stmt->getValue());
    if (!value) {
        errorReporter_.error(
            stmt->getLocation(),
            "Failed to evaluate assignment value"
        );
        return;
    }
    
    // Get the target (must be a variable expression)
    ast::Expr* targetExpr = stmt->getTarget();
    if (auto* varExpr = dynamic_cast<ast::VariableExpr*>(targetExpr)) {
        std::string varName = varExpr->getName();
        llvm::Value* alloca = getVariable(varName);
        
        if (!alloca) {
            errorReporter_.error(
                stmt->getLocation(),
                "Undefined variable: " + varName
            );
            return;
        }
        
        // Store the value to the variable's alloca
        builder_->CreateStore(value, alloca);
    } else {
        errorReporter_.error(
            stmt->getLocation(),
            "Assignment target must be a variable"
        );
    }
}

} // namespace ir
} // namespace first
