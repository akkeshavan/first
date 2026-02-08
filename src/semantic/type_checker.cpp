#include "first/semantic/type_checker.h"
#include "first/ast/declarations.h"
#include "first/ast/statements.h"
#include "first/ast/expressions.h"
#include "first/ast/match_case.h"
#include "first/ast/patterns.h"
#include "first/semantic/module_resolver.h"
#include <sstream>
#include <set>

namespace first {
namespace semantic {

static ast::Type* getEffectiveType(ast::Type* type);

bool TypeChecker::check(ast::Program* program) {
    if (!program) {
        errorReporter_.error(SourceLocation(), "Cannot type check null program");
        return false;
    }
    
    // Track the current program so we can consult its imports when resolving
    // symbols via the module resolver (for multi-module calls, etc.).
    currentProgram_ = program;
    checkProgram(program);
    currentProgram_ = nullptr;
    
    // Return true if no errors (ErrorReporter tracks errors)
    return true; // callers check errorReporter_ for actual failures
}

void TypeChecker::checkProgram(ast::Program* program) {
    typeDecls_.clear();
    typeDeclParams_.clear();
    constructorToADT_.clear();
    currentFunction_ = nullptr;
    currentInteraction_ = nullptr;
    for (const auto& typeDecl : program->getTypeDecls()) {
        if (!typeDecl || !typeDecl->getType()) continue;
        const std::string& name = typeDecl->getTypeName();
        ast::Type* ty = typeDecl->getType();
        typeDecls_[name] = ty;
        if (typeDecl->isGeneric()) {
            typeDeclParams_[name] = typeDecl->getTypeParams();
        }
        if (auto* adt = dynamic_cast<ast::ADTType*>(ty)) {
            for (const auto& c : adt->getConstructors()) {
                if (c) constructorToADT_[c->getName()] = adt;
            }
        }
    }
    // First pass: collect all top-level declarations into symbol table
    for (const auto& func : program->getFunctions()) {
        auto symbol = std::make_unique<FunctionSymbol>(
            func->getName(), func->getLocation(), func.get()
        );
        if (!symbolTable_.insert(std::move(symbol))) {
            errorReporter_.error(
                func->getLocation(),
                "Duplicate function declaration: " + func->getName()
            );
        }
    }
    
    for (const auto& interaction : program->getInteractions()) {
        auto symbol = std::make_unique<InteractionSymbol>(
            interaction->getName(), interaction->getLocation(), interaction.get()
        );
        if (!symbolTable_.insert(std::move(symbol))) {
            errorReporter_.error(
                interaction->getLocation(),
                "Duplicate interaction declaration: " + interaction->getName()
            );
        }
    }
    
    // Second pass: type check all functions and interactions
    for (const auto& func : program->getFunctions()) {
        checkFunction(func.get());
    }
    
    for (const auto& interaction : program->getInteractions()) {
        checkInteraction(interaction.get());
    }
}

void TypeChecker::checkFunction(ast::FunctionDecl* func) {
    if (!func) return;

    // Generic params must appear in at least one parameter type (no separate return-only generic)
    checkGenericParamsAppearInParameterTypes(func);
    
    currentFunction_ = func;
    currentInteraction_ = nullptr;
    // Enter function scope
    symbolTable_.enterScope();
    
    // Add parameters to symbol table
    for (const auto& param : func->getParameters()) {
        // Create a copy of the parameter type
        std::unique_ptr<ast::Type> typeCopy = copyType(param->getType());
        if (!typeCopy) {
            errorReporter_.error(
                param->getLocation(),
                "Invalid type for parameter: " + param->getName()
            );
            continue;
        }
        
        auto varSymbol = std::make_unique<VariableSymbol>(
            param->getName(),
            param->getLocation(),
            std::move(typeCopy)
        );
        symbolTable_.insert(std::move(varSymbol));
    }
    
    // Type check function body
    for (const auto& stmt : func->getBody()) {
        checkStatement(stmt.get());
    }
    
    // Exit function scope
    symbolTable_.exitScope();
    currentFunction_ = nullptr;
}

void TypeChecker::checkInteraction(ast::InteractionDecl* interaction) {
    if (!interaction) return;

    // Generic params must appear in at least one parameter type (no separate return-only generic)
    checkGenericParamsAppearInParameterTypes(interaction);
    
    currentFunction_ = nullptr;
    currentInteraction_ = interaction;
    // Similar to function checking
    symbolTable_.enterScope();
    
    for (const auto& param : interaction->getParameters()) {
        // Create a copy of the parameter type
        std::unique_ptr<ast::Type> typeCopy = copyType(param->getType());
        if (!typeCopy) {
            errorReporter_.error(
                param->getLocation(),
                "Invalid type for parameter: " + param->getName()
            );
            continue;
        }
        
        auto varSymbol = std::make_unique<VariableSymbol>(
            param->getName(),
            param->getLocation(),
            std::move(typeCopy)
        );
        symbolTable_.insert(std::move(varSymbol));
    }
    
    for (const auto& stmt : interaction->getBody()) {
        checkStatement(stmt.get());
    }
    
    symbolTable_.exitScope();
    currentInteraction_ = nullptr;
}

void TypeChecker::collectTypeParamNamesInType(ast::Type* type, std::set<std::string>& out) {
    if (!type) return;
    if (auto* gen = dynamic_cast<ast::GenericType*>(type)) {
        out.insert(gen->getName());
        return;
    }
    if (auto* arr = dynamic_cast<ast::ArrayType*>(type)) {
        collectTypeParamNamesInType(arr->getElementType(), out);
        return;
    }
    if (auto* param = dynamic_cast<ast::ParameterizedType*>(type)) {
        for (const auto& arg : param->getTypeArgs()) {
            collectTypeParamNamesInType(arg.get(), out);
        }
        return;
    }
    if (auto* func = dynamic_cast<ast::FunctionType*>(type)) {
        for (const auto& pt : func->getParamTypes()) {
            collectTypeParamNamesInType(pt.get(), out);
        }
        collectTypeParamNamesInType(func->getReturnType(), out);
        return;
    }
    if (auto* rec = dynamic_cast<ast::RecordType*>(type)) {
        for (const auto& field : rec->getFields()) {
            collectTypeParamNamesInType(field->getType(), out);
        }
        return;
    }
    if (auto* ref = dynamic_cast<ast::RefinementType*>(type)) {
        collectTypeParamNamesInType(ref->getBaseType(), out);
        return;
    }
    if (auto* idx = dynamic_cast<ast::IndexedType*>(type)) {
        collectTypeParamNamesInType(idx->getBaseType(), out);
        return;
    }
    if (auto* dep = dynamic_cast<ast::DependentFunctionType*>(type)) {
        collectTypeParamNamesInType(dep->getParamType(), out);
        collectTypeParamNamesInType(dep->getReturnType(), out);
        return;
    }
    if (auto* sigma = dynamic_cast<ast::DependentPairType*>(type)) {
        collectTypeParamNamesInType(sigma->getVarType(), out);
        collectTypeParamNamesInType(sigma->getBodyType(), out);
        return;
    }
    if (auto* forall = dynamic_cast<ast::ForallType*>(type)) {
        collectTypeParamNamesInType(forall->getBodyType(), out);
        return;
    }
    if (auto* ex = dynamic_cast<ast::ExistentialType*>(type)) {
        collectTypeParamNamesInType(ex->getVarType(), out);
        collectTypeParamNamesInType(ex->getBodyType(), out);
        return;
    }
    // PrimitiveType, ADTType, etc. have no nested type parameters
}

void TypeChecker::checkGenericParamsAppearInParameterTypes(ast::FunctionDecl* func) {
    const auto& genericParams = func->getGenericParams();
    if (genericParams.empty()) return;
    std::set<std::string> namesInParams;
    for (const auto& param : func->getParameters()) {
        if (param->getType()) {
            collectTypeParamNamesInType(param->getType(), namesInParams);
        }
    }
    for (const auto& gp : genericParams) {
        const std::string& name = gp.name;
        if (namesInParams.count(name) == 0) {
            errorReporter_.error(
                func->getLocation(),
                "Generic type parameter '" + name + "' must appear in at least one parameter type (return-only generic not allowed)"
            );
        }
    }
}

void TypeChecker::checkGenericParamsAppearInParameterTypes(ast::InteractionDecl* interaction) {
    const auto& genericParams = interaction->getGenericParams();
    if (genericParams.empty()) return;
    std::set<std::string> namesInParams;
    for (const auto& param : interaction->getParameters()) {
        if (param->getType()) {
            collectTypeParamNamesInType(param->getType(), namesInParams);
        }
    }
    for (const auto& gp : genericParams) {
        const std::string& name = gp.name;
        if (namesInParams.count(name) == 0) {
            errorReporter_.error(
                interaction->getLocation(),
                "Generic type parameter '" + name + "' must appear in at least one parameter type (return-only generic not allowed)"
            );
        }
    }
}

bool TypeChecker::typeImplementsInterface(ast::Type* type, const std::string& interfaceName) {
    if (!type) return false;
    type = getEffectiveType(type);
    // Type parameter with constraint: if we're inside a function that declares T : Interface, then T implements Interface
    if (auto* gen = dynamic_cast<ast::GenericType*>(type)) {
        const std::string& name = gen->getName();
        if (currentFunction_) {
            for (const auto& gp : currentFunction_->getGenericParams()) {
                if (gp.name == name && gp.constraint == interfaceName)
                    return true;
            }
        }
        if (currentInteraction_) {
            for (const auto& gp : currentInteraction_->getGenericParams()) {
                if (gp.name == name && gp.constraint == interfaceName)
                    return true;
            }
        }
    }
    // Built-in Eq and Ord for Int, Float, Bool, String — no user implementation required
    if (interfaceName == "Eq" || interfaceName == "Ord") {
        if (auto* prim = dynamic_cast<ast::PrimitiveType*>(type)) {
            if (interfaceName == "Eq") {
                switch (prim->getKind()) {
                    case ast::PrimitiveType::Kind::Int:
                    case ast::PrimitiveType::Kind::Float:
                    case ast::PrimitiveType::Kind::Bool:
                    case ast::PrimitiveType::Kind::String:
                        return true;
                    default: break;
                }
            }
            if (interfaceName == "Ord") {
                switch (prim->getKind()) {
                    case ast::PrimitiveType::Kind::Int:
                    case ast::PrimitiveType::Kind::Float:
                    case ast::PrimitiveType::Kind::String:
                        return true;
                    default: break;
                }
            }
        }
    }
    // Built-in Iterator for Array<T> — for-in over arrays works without user implementation
    if (interfaceName == "Iterator") {
        if (auto* arr = dynamic_cast<ast::ArrayType*>(type)) {
            return true;  // Array<T> implements Iterator<T>
        }
        if (auto* param = dynamic_cast<ast::ParameterizedType*>(type)) {
            if (param->getBaseName() == "Array" && param->getTypeArgs().size() == 1) {
                return true;  // Array<T> implements Iterator<T>
            }
        }
    }
    // User-defined implementations from the program and from imported modules (e.g. Prelude)
    auto checkImpls = [this, type, &interfaceName](ast::Program* program) {
        if (!program) return false;
        for (const auto& impl : program->getImplementations()) {
            if (!impl || impl->getInterfaceName() != interfaceName) continue;
            const auto& typeArgs = impl->getTypeArgs();
            if (typeArgs.size() != 1) continue;
            if (typesEqual(typeArgs[0].get(), type)) return true;
        }
        return false;
    };
    if (currentProgram_ && checkImpls(currentProgram_)) return true;
    if (moduleResolver_) {
        for (const std::string& modName : moduleResolver_->getLoadedModuleNames()) {
            if (checkImpls(moduleResolver_->getModule(modName))) return true;
        }
    }
    return false;
}

void TypeChecker::checkStatement(ast::Stmt* stmt) {
    if (!stmt) return;
    
    if (auto* varDecl = dynamic_cast<ast::VariableDecl*>(stmt)) {
        // Type check variable declaration
        if (varDecl->getInitializer()) {
            ast::Type* initType = inferType(varDecl->getInitializer());
            if (varDecl->getType()) {
                // Type annotation provided - check compatibility
                if (!isAssignable(initType, varDecl->getType())) {
                    reportTypeError(
                        varDecl->getLocation(),
                        "Type mismatch in variable initialization",
                        initType,
                        varDecl->getType()
                    );
                }
            }
            // If no type annotation, infer from initializer
        }
        
        // Add variable to symbol table
        std::unique_ptr<ast::Type> varType;
        if (varDecl->getType()) {
            // Create a copy of the type
            varType = copyType(varDecl->getType());
            if (!varType) {
                errorReporter_.error(
                    varDecl->getLocation(),
                    "Invalid type annotation for variable: " + varDecl->getName()
                );
                return;
            }
        } else if (varDecl->getInitializer()) {
            ast::Type* inferred = inferType(varDecl->getInitializer());
            if (inferred) {
                // Create a copy
                varType = copyType(inferred);
                if (!varType) {
                    errorReporter_.error(
                        varDecl->getLocation(),
                        "Cannot infer type for variable: " + varDecl->getName()
                    );
                    return;
                }
            } else {
                errorReporter_.error(
                    varDecl->getLocation(),
                    "Cannot infer type for variable: " + varDecl->getName()
                );
                return;
            }
        } else {
            errorReporter_.error(
                varDecl->getLocation(),
                "Variable declaration must have type annotation or initializer"
            );
            return;
        }
        
        auto varSymbol = std::make_unique<VariableSymbol>(
            varDecl->getName(),
            varDecl->getLocation(),
            std::move(varType),
            varDecl->getMutability() == ast::VariableDecl::Mutability::Mutable
        );
        
        if (!symbolTable_.insert(std::move(varSymbol))) {
            errorReporter_.error(
                varDecl->getLocation(),
                "Duplicate variable declaration: " + varDecl->getName()
            );
        }
    } else if (auto* retStmt = dynamic_cast<ast::ReturnStmt*>(stmt)) {
        // Return statement type checking will be done in function context
        if (retStmt->getValue()) {
            inferType(retStmt->getValue());
        }
    } else if (auto* exprStmt = dynamic_cast<ast::ExprStmt*>(stmt)) {
        if (exprStmt->getExpr()) {
            inferType(exprStmt->getExpr());
        }
    } else if (auto* forIn = dynamic_cast<ast::ForInStmt*>(stmt)) {
        ast::Type* elementType = getIterableElementType(forIn->getIterable());
        if (!elementType) {
            errorReporter_.error(
                forIn->getIterable()->getLocation(),
                "for-in requires an iterable (range 1..n or Array<T>)"
            );
            return;
        }
        symbolTable_.enterScope();
        auto varType = copyType(elementType);
        if (varType) {
            symbolTable_.insert(std::make_unique<VariableSymbol>(
                forIn->getVariableName(),
                forIn->getLocation(),
                std::move(varType),
                false  // immutable in for-in
            ));
        }
        for (const auto& s : forIn->getBody()) {
            checkStatement(s.get());
        }
        symbolTable_.exitScope();
    }
}

ast::Type* TypeChecker::inferType(ast::Expr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    return inferExpression(expr);
}

ast::Type* TypeChecker::inferExpression(ast::Expr* expr) {
    if (auto* literal = dynamic_cast<ast::LiteralExpr*>(expr)) {
        return inferLiteral(literal);
    } else if (auto* binary = dynamic_cast<ast::BinaryExpr*>(expr)) {
        return inferBinary(binary);
    } else if (auto* unary = dynamic_cast<ast::UnaryExpr*>(expr)) {
        return inferUnary(unary);
    } else if (auto* variable = dynamic_cast<ast::VariableExpr*>(expr)) {
        return inferVariable(variable);
    } else if (auto* call = dynamic_cast<ast::FunctionCallExpr*>(expr)) {
        return inferFunctionCall(call);
    } else if (auto* constructor = dynamic_cast<ast::ConstructorExpr*>(expr)) {
        return inferConstructor(constructor);
    } else if (auto* match = dynamic_cast<ast::MatchExpr*>(expr)) {
        return inferMatch(match);
    } else if (auto* asyncExpr = dynamic_cast<ast::AsyncExpr*>(expr)) {
        ast::Type* inner = inferType(asyncExpr->getOperand());
        if (!inner) return nullptr;
        // async expr returns Promise<T,E>; simplified: treat as same type for now
        return copyType(inner).release();
    } else if (auto* awaitExpr = dynamic_cast<ast::AwaitExpr*>(expr)) {
        ast::Type* promiseType = inferType(awaitExpr->getOperand());
        if (!promiseType) return nullptr;
        // await unwraps Promise<T> to T; simplified: return operand type
        return copyType(promiseType).release();
    } else if (auto* spawnExpr = dynamic_cast<ast::SpawnExpr*>(expr)) {
        ast::Type* inner = inferType(spawnExpr->getOperand());
        if (!inner) return nullptr;
        // spawn returns Task<T>; simplified: treat as same type for now
        return copyType(inner).release();
    } else if (auto* joinExpr = dynamic_cast<ast::JoinExpr*>(expr)) {
        ast::Type* taskType = inferType(joinExpr->getOperand());
        if (!taskType) return nullptr;
        // join unwraps Task<T> to T; simplified: return operand type
        return copyType(taskType).release();
    } else if (auto* selectExpr = dynamic_cast<ast::SelectExpr*>(expr)) {
        // select { branches } type-checks each branch; result type is Unit or first branch
        for (const auto& branch : selectExpr->getBranches()) {
            if (branch->getStatement()) {
                checkStatement(branch->getStatement());
            }
        }
        return createUnitType().release();
    } else if (auto* arrayLit = dynamic_cast<ast::ArrayLiteralExpr*>(expr)) {
        return inferArrayLiteral(arrayLit);
    } else if (auto* arrayIndex = dynamic_cast<ast::ArrayIndexExpr*>(expr)) {
        return inferArrayIndex(arrayIndex);
    } else if (auto* recordLit = dynamic_cast<ast::RecordLiteralExpr*>(expr)) {
        return inferRecordLiteral(recordLit);
    } else if (auto* fieldAccess = dynamic_cast<ast::FieldAccessExpr*>(expr)) {
        return inferFieldAccess(fieldAccess);
    } else if (auto* blockExpr = dynamic_cast<ast::BlockExpr*>(expr)) {
        return inferBlockExpr(blockExpr);
    } else if (auto* ifExpr = dynamic_cast<ast::IfExpr*>(expr)) {
        return inferIfExpr(ifExpr);
    } else if (auto* rangeExpr = dynamic_cast<ast::RangeExpr*>(expr)) {
        return inferRangeExpr(rangeExpr);
    }
    
    return nullptr;
}

ast::Type* TypeChecker::inferLiteral(ast::LiteralExpr* expr) {
    switch (expr->getType()) {
        case ast::LiteralExpr::LiteralType::Int:
            return createIntType().release();
        case ast::LiteralExpr::LiteralType::Float:
            return createFloatType().release();
        case ast::LiteralExpr::LiteralType::Bool:
            return createBoolType().release();
        case ast::LiteralExpr::LiteralType::String:
            return createStringType().release();
        case ast::LiteralExpr::LiteralType::Null:
            return createNullType().release();
    }
    return nullptr;
}

ast::Type* TypeChecker::inferBinary(ast::BinaryExpr* expr) {
    ast::Type* leftType = inferType(expr->getLeft());
    ast::Type* rightType = inferType(expr->getRight());
    
    if (!leftType || !rightType) {
        return nullptr;
    }
    
    // Arithmetic operators (+, -, *, /, %)
    if (expr->getOp() == ast::BinaryExpr::Op::Add ||
        expr->getOp() == ast::BinaryExpr::Op::Sub ||
        expr->getOp() == ast::BinaryExpr::Op::Mul ||
        expr->getOp() == ast::BinaryExpr::Op::Div ||
        expr->getOp() == ast::BinaryExpr::Op::Mod) {
        
        // Int + Int -> Int
        if (typesEqual(leftType, createIntType().get()) &&
            typesEqual(rightType, createIntType().get())) {
            return createIntType().release();
        }
        
        // Float + Float -> Float
        // Int + Float -> Float (promotion)
        if (typesEqual(leftType, createFloatType().get()) ||
            typesEqual(rightType, createFloatType().get())) {
            return createFloatType().release();
        }
        
        // String + String -> String (concatenation)
        if (typesEqual(leftType, createStringType().get()) &&
            typesEqual(rightType, createStringType().get()) &&
            expr->getOp() == ast::BinaryExpr::Op::Add) {
            return createStringType().release();
        }
        
        reportTypeError(
            expr->getLocation(),
            "Invalid operands for arithmetic operator",
            leftType,
            rightType
        );
        return nullptr;
    }
    
    // Comparison operators (<, <=, >, >=, ==, !=)
    if (expr->getOp() == ast::BinaryExpr::Op::Lt ||
        expr->getOp() == ast::BinaryExpr::Op::Le ||
        expr->getOp() == ast::BinaryExpr::Op::Gt ||
        expr->getOp() == ast::BinaryExpr::Op::Ge ||
        expr->getOp() == ast::BinaryExpr::Op::Eq ||
        expr->getOp() == ast::BinaryExpr::Op::Ne) {
        
        // For == and !=, if both operands are the same generic type variable T, require T : Eq
        const bool isEquality = (expr->getOp() == ast::BinaryExpr::Op::Eq ||
                                 expr->getOp() == ast::BinaryExpr::Op::Ne);
        if (isEquality) {
            auto* leftGen = dynamic_cast<ast::GenericType*>(getEffectiveType(leftType));
            auto* rightGen = dynamic_cast<ast::GenericType*>(getEffectiveType(rightType));
            if (leftGen && rightGen && leftGen->getName() == rightGen->getName()) {
                const std::string& tv = leftGen->getName();
                ast::FunctionDecl* fn = currentFunction_;
                ast::InteractionDecl* in = currentInteraction_;
                bool hasEq = false;
                if (fn) {
                    for (const auto& gp : fn->getGenericParams()) {
                        if (gp.name == tv) {
                            hasEq = (gp.constraint == "Eq");
                            break;
                        }
                    }
                } else if (in) {
                    for (const auto& gp : in->getGenericParams()) {
                        if (gp.name == tv) {
                            hasEq = (gp.constraint == "Eq");
                            break;
                        }
                    }
                }
                if (!hasEq) {
                    errorReporter_.error(
                        expr->getLocation(),
                        "Type parameter '" + tv + "' must implement Eq to use == or != (add constraint: " + tv + " : Eq)"
                    );
                    return nullptr;
                }
            }
        }
        
        // Comparisons return Bool
        if (isAssignable(leftType, rightType) || isAssignable(rightType, leftType)) {
            return createBoolType().release();
        }
        
        reportTypeError(
            expr->getLocation(),
            "Cannot compare incompatible types",
            leftType,
            rightType
        );
        return nullptr;
    }
    
    // Logical operators (&&, ||)
    if (expr->getOp() == ast::BinaryExpr::Op::And ||
        expr->getOp() == ast::BinaryExpr::Op::Or) {
        
        if (typesEqual(leftType, createBoolType().get()) &&
            typesEqual(rightType, createBoolType().get())) {
            return createBoolType().release();
        }
        
        reportTypeError(
            expr->getLocation(),
            "Logical operators require boolean operands",
            leftType,
            rightType
        );
        return nullptr;
    }
    
    return nullptr;
}

ast::Type* TypeChecker::inferUnary(ast::UnaryExpr* expr) {
    ast::Type* operandType = inferType(expr->getOperand());
    
    if (!operandType) {
        return nullptr;
    }
    
    if (expr->getOp() == ast::UnaryExpr::Op::Neg) {
        // Negation: Int -> Int, Float -> Float
        if (typesEqual(operandType, createIntType().get())) {
            return createIntType().release();
        }
        if (typesEqual(operandType, createFloatType().get())) {
            return createFloatType().release();
        }
        
        reportTypeError(
            expr->getLocation(),
            "Negation operator requires numeric type",
            operandType
        );
        return nullptr;
    }
    
    if (expr->getOp() == ast::UnaryExpr::Op::Not) {
        // Logical not: Bool -> Bool
        if (typesEqual(operandType, createBoolType().get())) {
            return createBoolType().release();
        }
        
        reportTypeError(
            expr->getLocation(),
            "Logical not requires boolean operand",
            operandType
        );
        return nullptr;
    }
    
    return nullptr;
}

ast::Type* TypeChecker::inferArrayLiteral(ast::ArrayLiteralExpr* expr) {
    const auto& elements = expr->getElements();
    if (elements.empty()) {
        errorReporter_.error(
            expr->getLocation(),
            "Empty array literal requires explicit type annotation"
        );
        return nullptr;
    }
    ast::Type* elementType = inferType(elements[0].get());
    if (!elementType) return nullptr;
    for (size_t i = 1; i < elements.size(); ++i) {
        ast::Type* elemType = inferType(elements[i].get());
        if (!elemType) return nullptr;
        if (!typesEqual(elementType, elemType)) {
            reportTypeError(
                elements[i]->getLocation(),
                "Array elements must have the same type",
                elemType,
                elementType
            );
            return nullptr;
        }
    }
    return createArrayType(copyType(elementType)).release();
}

ast::Type* TypeChecker::inferArrayIndex(ast::ArrayIndexExpr* expr) {
    ast::Type* arrayType = inferType(expr->getArray());
    if (!arrayType) return nullptr;
    ast::Type* indexType = inferType(expr->getIndex());
    if (!indexType) return nullptr;
    if (!typesEqual(indexType, createIntType().get())) {
        reportTypeError(
            expr->getIndex()->getLocation(),
            "Array index must be Int",
            indexType
        );
        return nullptr;
    }
    auto* arrType = dynamic_cast<ast::ArrayType*>(arrayType);
    if (arrType) {
        return copyType(arrType->getElementType()).release();
    }
    auto* paramType = dynamic_cast<ast::ParameterizedType*>(arrayType);
    if (paramType && paramType->getBaseName() == "Array" && paramType->getTypeArgs().size() == 1) {
        return copyType(paramType->getTypeArgs()[0].get()).release();
    }
    reportTypeError(
        expr->getArray()->getLocation(),
        "Expected array type for indexing",
        arrayType
    );
    return nullptr;
}

ast::Type* TypeChecker::inferRecordLiteral(ast::RecordLiteralExpr* expr) {
    if (!expr || expr->getFields().empty()) {
        return nullptr;
    }
    std::vector<std::unique_ptr<ast::RecordField>> fields;
    for (const auto& f : expr->getFields()) {
        if (!f.value) {
            return nullptr;
        }
        ast::Type* fieldType = inferType(f.value.get());
        if (!fieldType) {
            return nullptr;
        }
        std::unique_ptr<ast::Type> fieldCopy = copyType(fieldType);
        if (!fieldCopy) {
            return nullptr;
        }
        fields.push_back(std::make_unique<ast::RecordField>(
            f.value->getLocation(), f.name, std::move(fieldCopy)));
    }
    return new ast::RecordType(expr->getLocation(), std::move(fields));
}

ast::Type* TypeChecker::inferFieldAccess(ast::FieldAccessExpr* expr) {
    ast::Type* baseType = inferType(expr->getRecord());
    if (!baseType) return nullptr;
    auto* recType = dynamic_cast<ast::RecordType*>(baseType);
    if (recType) {
        ast::RecordField* field = recType->getField(expr->getFieldName());
        if (field) {
            return copyType(field->getType()).release();
        }
    }
    // Base might be ParameterizedType for a type alias - would need type alias resolution.
    return nullptr;
}

ast::Type* TypeChecker::inferBlockExpr(ast::BlockExpr* expr) {
    symbolTable_.enterScope();
    for (const auto& stmt : expr->getStatements()) {
        checkStatement(stmt.get());
    }
    ast::Type* resultType = nullptr;
    if (expr->hasValueExpr()) {
        resultType = inferType(expr->getValueExpr());
    } else {
        resultType = createUnitType().release();
    }
    symbolTable_.exitScope();
    return resultType;
}

ast::Type* TypeChecker::inferIfExpr(ast::IfExpr* expr) {
    if (!expr) return nullptr;
    ast::Expr* thenBranch = expr->getThenBranch();
    ast::Expr* elseBranch = expr->getElseBranch();
    if (!elseBranch) {
        errorReporter_.error(
            expr->getLocation(),
            "if expression requires an else branch");
        return nullptr;
    }
    ast::Type* condType = inferType(expr->getCondition());
    if (!condType) return nullptr;
    ast::Type* thenType = thenBranch ? inferType(thenBranch) : nullptr;
    if (!thenType) return nullptr;
    ast::Type* elseType = inferType(elseBranch);
    if (!elseType) return nullptr;
    if (!typesEqual(thenType, elseType)) {
        errorReporter_.error(
            expr->getLocation(),
            "if expression branches must have the same type"
        );
        return nullptr;
    }
    return copyType(thenType).release();
}

ast::Type* TypeChecker::inferRangeExpr(ast::RangeExpr* expr) {
    ast::Type* startType = inferType(expr->getStart());
    ast::Type* endType = inferType(expr->getEnd());
    if (!startType || !endType) return nullptr;
    if (!typesEqual(startType, createIntType().get()) ||
        !typesEqual(endType, createIntType().get())) {
        errorReporter_.error(
            expr->getLocation(),
            "Range bounds must be Int"
        );
        return nullptr;
    }
    if (expr->hasStepHint()) {
        ast::Type* stepType = inferType(expr->getStepHint());
        if (!stepType || !typesEqual(stepType, createIntType().get())) {
            errorReporter_.error(
                expr->getStepHint()->getLocation(),
                "Range step hint (second value) must be Int"
            );
            return nullptr;
        }
    }
    // Range implements Iterator<Int>; return opaque Range type
    return std::make_unique<ast::ParameterizedType>(
        expr->getLocation(), "Range", std::vector<std::unique_ptr<ast::Type>>{}
    ).release();
}

ast::Type* TypeChecker::getIterableElementType(ast::Expr* iterable) {
    if (!iterable) return nullptr;
    // Range expression: element type is Int
    if (dynamic_cast<ast::RangeExpr*>(iterable)) {
        return createIntType().release();
    }
    // Array<T>: element type is T
    ast::Type* iterType = inferType(iterable);
    if (!iterType) return nullptr;
    if (auto* arr = dynamic_cast<ast::ArrayType*>(iterType)) {
        return copyType(arr->getElementType()).release();
    }
    if (auto* param = dynamic_cast<ast::ParameterizedType*>(iterType)) {
        if (param->getBaseName() == "Array" && param->getTypeArgs().size() == 1) {
            return copyType(param->getTypeArgs()[0].get()).release();
        }
    }
    return nullptr;
}

ast::Type* TypeChecker::inferVariable(ast::VariableExpr* expr) {
    Symbol* symbol = symbolTable_.lookup(expr->getName());
    
    if (!symbol) {
        errorReporter_.error(
            expr->getLocation(),
            "Undefined variable: " + expr->getName()
        );
        return nullptr;
    }
    
    if (symbol->getKind() == SymbolKind::Variable) {
        VariableSymbol* varSymbol = dynamic_cast<VariableSymbol*>(symbol);
        if (varSymbol && varSymbol->getType()) {
            return varSymbol->getType(); // Return non-owning pointer
        }
        return nullptr;
    }

    // Allow referring to a function/interaction by name as a value (function pointer-like).
    if (symbol->getKind() == SymbolKind::Function) {
        auto* funcSym = dynamic_cast<FunctionSymbol*>(symbol);
        if (!funcSym || !funcSym->getFunction()) return nullptr;
        ast::FunctionDecl* f = funcSym->getFunction();
        std::vector<std::unique_ptr<ast::Type>> paramTypes;
        for (const auto& p : f->getParameters()) {
            paramTypes.push_back(copyType(p->getType()));
        }
        auto ret = copyType(f->getReturnType());
        return createFunctionType(std::move(paramTypes), std::move(ret), /*isInteraction*/ false).release();
    }
    if (symbol->getKind() == SymbolKind::Interaction) {
        auto* interSym = dynamic_cast<InteractionSymbol*>(symbol);
        if (!interSym || !interSym->getInteraction()) return nullptr;
        ast::InteractionDecl* i = interSym->getInteraction();
        std::vector<std::unique_ptr<ast::Type>> paramTypes;
        for (const auto& p : i->getParameters()) {
            paramTypes.push_back(copyType(p->getType()));
        }
        auto ret = copyType(i->getReturnType());
        return createFunctionType(std::move(paramTypes), std::move(ret), /*isInteraction*/ true).release();
    }

    errorReporter_.error(
        expr->getLocation(),
        "Expected value, found: " + expr->getName()
    );
    return nullptr;
    
    return nullptr;
}

ast::Type* TypeChecker::inferStdlibCall(ast::FunctionCallExpr* expr) {
    const std::string& name = expr->getName();
    const size_t n = expr->getArgs().size();
    auto arg = [this, expr](size_t i) { return inferType(expr->getArgs()[i].get()); };
    auto err = [this, expr](const std::string& msg) {
        errorReporter_.error(expr->getLocation(), msg);
        return nullptr;
    };

    // I/O
    if (name == "print" || name == "println") {
        if (n != 1) return err("print/println expect 1 argument (String)");
        if (!arg(0)) return nullptr;
        return createUnitType().release();
    }
    if (name == "readLine") {
        if (n != 0) return err("readLine() takes no arguments");
        return createStringType().release();
    }
    if (name == "readFile") {
        if (n != 1) return err("readFile(filename) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    if (name == "writeFile") {
        if (n != 2) return err("writeFile(filename, content) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createUnitType().release();
    }
    // Math (Float -> Float or (Float,Float) -> Float)
    if (name == "sin" || name == "cos" || name == "sqrt" || name == "abs" || name == "floor" || name == "ceil") {
        if (n != 1) return err(name + " expects 1 argument (Float)");
        if (!arg(0)) return nullptr;
        return createFloatType().release();
    }
    if (name == "min" || name == "max") {
        if (n != 2) return err(name + " expects 2 arguments (Float, Float)");
        if (!arg(0) || !arg(1)) return nullptr;
        return createFloatType().release();
    }
    if (name == "minInt" || name == "maxInt") {
        if (n != 2) return err(name + " expects 2 arguments (Int, Int)");
        if (!arg(0) || !arg(1)) return nullptr;
        return createIntType().release();
    }
    // String
    if (name == "stringLength") {
        if (n != 1) return err("stringLength(s) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createIntType().release();
    }
    if (name == "stringConcat") {
        if (n != 2) return err("stringConcat(s1, s2) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createStringType().release();
    }
    if (name == "stringSlice") {
        if (n != 3) return err("stringSlice(s, start, end) expects 3 arguments");
        if (!arg(0) || !arg(1) || !arg(2)) return nullptr;
        return createStringType().release();
    }
    if (name == "stringToInt" || name == "stringToFloat") {
        if (n != 1) return err(name + "(s) expects 1 argument");
        if (!arg(0)) return nullptr;
        return (name == "stringToInt") ? createIntType().release() : createFloatType().release();
    }
    if (name == "intToString" || name == "floatToString") {
        if (n != 1) return err(name + " expects 1 argument");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    // String comparison
    if (name == "stringEquals") {
        if (n != 2) return err("stringEquals(s1, s2) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createBoolType().release();
    }
    if (name == "stringCompare") {
        if (n != 2) return err("stringCompare(s1, s2) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createIntType().release();
    }
    // Regular expressions
    if (name == "regexMatches") {
        if (n != 2) return err("regexMatches(str, pattern) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createIntType().release(); // Returns 1, 0, or -1
    }
    if (name == "regexSearch") {
        if (n != 2) return err("regexSearch(str, pattern) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createIntType().release(); // Returns index or -1
    }
    if (name == "regexReplace") {
        if (n != 3) return err("regexReplace(str, pattern, replacement) expects 3 arguments");
        if (!arg(0) || !arg(1) || !arg(2)) return nullptr;
        return createStringType().release();
    }
    if (name == "regexReplaceAll") {
        if (n != 3) return err("regexReplaceAll(str, pattern, replacement) expects 3 arguments");
        if (!arg(0) || !arg(1) || !arg(2)) return nullptr;
        return createStringType().release();
    }
    if (name == "regexExtract") {
        if (n != 3) return err("regexExtract(str, pattern, groupIndex) expects 3 arguments");
        if (!arg(0) || !arg(1) || !arg(2)) return nullptr;
        return createStringType().release();
    }
    // Array
    if (name == "arrayLength") {
        if (n != 1) return err("arrayLength(arr) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createIntType().release();
    }
    auto isArrayOfInt = [this](ast::Type* t) -> bool {
        if (!t) return false;
        if (auto* arr = dynamic_cast<ast::ArrayType*>(t))
            return dynamic_cast<ast::PrimitiveType*>(arr->getElementType()) &&
                   static_cast<ast::PrimitiveType*>(arr->getElementType())->getKind() == ast::PrimitiveType::Kind::Int;
        if (auto* p = dynamic_cast<ast::ParameterizedType*>(t))
            return p->getBaseName() == "Array" && p->getTypeArgs().size() == 1 &&
                   dynamic_cast<ast::PrimitiveType*>(p->getTypeArgs()[0].get()) &&
                   static_cast<ast::PrimitiveType*>(p->getTypeArgs()[0].get())->getKind() == ast::PrimitiveType::Kind::Int;
        return false;
    };
    if (name == "arrayReduceIntSum") {
        if (n != 1) return err("arrayReduceIntSum(arr) expects 1 argument");
        if (!arg(0)) return nullptr;
        if (!isArrayOfInt(arg(0))) return err("arrayReduceIntSum expects Array<Int>");
        return createIntType().release();
    }
    if (name == "arrayMapIntDouble") {
        if (n != 1) return err("arrayMapIntDouble(arr) expects 1 argument");
        if (!arg(0)) return nullptr;
        if (!isArrayOfInt(arg(0))) return err("arrayMapIntDouble expects Array<Int>");
        return createArrayType(createIntType()).release();
    }
    if (name == "arrayFilterIntPositive") {
        if (n != 1) return err("arrayFilterIntPositive(arr) expects 1 argument");
        if (!arg(0)) return nullptr;
        if (!isArrayOfInt(arg(0))) return err("arrayFilterIntPositive expects Array<Int>");
        return createArrayType(createIntType()).release();
    }
    // Socket
    if (name == "socketConnect") {
        if (n != 2) return err("socketConnect(host, port) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createIntType().release();
    }
    if (name == "socketSend") {
        if (n != 2) return err("socketSend(fd, str) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createIntType().release();
    }
    if (name == "socketRecv") {
        if (n != 1) return err("socketRecv(fd) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    if (name == "socketClose") {
        if (n != 1) return err("socketClose(fd) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createUnitType().release();
    }
    // HTTP
    if (name == "httpGet") {
        if (n != 1) return err("httpGet(url) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    if (name == "httpPost") {
        if (n != 2) return err("httpPost(url, body) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createStringType().release();
    }
    // HTTP client/server (handles are Int)
    if (name == "httpRequest") {
        if (n != 6) return err("httpRequest(method, url, pathParamsJson, queryJson, headersJson, body) expects 6 arguments");
        for (size_t i = 0; i < n; ++i) if (!arg(i)) return nullptr;
        return createIntType().release(); // Response handle
    }
    if (name == "httpServerCreate") {
        if (n != 2) return err("httpServerCreate(host, port) expects 2 arguments");
        if (!arg(0) || !arg(1)) return nullptr;
        return createIntType().release(); // Server handle
    }
    if (name == "httpServerGet" || name == "httpServerPost") {
        if (n != 3) return err(name + "(server, route, handler) expects 3 arguments");
        if (!arg(0) || !arg(1) || !arg(2)) return nullptr;
        return createUnitType().release();
    }
    if (name == "httpServerListen" || name == "httpServerClose") {
        if (n != 1) return err(name + "(server) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createUnitType().release();
    }
    if (name == "httpReqMethod" || name == "httpReqPath" || name == "httpReqParamsJson" ||
        name == "httpReqQueryJson" || name == "httpReqHeadersJson" || name == "httpReqBody") {
        if (n != 1) return err(name + "(req) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    if (name == "httpResponseCreate") {
        if (n != 3) return err("httpResponseCreate(status, headersJson, body) expects 3 arguments");
        if (!arg(0) || !arg(1) || !arg(2)) return nullptr;
        return createIntType().release(); // Response handle
    }
    if (name == "httpRespStatus") {
        if (n != 1) return err("httpRespStatus(resp) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createIntType().release();
    }
    if (name == "httpRespHeadersJson" || name == "httpRespBody") {
        if (n != 1) return err(name + "(resp) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    // JSON
    if (name == "jsonPrettify" || name == "jsonStringifyString") {
        if (n != 1) return err(name + " expects 1 argument (String)");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    if (name == "jsonStringifyInt" || name == "jsonStringifyFloat") {
        if (n != 1) return err(name + " expects 1 argument");
        if (!arg(0)) return nullptr;
        return createStringType().release();
    }
    return nullptr;  // not a stdlib function
}

ast::Type* TypeChecker::inferFunctionCall(ast::FunctionCallExpr* expr) {
    const std::string& name = expr->getName();
    auto cit = constructorToADT_.find(name);
    if (cit != constructorToADT_.end()) {
        ast::ADTType* adt = cit->second;
        ast::Constructor* constructor = nullptr;
        for (const auto& c : adt->getConstructors()) {
            if (c && c->getName() == name) { constructor = c.get(); break; }
        }
        if (!constructor) return nullptr;
        const auto& expected = constructor->getArgumentTypes();
        const auto& args = expr->getArgs();
        if (args.size() != expected.size()) {
            errorReporter_.error(expr->getLocation(),
                "Constructor " + name + " expects " + std::to_string(expected.size()) +
                " argument(s), got " + std::to_string(args.size()));
            return nullptr;
        }
        std::string typeName;
        for (const auto& p : typeDecls_) {
            if (p.second == adt) { typeName = p.first; break; }
        }
        const bool isGeneric = !typeName.empty() && typeDeclParams_.count(typeName) && !typeDeclParams_[typeName].empty();
        std::map<std::string, ast::Type*> inferredParams;
        for (size_t i = 0; i < args.size(); ++i) {
            ast::Type* argType = inferType(args[i].get());
            if (!argType) return nullptr;
            if (auto* gen = dynamic_cast<ast::GenericType*>(expected[i].get()))
                inferredParams[gen->getName()] = argType;
            std::unique_ptr<ast::Type> expectedCopy;
            if (isGeneric && !inferredParams.empty())
                expectedCopy = substituteType(expected[i].get(), inferredParams);
            if (!expectedCopy)
                expectedCopy = copyType(expected[i].get());
            if (!expectedCopy || !isAssignable(argType, expectedCopy.get())) {
                errorReporter_.error(args[i]->getLocation(),
                    "Constructor " + name + " argument " + std::to_string(i + 1) + " type mismatch");
                return nullptr;
            }
        }
        if (isGeneric) {
            const auto& params = typeDeclParams_[typeName];
            std::vector<std::unique_ptr<ast::Type>> typeArgs;
            for (const std::string& pname : params) {
                auto it = inferredParams.find(pname);
                if (it == inferredParams.end()) break;
                auto a = copyType(it->second);
                if (!a) break;
                typeArgs.push_back(std::move(a));
            }
            if (typeArgs.size() == params.size())
                return new ast::ParameterizedType(expr->getLocation(), typeName, std::move(typeArgs));
        }
        return copyType(adt).release();
    }

    // Look up function
    std::vector<Symbol*> functions = symbolTable_.lookupAll(name);
    
    if (functions.empty()) {
        // Try resolving as an imported symbol via module resolver, but only
        // through modules that are explicitly imported by this program,
        // respecting the import kind (All/Specific/Default).
        if (moduleResolver_ && currentProgram_) {
            // Temporary debug output for multi-module resolution issues.
            // This will be removed once the module search path is stable.
            // It is intentionally minimal to avoid noisy logs.
            // std::cerr << "[TypeChecker] resolving call to '" << expr->getName()
            //           << "' in module '" << currentProgram_->getModuleName() << "'\n";
            // Infer argument types first (needed for signature match).
            std::vector<ast::Type*> argTypes;
            for (const auto& arg : expr->getArgs()) {
                ast::Type* argType = inferType(arg.get());
                if (!argType) {
                    return nullptr;
                }
                argTypes.push_back(argType);
            }

            std::vector<std::string> candidateModules;
            for (const auto& impPtr : currentProgram_->getImports()) {
                auto* imp = impPtr.get();
                if (!imp) continue;
                const std::string& modName = imp->getModuleName();
                switch (imp->getKind()) {
                case ast::ImportDecl::ImportKind::All:
                case ast::ImportDecl::ImportKind::Default:
                    candidateModules.push_back(modName);
                    break;
                case ast::ImportDecl::ImportKind::Specific: {
                    const auto& syms = imp->getSymbols();
                    if (std::find(syms.begin(), syms.end(), expr->getName()) != syms.end()) {
                        candidateModules.push_back(modName);
                    }
                    break;
                }
                }
            }

            for (const auto& modName : candidateModules) {
                if (!moduleResolver_->isModuleLoaded(modName)) {
                    continue;
                }
                if (auto* f = moduleResolver_->getFunction(modName, expr->getName())) {
                    // For imported symbols, trust the callee's signature and
                    // use its declared return type without re-checking the
                    // full parameter list. This keeps multi-module resolution
                    // simple while still allowing local type checking to be
                    // strict.
                    return f->getReturnType();
                }
                if (auto* i = moduleResolver_->getInteraction(modName, expr->getName())) {
                    return i->getReturnType();
                }
            }
        }

        // Phase 7.3: Standard library built-ins (I/O, Math, String, Array, Socket, HTTP, JSON)
        if (ast::Type* t = inferStdlibCall(expr)) return t;

        errorReporter_.error(
            expr->getLocation(),
            "Undefined function: " + expr->getName()
        );
        return nullptr;
    }
    
    // Infer argument types
    std::vector<ast::Type*> argTypes;
    for (const auto& arg : expr->getArgs()) {
        ast::Type* argType = inferType(arg.get());
        if (!argType) {
            return nullptr; // Error already reported
        }
        argTypes.push_back(argType);
    }
    
    // Try to find matching function based on argument types
    ast::FunctionDecl* matchedFunc = nullptr;
    for (Symbol* sym : functions) {
        if (sym->getKind() == SymbolKind::Function) {
            FunctionSymbol* funcSym = dynamic_cast<FunctionSymbol*>(sym);
            if (funcSym && funcSym->getFunction()) {
                ast::FunctionDecl* func = funcSym->getFunction();
                if (matchFunctionSignature(func, argTypes)) {
                    matchedFunc = func;
                    break;
                }
            }
        } else if (sym->getKind() == SymbolKind::Interaction) {
            InteractionSymbol* interSym = dynamic_cast<InteractionSymbol*>(sym);
            if (interSym && interSym->getInteraction()) {
                ast::InteractionDecl* interaction = interSym->getInteraction();
                if (matchFunctionSignature(interaction, argTypes)) {
                    // For now, treat interactions like functions
                    // TODO: Track purity properly
                    matchedFunc = nullptr; // Can't cast InteractionDecl to FunctionDecl
                    // We'll handle this differently
                }
            }
        }
    }
    
    if (!matchedFunc) {
        // Try interactions separately
        for (Symbol* sym : functions) {
            if (sym->getKind() == SymbolKind::Interaction) {
                InteractionSymbol* interSym = dynamic_cast<InteractionSymbol*>(sym);
                if (interSym && interSym->getInteraction()) {
                    ast::InteractionDecl* interaction = interSym->getInteraction();
                    if (matchFunctionSignature(interaction, argTypes)) {
                        ast::Type* returnType = interaction->getReturnType();
                        if (returnType) {
                            return returnType;
                        }
                    }
                }
            }
        }
        
        // No match found
        errorReporter_.error(
            expr->getLocation(),
            "No matching function signature for: " + expr->getName() + 
            " with " + std::to_string(argTypes.size()) + " argument(s)"
        );
        return nullptr;
    }
    
    // Check interface constraints at call site: for each generic param with constraint, type arg must implement it
    const auto& params = matchedFunc->getParameters();
    std::map<std::string, ast::Type*> subst;
    for (size_t i = 0; i < params.size() && i < argTypes.size(); ++i) {
        ast::Type* pt = params[i]->getType();
        if (auto* gen = dynamic_cast<ast::GenericType*>(pt)) {
            subst[gen->getName()] = argTypes[i];
        }
        // Infer from ParameterizedType vs ParameterizedType (e.g. List<T> vs List<Int> -> T = Int)
        auto* pParam = dynamic_cast<ast::ParameterizedType*>(pt);
        auto* pArg = dynamic_cast<ast::ParameterizedType*>(argTypes[i]);
        if (pParam && pArg && pParam->getBaseName() == pArg->getBaseName()) {
            const auto& paramArgs = pParam->getTypeArgs();
            const auto& argArgs = pArg->getTypeArgs();
            if (paramArgs.size() == argArgs.size()) {
                for (size_t j = 0; j < paramArgs.size(); ++j) {
                    if (paramArgs[j] && argArgs[j]) {
                        if (auto* g = dynamic_cast<ast::GenericType*>(paramArgs[j].get())) {
                            subst[g->getName()] = argArgs[j].get();
                        }
                    }
                }
            }
        }
    }
    for (const auto& gp : matchedFunc->getGenericParams()) {
        if (gp.constraint.empty()) continue;
        auto it = subst.find(gp.name);
        if (it == subst.end()) continue;
        if (!typeImplementsInterface(it->second, gp.constraint)) {
            errorReporter_.error(
                expr->getLocation(),
                "Type argument for '" + gp.name + "' does not implement " + gp.constraint
            );
            return nullptr;
        }
    }
    
    // Store inferred type arguments on the call for IR generation (monomorphization at call sites).
    // Only set when we have concrete types from subst; recursive calls inside generic functions
    // get type args from the current monomorphized body's substitution in the IR generator.
    if (!subst.empty() && !matchedFunc->getGenericParams().empty()) {
        std::vector<std::unique_ptr<ast::Type>> typeArgs;
        for (const auto& gp : matchedFunc->getGenericParams()) {
            auto it = subst.find(gp.name);
            if (it == subst.end() || !it->second) break;
            auto c = copyType(it->second);
            if (!c) break;
            typeArgs.push_back(std::move(c));
        }
        if (typeArgs.size() == matchedFunc->getGenericParams().size()) {
            expr->setInferredTypeArgs(std::move(typeArgs));
        }
    }

    ast::Type* returnType = matchedFunc->getReturnType();
    if (returnType) {
        // Substitute generic return type with inferred type args so e.g. cons(1, ...) has type List<Int>, not List<T>
        if (!subst.empty()) {
            auto st = substituteType(returnType, subst);
            if (st) {
                substitutedReturnTypes_.push_back(std::move(st));
                return substitutedReturnTypes_.back().get();
            }
        }
        return returnType; // Non-owning pointer
    }
    
    return nullptr;
}

bool TypeChecker::checkType(ast::Expr* expr, ast::Type* expectedType) {
    if (!expr || !expectedType) {
        return false;
    }
    
    ast::Type* actualType = inferType(expr);
    if (!actualType) {
        return false;
    }
    
    if (!isAssignable(actualType, expectedType)) {
        reportTypeError(
            expr->getLocation(),
            "Type mismatch",
            actualType,
            expectedType
        );
        return false;
    }
    
    return true;
}

// Unwrap refinement types to base type for structural comparison/assignability.
static ast::Type* getEffectiveType(ast::Type* type) {
    if (auto* ref = dynamic_cast<ast::RefinementType*>(type)) {
        return ref->getBaseType();
    }
    return type;
}

// Compare two index expressions (for dependent/indexed type equality).
// LiteralExpr: same type and value; VariableExpr: same name; otherwise false.
static bool indexExprsEqual(ast::Expr* a, ast::Expr* b) {
    if (!a || !b) {
        return a == b;
    }
    auto* litA = dynamic_cast<ast::LiteralExpr*>(a);
    auto* litB = dynamic_cast<ast::LiteralExpr*>(b);
    if (litA && litB) {
        return litA->getType() == litB->getType() && litA->getValue() == litB->getValue();
    }
    auto* varA = dynamic_cast<ast::VariableExpr*>(a);
    auto* varB = dynamic_cast<ast::VariableExpr*>(b);
    if (varA && varB) {
        return varA->getName() == varB->getName();
    }
    return false;
}

bool TypeChecker::typesEqual(ast::Type* type1, ast::Type* type2) {
    if (!type1 || !type2) {
        return type1 == type2; // Both null or both non-null
    }
    type1 = getEffectiveType(type1);
    type2 = getEffectiveType(type2);
    // Resolve user-defined generic only when comparing to a non-parameterized type (avoids infinite recursion for recursive types like List<Int>)
    if (auto* p1 = dynamic_cast<ast::ParameterizedType*>(type1)) {
        auto* p2 = dynamic_cast<ast::ParameterizedType*>(type2);
        if (!p2) {
            auto r1 = resolveParameterizedType(p1);
            if (r1) return typesEqual(r1.get(), type2);
        }
    }
    if (auto* p2 = dynamic_cast<ast::ParameterizedType*>(type2)) {
        auto* p1 = dynamic_cast<ast::ParameterizedType*>(type1);
        if (!p1) {
            auto r2 = resolveParameterizedType(p2);
            if (r2) return typesEqual(type1, r2.get());
        }
    }
    // Same type alias by name (avoids infinite recursion when comparing ADTs with recursive refs)
    auto* alias1 = dynamic_cast<ast::GenericType*>(type1);
    auto* alias2 = dynamic_cast<ast::GenericType*>(type2);
    if (alias1 && alias2 && alias1->getName() == alias2->getName()) {
        return true;
    }
    // Resolve one side when comparing type alias to concrete type
    if (alias1) {
        auto it = typeDecls_.find(alias1->getName());
        if (it != typeDecls_.end() && it->second) {
            return typesEqual(it->second, type2);
        }
    }
    if (alias2) {
        auto it = typeDecls_.find(alias2->getName());
        if (it != typeDecls_.end() && it->second) {
            return typesEqual(type1, it->second);
        }
    }

    // Compare primitive types
    auto* prim1 = dynamic_cast<ast::PrimitiveType*>(type1);
    auto* prim2 = dynamic_cast<ast::PrimitiveType*>(type2);
    
    if (prim1 && prim2) {
        return prim1->getKind() == prim2->getKind();
    }
    
    // Compare array types (ArrayType and ParameterizedType("Array", [T]))
    auto* arr1 = dynamic_cast<ast::ArrayType*>(type1);
    auto* arr2 = dynamic_cast<ast::ArrayType*>(type2);
    auto* param1 = dynamic_cast<ast::ParameterizedType*>(type1);
    auto* param2 = dynamic_cast<ast::ParameterizedType*>(type2);
    
    if (arr1 && arr2) {
        return typesEqual(arr1->getElementType(), arr2->getElementType());
    }
    if (arr1 && param2 && param2->getBaseName() == "Array" && param2->getTypeArgs().size() == 1) {
        return typesEqual(arr1->getElementType(), param2->getTypeArgs()[0].get());
    }
    if (arr2 && param1 && param1->getBaseName() == "Array" && param1->getTypeArgs().size() == 1) {
        return typesEqual(param1->getTypeArgs()[0].get(), arr2->getElementType());
    }
    
    // Compare indexed types (BaseType[indexList]): same base type and equal indices
    auto* idx1 = dynamic_cast<ast::IndexedType*>(type1);
    auto* idx2 = dynamic_cast<ast::IndexedType*>(type2);
    if (idx1 && idx2) {
        if (!typesEqual(idx1->getBaseType(), idx2->getBaseType())) {
            return false;
        }
        const auto& i1 = idx1->getIndices();
        const auto& i2 = idx2->getIndices();
        if (i1.size() != i2.size()) {
            return false;
        }
        for (size_t k = 0; k < i1.size(); ++k) {
            if (!indexExprsEqual(i1[k].get(), i2[k].get())) {
                return false;
            }
        }
        return true;
    }
    
    // Compare dependent function types (Pi): (x: A) -> B
    auto* pi1 = dynamic_cast<ast::DependentFunctionType*>(type1);
    auto* pi2 = dynamic_cast<ast::DependentFunctionType*>(type2);
    if (pi1 && pi2) {
        return pi1->getParamName() == pi2->getParamName() &&
               typesEqual(pi1->getParamType(), pi2->getParamType()) &&
               typesEqual(pi1->getReturnType(), pi2->getReturnType());
    }
    
    // Compare dependent pair types (Sigma): (x: A) * B
    auto* sigma1 = dynamic_cast<ast::DependentPairType*>(type1);
    auto* sigma2 = dynamic_cast<ast::DependentPairType*>(type2);
    if (sigma1 && sigma2) {
        return sigma1->getVarName() == sigma2->getVarName() &&
               typesEqual(sigma1->getVarType(), sigma2->getVarType()) &&
               typesEqual(sigma1->getBodyType(), sigma2->getBodyType());
    }
    
    // Compare forall types: forall T U. Body (same type vars and body)
    auto* forall1 = dynamic_cast<ast::ForallType*>(type1);
    auto* forall2 = dynamic_cast<ast::ForallType*>(type2);
    if (forall1 && forall2) {
        const auto& v1 = forall1->getTypeVars();
        const auto& v2 = forall2->getTypeVars();
        if (v1.size() != v2.size()) {
            return false;
        }
        for (size_t i = 0; i < v1.size(); ++i) {
            if (v1[i] != v2[i]) {
                return false;
            }
        }
        return typesEqual(forall1->getBodyType(), forall2->getBodyType());
    }
    
    // Compare existential types: exists x: A. B
    auto* ex1 = dynamic_cast<ast::ExistentialType*>(type1);
    auto* ex2 = dynamic_cast<ast::ExistentialType*>(type2);
    if (ex1 && ex2) {
        return ex1->getVarName() == ex2->getVarName() &&
               typesEqual(ex1->getVarType(), ex2->getVarType()) &&
               typesEqual(ex1->getBodyType(), ex2->getBodyType());
    }
    
    // Compare generic types (by name)
    auto* gen1 = dynamic_cast<ast::GenericType*>(type1);
    auto* gen2 = dynamic_cast<ast::GenericType*>(type2);
    
    if (gen1 && gen2) {
        return gen1->getName() == gen2->getName();
    }
    
    // Compare parameterized types (param1, param2 already declared above for array comparison)
    if (param1 && param2) {
        if (param1->getBaseName() != param2->getBaseName()) {
            return false;
        }
        
        const auto& args1 = param1->getTypeArgs();
        const auto& args2 = param2->getTypeArgs();
        
        if (args1.size() != args2.size()) {
            return false;
        }
        
        for (size_t i = 0; i < args1.size(); ++i) {
            if (!typesEqual(args1[i].get(), args2[i].get())) {
                return false;
            }
        }
        
        return true;
    }
    
    // Compare record types (structural: same field names and types in order)
    auto* rec1 = dynamic_cast<ast::RecordType*>(type1);
    auto* rec2 = dynamic_cast<ast::RecordType*>(type2);
    if (rec1 && rec2) {
        const auto& f1 = rec1->getFields();
        const auto& f2 = rec2->getFields();
        if (f1.size() != f2.size()) {
            return false;
        }
        for (size_t i = 0; i < f1.size(); ++i) {
            if (f1[i]->getName() != f2[i]->getName()) {
                return false;
            }
            if (!typesEqual(f1[i]->getType(), f2[i]->getType())) {
                return false;
            }
        }
        return true;
    }

    // Compare function types
    auto* func1 = dynamic_cast<ast::FunctionType*>(type1);
    auto* func2 = dynamic_cast<ast::FunctionType*>(type2);
    
    if (func1 && func2) {
        // Check if both are interactions or both are functions
        if (func1->isInteraction() != func2->isInteraction()) {
            return false;
        }
        
        // Check return types
        if (!typesEqual(func1->getReturnType(), func2->getReturnType())) {
            return false;
        }
        
        // Check parameter types
        const auto& params1 = func1->getParamTypes();
        const auto& params2 = func2->getParamTypes();
        
        if (params1.size() != params2.size()) {
            return false;
        }
        
        for (size_t i = 0; i < params1.size(); ++i) {
            if (!typesEqual(params1[i].get(), params2[i].get())) {
                return false;
            }
        }
        
        return true;
    }

    // Compare ADT types (by name and structure)
    auto* adt1 = dynamic_cast<ast::ADTType*>(type1);
    auto* adt2 = dynamic_cast<ast::ADTType*>(type2);
    if (adt1 && adt2) {
        if (adt1->getName() != adt2->getName()) return false;
        const auto& c1 = adt1->getConstructors();
        const auto& c2 = adt2->getConstructors();
        if (c1.size() != c2.size()) return false;
        for (size_t i = 0; i < c1.size(); ++i) {
            if (!c1[i] || !c2[i] || c1[i]->getName() != c2[i]->getName()) return false;
            const auto& a1 = c1[i]->getArgumentTypes();
            const auto& a2 = c2[i]->getArgumentTypes();
            if (a1.size() != a2.size()) return false;
            for (size_t j = 0; j < a1.size(); ++j) {
                if (!typesEqual(a1[j].get(), a2[j].get())) return false;
            }
        }
        return true;
    }
    
    return false;
}

bool TypeChecker::isAssignable(ast::Type* from, ast::Type* to) {
    if (!from || !to) {
        return false;
    }
    from = getEffectiveType(from);
    to = getEffectiveType(to);
    // null is assignable to any pointer-like or parameterized type (e.g. List<Int>) before resolution
    if (auto* fromP = dynamic_cast<ast::PrimitiveType*>(from)) {
        if (fromP->getKind() == ast::PrimitiveType::Kind::Null) {
            if (dynamic_cast<ast::ADTType*>(to)) return true;
            if (dynamic_cast<ast::RecordType*>(to)) return true;
            if (dynamic_cast<ast::ArrayType*>(to)) return true;
            if (dynamic_cast<ast::ParameterizedType*>(to)) return true;
            if (auto* toP = dynamic_cast<ast::PrimitiveType*>(to))
                if (toP->getKind() == ast::PrimitiveType::Kind::String) return true;
        }
    }
    // ParameterizedType with same base: assignable when each type arg is assignable (e.g. List<Int> to List<T>)
    auto* fromPT = dynamic_cast<ast::ParameterizedType*>(from);
    auto* toPT = dynamic_cast<ast::ParameterizedType*>(to);
    if (fromPT && toPT &&
        fromPT->getBaseName() == toPT->getBaseName() &&
        fromPT->getTypeArgs().size() == toPT->getTypeArgs().size()) {
        for (size_t i = 0; i < fromPT->getTypeArgs().size(); ++i) {
            if (!isAssignable(fromPT->getTypeArgs()[i].get(), toPT->getTypeArgs()[i].get()))
                return false;
        }
        return true;
    }
    // Resolve user-defined generic types (e.g. List<Int> -> expanded ADT)
    if (auto* pf = dynamic_cast<ast::ParameterizedType*>(from)) {
        auto rf = resolveParameterizedType(pf);
        if (rf) return isAssignable(rf.get(), to);
    }
    if (auto* pt = dynamic_cast<ast::ParameterizedType*>(to)) {
        auto rt = resolveParameterizedType(pt);
        if (rt) return isAssignable(from, rt.get());
    }
    // Resolve type alias (e.g. parameter type LexItem -> ADTType)
    if (auto* toGen = dynamic_cast<ast::GenericType*>(to)) {
        auto it = typeDecls_.find(toGen->getName());
        if (it != typeDecls_.end() && it->second) {
            return isAssignable(from, it->second);
        }
        // Type variable (generic param, not a type decl): allow concrete type so generic function matching succeeds
        if (auto* fromGen = dynamic_cast<ast::GenericType*>(from)) {
            return fromGen->getName() == toGen->getName();
        }
        return true;
    }
    // Resolve "from" when it is a type alias (e.g. pattern-bound variable type GenericType("Expr"))
    if (auto* fromGen = dynamic_cast<ast::GenericType*>(from)) {
        auto it = typeDecls_.find(fromGen->getName());
        if (it != typeDecls_.end() && it->second) {
            return isAssignable(it->second, to);
        }
    }

    // Exact type match
    if (typesEqual(from, to)) {
        return true;
    }

    // Numeric promotion: Int -> Float
    auto* fromPrim = dynamic_cast<ast::PrimitiveType*>(from);
    auto* toPrim = dynamic_cast<ast::PrimitiveType*>(to);
    
    if (fromPrim && toPrim) {
        if (fromPrim->getKind() == ast::PrimitiveType::Kind::Int &&
            toPrim->getKind() == ast::PrimitiveType::Kind::Float) {
            return true;
        }
    }
    
    // Array types: Array<T1> is assignable to Array<T2> only if T1 == T2 (no covariance)
    auto* fromArr = dynamic_cast<ast::ArrayType*>(from);
    auto* toArr = dynamic_cast<ast::ArrayType*>(to);
    auto* fromParam = dynamic_cast<ast::ParameterizedType*>(from);
    auto* toParam = dynamic_cast<ast::ParameterizedType*>(to);
    
    if (fromArr && toArr) {
        return typesEqual(fromArr->getElementType(), toArr->getElementType());
    }
    // Array literal yields ArrayType; annotation may be ParameterizedType("Array", [T])
    if (fromArr && toParam && toParam->getBaseName() == "Array" &&
        toParam->getTypeArgs().size() == 1) {
        return typesEqual(fromArr->getElementType(), toParam->getTypeArgs()[0].get());
    }
    if (toArr && fromParam && fromParam->getBaseName() == "Array" &&
        fromParam->getTypeArgs().size() == 1) {
        return typesEqual(fromParam->getTypeArgs()[0].get(), toArr->getElementType());
    }
    
    // Indexed types: exact structural match (same base and same indices)
    auto* fromIdx = dynamic_cast<ast::IndexedType*>(from);
    auto* toIdx = dynamic_cast<ast::IndexedType*>(to);
    if (fromIdx && toIdx) {
        return typesEqual(from, to);
    }
    
    // Dependent function (Pi) and dependent pair (Sigma): exact structural match
    auto* fromPi = dynamic_cast<ast::DependentFunctionType*>(from);
    auto* toPi = dynamic_cast<ast::DependentFunctionType*>(to);
    if (fromPi && toPi) {
        return typesEqual(from, to);
    }
    auto* fromSigma = dynamic_cast<ast::DependentPairType*>(from);
    auto* toSigma = dynamic_cast<ast::DependentPairType*>(to);
    if (fromSigma && toSigma) {
        return typesEqual(from, to);
    }
    
    // Forall and existential: exact structural match
    auto* fromForall = dynamic_cast<ast::ForallType*>(from);
    auto* toForall = dynamic_cast<ast::ForallType*>(to);
    if (fromForall && toForall) {
        return typesEqual(from, to);
    }
    auto* fromEx = dynamic_cast<ast::ExistentialType*>(from);
    auto* toEx = dynamic_cast<ast::ExistentialType*>(to);
    if (fromEx && toEx) {
        return typesEqual(from, to);
    }
    
    // Generic type parameter can be assigned from/to any type (will be checked at instantiation)
    // For now, we'll be conservative and only allow exact matches
    auto* fromGen = dynamic_cast<ast::GenericType*>(from);
    auto* toGen = dynamic_cast<ast::GenericType*>(to);
    
    if (fromGen && toGen) {
        return fromGen->getName() == toGen->getName();
    }
    
    // Function types: function(Params1) -> Ret1 is assignable to function(Params2) -> Ret2
    // if Ret1 is assignable to Ret2 and Params2 are assignable to Params1 (contravariant params, covariant return)
    // For now, we'll use exact matching for simplicity
    auto* fromFunc = dynamic_cast<ast::FunctionType*>(from);
    auto* toFunc = dynamic_cast<ast::FunctionType*>(to);
    
    if (fromFunc && toFunc) {
        // Both must be same kind (function or interaction)
        if (fromFunc->isInteraction() != toFunc->isInteraction()) {
            return false;
        }
        
        // For now, require exact match
        return typesEqual(from, to);
    }
    
    return false;
}

void TypeChecker::reportTypeError(const SourceLocation& loc,
                                  const std::string& message,
                                  ast::Type* actualType,
                                  ast::Type* expectedType) {
    std::ostringstream oss;
    oss << message;
    
    if (actualType && expectedType) {
        oss << " (got ";
        // TODO: Add type name formatting
        oss << "type, expected ";
        oss << "type)";
    } else if (actualType) {
        oss << " (got type)";
    } else if (expectedType) {
        oss << " (expected type)";
    }
    
    errorReporter_.error(loc, oss.str());
}

std::unique_ptr<ast::Type> TypeChecker::createPrimitiveType(ast::PrimitiveType::Kind kind) {
    return std::make_unique<ast::PrimitiveType>(SourceLocation(), kind);
}

std::unique_ptr<ast::Type> TypeChecker::createIntType() {
    return createPrimitiveType(ast::PrimitiveType::Kind::Int);
}

std::unique_ptr<ast::Type> TypeChecker::createFloatType() {
    return createPrimitiveType(ast::PrimitiveType::Kind::Float);
}

std::unique_ptr<ast::Type> TypeChecker::createBoolType() {
    return createPrimitiveType(ast::PrimitiveType::Kind::Bool);
}

std::unique_ptr<ast::Type> TypeChecker::createStringType() {
    return createPrimitiveType(ast::PrimitiveType::Kind::String);
}

std::unique_ptr<ast::Type> TypeChecker::createUnitType() {
    return createPrimitiveType(ast::PrimitiveType::Kind::Unit);
}

std::unique_ptr<ast::Type> TypeChecker::createNullType() {
    return createPrimitiveType(ast::PrimitiveType::Kind::Null);
}

std::unique_ptr<ast::Type> TypeChecker::createArrayType(std::unique_ptr<ast::Type> elementType) {
    return std::make_unique<ast::ArrayType>(SourceLocation(), std::move(elementType));
}

std::unique_ptr<ast::Type> TypeChecker::createFunctionType(
    std::vector<std::unique_ptr<ast::Type>> paramTypes,
    std::unique_ptr<ast::Type> returnType,
    bool isInteraction) {
    return std::make_unique<ast::FunctionType>(
        SourceLocation(), std::move(paramTypes), std::move(returnType), isInteraction
    );
}

std::unique_ptr<ast::Type> TypeChecker::copyType(ast::Type* type) {
    if (!type) {
        return nullptr;
    }
    
    SourceLocation loc = type->getLocation();
    
    // Copy primitive types
    if (auto* prim = dynamic_cast<ast::PrimitiveType*>(type)) {
        return std::make_unique<ast::PrimitiveType>(loc, prim->getKind());
    }
    
    // Copy array types
    if (auto* arr = dynamic_cast<ast::ArrayType*>(type)) {
        std::unique_ptr<ast::Type> elementCopy = copyType(arr->getElementType());
        if (!elementCopy) {
            return nullptr;
        }
        return std::make_unique<ast::ArrayType>(loc, std::move(elementCopy));
    }
    
    // Copy generic types (resolve type alias / sum type name if declared)
    if (auto* gen = dynamic_cast<ast::GenericType*>(type)) {
        auto it = typeDecls_.find(gen->getName());
        if (it != typeDecls_.end() && it->second) {
            return copyType(it->second);
        }
        return std::make_unique<ast::GenericType>(loc, gen->getName());
    }
    
    // Copy refinement types (predicate is shared)
    if (auto* ref = dynamic_cast<ast::RefinementType*>(type)) {
        std::unique_ptr<ast::Type> baseCopy = copyType(ref->getBaseType());
        if (!baseCopy) {
            return nullptr;
        }
        return std::make_unique<ast::RefinementType>(
            loc, ref->getVariableName(), std::move(baseCopy), ref->getPredicateShared());
    }
    
    // Copy indexed types (indices are shared)
    if (auto* idx = dynamic_cast<ast::IndexedType*>(type)) {
        std::unique_ptr<ast::Type> baseCopy = copyType(idx->getBaseType());
        if (!baseCopy) {
            return nullptr;
        }
        std::vector<std::shared_ptr<ast::Expr>> indicesCopy(idx->getIndices().begin(), idx->getIndices().end());
        return std::make_unique<ast::IndexedType>(loc, std::move(baseCopy), std::move(indicesCopy));
    }
    
    // Copy dependent function type (Pi)
    if (auto* pi = dynamic_cast<ast::DependentFunctionType*>(type)) {
        std::unique_ptr<ast::Type> paramCopy = copyType(pi->getParamType());
        std::unique_ptr<ast::Type> returnCopy = copyType(pi->getReturnType());
        if (!paramCopy || !returnCopy) {
            return nullptr;
        }
        return std::make_unique<ast::DependentFunctionType>(
            loc, pi->getParamName(), std::move(paramCopy), std::move(returnCopy));
    }
    
    // Copy dependent pair type (Sigma)
    if (auto* sigma = dynamic_cast<ast::DependentPairType*>(type)) {
        std::unique_ptr<ast::Type> varCopy = copyType(sigma->getVarType());
        std::unique_ptr<ast::Type> bodyCopy = copyType(sigma->getBodyType());
        if (!varCopy || !bodyCopy) {
            return nullptr;
        }
        return std::make_unique<ast::DependentPairType>(
            loc, sigma->getVarName(), std::move(varCopy), std::move(bodyCopy));
    }
    
    // Copy forall type
    if (auto* forall = dynamic_cast<ast::ForallType*>(type)) {
        std::unique_ptr<ast::Type> bodyCopy = copyType(forall->getBodyType());
        if (!bodyCopy) {
            return nullptr;
        }
        return std::make_unique<ast::ForallType>(
            loc, forall->getTypeVars(), std::move(bodyCopy));
    }
    
    // Copy existential type
    if (auto* ex = dynamic_cast<ast::ExistentialType*>(type)) {
        std::unique_ptr<ast::Type> varCopy = copyType(ex->getVarType());
        std::unique_ptr<ast::Type> bodyCopy = copyType(ex->getBodyType());
        if (!varCopy || !bodyCopy) {
            return nullptr;
        }
        return std::make_unique<ast::ExistentialType>(
            loc, ex->getVarName(), std::move(varCopy), std::move(bodyCopy));
    }

    // Copy parameterized types
    if (auto* param = dynamic_cast<ast::ParameterizedType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> argsCopy;
        for (const auto& arg : param->getTypeArgs()) {
            std::unique_ptr<ast::Type> argCopy = copyType(arg.get());
            if (!argCopy) {
                return nullptr;
            }
            argsCopy.push_back(std::move(argCopy));
        }
        return std::make_unique<ast::ParameterizedType>(
            loc, param->getBaseName(), std::move(argsCopy)
        );
    }
    
    // Copy record types
    if (auto* rec = dynamic_cast<ast::RecordType*>(type)) {
        std::vector<std::unique_ptr<ast::RecordField>> fieldsCopy;
        for (const auto& field : rec->getFields()) {
            std::unique_ptr<ast::Type> fieldCopy = copyType(field->getType());
            if (!fieldCopy) {
                return nullptr;
            }
            fieldsCopy.push_back(std::make_unique<ast::RecordField>(
                field->getLocation(), field->getName(), std::move(fieldCopy)));
        }
        return std::make_unique<ast::RecordType>(loc, std::move(fieldsCopy));
    }

    // Copy ADT types (sum types)
    if (auto* adt = dynamic_cast<ast::ADTType*>(type)) {
        std::vector<std::unique_ptr<ast::Constructor>> constructorsCopy;
        for (const auto& c : adt->getConstructors()) {
            if (!c) continue;
            std::vector<std::unique_ptr<ast::Type>> argTypesCopy;
            for (const auto& arg : c->getArgumentTypes()) {
                std::unique_ptr<ast::Type> ac;
                if (auto* gen = dynamic_cast<ast::GenericType*>(arg.get())) {
                    if (gen->getName() == adt->getName()) {
                        ac = std::make_unique<ast::GenericType>(loc, gen->getName());
                    } else {
                        ac = copyType(arg.get());
                    }
                } else {
                    ac = copyType(arg.get());
                }
                if (!ac) return nullptr;
                argTypesCopy.push_back(std::move(ac));
            }
            constructorsCopy.push_back(std::make_unique<ast::Constructor>(
                c->getLocation(), c->getName(), std::move(argTypesCopy)));
        }
        return std::make_unique<ast::ADTType>(loc, adt->getName(), std::move(constructorsCopy));
    }

    // Copy function types
    if (auto* func = dynamic_cast<ast::FunctionType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> paramTypesCopy;
        for (const auto& paramType : func->getParamTypes()) {
            std::unique_ptr<ast::Type> paramCopy = copyType(paramType.get());
            if (!paramCopy) {
                return nullptr;
            }
            paramTypesCopy.push_back(std::move(paramCopy));
        }
        
        std::unique_ptr<ast::Type> returnTypeCopy = copyType(func->getReturnType());
        if (!returnTypeCopy) {
            return nullptr;
        }
        
        return std::make_unique<ast::FunctionType>(
            loc, std::move(paramTypesCopy), std::move(returnTypeCopy), func->isInteraction()
        );
    }
    
    // Unknown type - return nullptr
    return nullptr;
}

std::unique_ptr<ast::Type> TypeChecker::substituteType(ast::Type* type,
    const std::map<std::string, ast::Type*>& substitutions) {
    if (!type) return nullptr;
    SourceLocation loc = type->getLocation();
    if (auto* gen = dynamic_cast<ast::GenericType*>(type)) {
        auto it = substitutions.find(gen->getName());
        if (it != substitutions.end())
            return copyType(it->second);
        return std::make_unique<ast::GenericType>(loc, gen->getName());
    }
    if (auto* param = dynamic_cast<ast::ParameterizedType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> args;
        for (const auto& arg : param->getTypeArgs()) {
            auto a = substituteType(arg.get(), substitutions);
            if (!a) return nullptr;
            args.push_back(std::move(a));
        }
        return std::make_unique<ast::ParameterizedType>(loc, param->getBaseName(), std::move(args));
    }
    if (auto* arr = dynamic_cast<ast::ArrayType*>(type)) {
        auto e = substituteType(arr->getElementType(), substitutions);
        if (!e) return nullptr;
        return std::make_unique<ast::ArrayType>(loc, std::move(e));
    }
    if (auto* rec = dynamic_cast<ast::RecordType*>(type)) {
        std::vector<std::unique_ptr<ast::RecordField>> fields;
        for (const auto& f : rec->getFields()) {
            auto ft = substituteType(f->getType(), substitutions);
            if (!ft) return nullptr;
            fields.push_back(std::make_unique<ast::RecordField>(f->getLocation(), f->getName(), std::move(ft)));
        }
        return std::make_unique<ast::RecordType>(loc, std::move(fields));
    }
    if (auto* adt = dynamic_cast<ast::ADTType*>(type)) {
        std::vector<std::unique_ptr<ast::Constructor>> constructors;
        for (const auto& c : adt->getConstructors()) {
            if (!c) continue;
            std::vector<std::unique_ptr<ast::Type>> argTypes;
            for (const auto& a : c->getArgumentTypes()) {
                auto at = substituteType(a.get(), substitutions);
                if (!at) return nullptr;
                argTypes.push_back(std::move(at));
            }
            constructors.push_back(std::make_unique<ast::Constructor>(
                c->getLocation(), c->getName(), std::move(argTypes)));
        }
        return std::make_unique<ast::ADTType>(loc, adt->getName(), std::move(constructors));
    }
    if (auto* func = dynamic_cast<ast::FunctionType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> paramTypes;
        for (const auto& p : func->getParamTypes()) {
            auto pt = substituteType(p.get(), substitutions);
            if (!pt) return nullptr;
            paramTypes.push_back(std::move(pt));
        }
        auto ret = substituteType(func->getReturnType(), substitutions);
        if (!ret) return nullptr;
        return std::make_unique<ast::FunctionType>(
            loc, std::move(paramTypes), std::move(ret), func->isInteraction());
    }
    return copyType(type);
}

std::unique_ptr<ast::Type> TypeChecker::resolveParameterizedType(ast::ParameterizedType* type) {
    if (!type) return nullptr;
    const std::string& baseName = type->getBaseName();
    const auto& typeArgs = type->getTypeArgs();
    auto it = typeDecls_.find(baseName);
    if (it == typeDecls_.end() || !it->second) return nullptr;
    auto pit = typeDeclParams_.find(baseName);
    if (pit == typeDeclParams_.end() || pit->second.empty()) return nullptr;
    const std::vector<std::string>& params = pit->second;
    if (params.size() != typeArgs.size()) {
        errorReporter_.error(
            type->getLocation(),
            "Type '" + baseName + "' expects " + std::to_string(params.size()) +
            " type argument(s), got " + std::to_string(typeArgs.size()));
        return nullptr;
    }
    std::map<std::string, ast::Type*> substitutions;
    for (size_t i = 0; i < params.size(); ++i) {
        ast::Type* arg = typeArgs[i] ? typeArgs[i].get() : nullptr;
        if (!arg) return nullptr;
        substitutions[params[i]] = arg;
    }
    return substituteType(it->second, substitutions);
}

ast::Type* TypeChecker::inferConstructor(ast::ConstructorExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    const std::string& name = expr->getConstructorName();
    auto cit = constructorToADT_.find(name);
    if (cit == constructorToADT_.end()) {
        errorReporter_.error(
            expr->getLocation(),
            "Unknown constructor: " + name
        );
        return nullptr;
    }
    ast::ADTType* adt = cit->second;
    ast::Constructor* constructor = nullptr;
    for (const auto& c : adt->getConstructors()) {
        if (c && c->getName() == name) {
            constructor = c.get();
            break;
        }
    }
    if (!constructor) {
        return nullptr;
    }
    std::string typeName;
    for (const auto& p : typeDecls_) {
        if (p.second == adt) {
            typeName = p.first;
            break;
        }
    }
    const auto& expected = constructor->getArgumentTypes();
    const auto& args = expr->getArguments();
    if (args.size() != expected.size()) {
        errorReporter_.error(expr->getLocation(),
            "Constructor " + name + " expects " + std::to_string(expected.size()) +
            " argument(s), got " + std::to_string(args.size()));
        return nullptr;
    }
    const bool isGenericType = !typeName.empty() && typeDeclParams_.count(typeName) && !typeDeclParams_[typeName].empty();
    std::set<std::string> typeParamNames;
    if (isGenericType) {
        for (const std::string& p : typeDeclParams_[typeName])
            typeParamNames.insert(p);
    }
    std::map<std::string, ast::Type*> inferredParams;
    for (size_t i = 0; i < args.size(); ++i) {
        ast::Type* argType = inferType(args[i].get());
        if (!argType) return nullptr;
        ast::Type* expectedType = expected[i].get();
        if (auto* gen = dynamic_cast<ast::GenericType*>(expectedType)) {
            inferredParams[gen->getName()] = argType;
            if (!isGenericType || typeParamNames.count(gen->getName()) == 0) {
                auto it = typeDecls_.find(gen->getName());
                if (it != typeDecls_.end() && it->second) expectedType = it->second;
            }
        }
        // Do not infer type params from compound types (e.g. List<T>) to avoid inferring T from null
        std::unique_ptr<ast::Type> expectedCopy;
        if (!inferredParams.empty())
            expectedCopy = substituteType(expectedType, inferredParams);
        if (!expectedCopy)
            expectedCopy = copyType(expectedType);
        if (expectedCopy && !isAssignable(argType, expectedCopy.get())) {
            errorReporter_.error(args[i]->getLocation(),
                "Constructor " + name + " argument " + std::to_string(i + 1) + " type mismatch");
            return nullptr;
        }
    }
    // If this ADT is a generic type (e.g. List<T>), return ParameterizedType with inferred args
    if (!typeName.empty()) {
        auto pit = typeDeclParams_.find(typeName);
        if (pit != typeDeclParams_.end() && !pit->second.empty()) {
            const auto& params = pit->second;
            std::vector<std::unique_ptr<ast::Type>> typeArgs;
            for (const std::string& pname : params) {
                auto it = inferredParams.find(pname);
                if (it == inferredParams.end()) break;
                std::unique_ptr<ast::Type> a = copyType(it->second);
                if (!a) break;
                typeArgs.push_back(std::move(a));
            }
            if (typeArgs.size() == params.size())
                return new ast::ParameterizedType(expr->getLocation(), typeName, std::move(typeArgs));
        }
    }
    return copyType(adt).release();
}

ast::Type* TypeChecker::inferMatch(ast::MatchExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    // Infer type of matched expression
    ast::Type* matchedType = inferType(expr->getMatchedExpr());
    if (!matchedType) {
        return nullptr;
    }

    // Check that all patterns match the matched expression type
    // Infer return type from case bodies (should all be the same)
    // Keep a copy of the first body type so we don't hold a pointer into a scope that gets exited.
    matchReturnType_.reset();
    ast::Type* returnType = nullptr;
    
    for (const auto& matchCase : expr->getCases()) {
        // Check pattern matches matched type
        if (auto* recType = dynamic_cast<ast::RecordType*>(matchedType)) {
            if (auto* recPat = dynamic_cast<ast::RecordPattern*>(matchCase->getPattern())) {
                for (const auto& fp : recPat->getFields()) {
                    ast::RecordField* field = recType->getField(fp.name);
                    if (!field) {
                        errorReporter_.error(
                            matchCase->getPattern()->getLocation(),
                            "Record pattern references unknown field '" + fp.name + "'"
                        );
                        continue;
                    }
                    // Minimal nested checking: literal patterns must be type-compatible.
                    if (auto* litPat = dynamic_cast<ast::LiteralPattern*>(fp.pattern.get())) {
                        ast::Type* ft = field->getType();
                        ast::Type* lt = inferType(litPat->getLiteral());
                        if (ft && lt && !isAssignable(lt, ft)) {
                            errorReporter_.error(
                                litPat->getLocation(),
                                "Record pattern field '" + fp.name + "' literal has incompatible type"
                            );
                        }
                    }
                }
            }
        }
        
        // Enter scope and bind pattern variables so the body can reference them
        symbolTable_.enterScope();
        bindPatternVariables(matchCase->getPattern(), matchedType);
        // Infer body type (may point into current scope's symbols)
        ast::Type* bodyType = inferType(matchCase->getBody());
        // Copy before exiting scope so we don't use-after-free
        std::unique_ptr<ast::Type> bodyTypeCopy = bodyType ? copyType(bodyType) : nullptr;
        symbolTable_.exitScope();
        if (!bodyTypeCopy) {
            continue;
        }
        
        if (!returnType) {
            matchReturnType_ = std::move(bodyTypeCopy);
            returnType = matchReturnType_.get();
        } else {
            // All case bodies should have the same type, or unify as optional (T | null)
            if (!typesEqual(returnType, bodyTypeCopy.get())) {
                auto isNull = [](ast::Type* t) {
                    auto* p = dynamic_cast<ast::PrimitiveType*>(t);
                    return p && p->getKind() == ast::PrimitiveType::Kind::Null;
                };
                auto isOptionalWithNull = [](ast::Type* t) {
                    auto* param = dynamic_cast<ast::ParameterizedType*>(t);
                    if (!param || param->getBaseName() != "|" || param->getTypeArgs().size() != 2) return false;
                    for (const auto& a : param->getTypeArgs())
                        if (a.get()) {
                            auto* prim = dynamic_cast<ast::PrimitiveType*>(a.get());
                            if (prim && prim->getKind() == ast::PrimitiveType::Kind::Null) return true;
                        }
                    return false;
                };
                if (isNull(bodyTypeCopy.get())) {
                    if (isOptionalWithNull(returnType)) {
                        // Already T | null, null branch is fine
                    } else {
                        std::vector<std::unique_ptr<ast::Type>> args;
                        args.push_back(copyType(returnType));
                        args.push_back(std::make_unique<ast::PrimitiveType>(returnType->getLocation(), ast::PrimitiveType::Kind::Null));
                        matchReturnType_ = std::make_unique<ast::ParameterizedType>(returnType->getLocation(), "|", std::move(args));
                        returnType = matchReturnType_.get();
                    }
                } else if (isNull(returnType)) {
                    std::vector<std::unique_ptr<ast::Type>> args;
                    args.push_back(std::move(bodyTypeCopy));
                    args.push_back(std::make_unique<ast::PrimitiveType>(matchCase->getBody()->getLocation(), ast::PrimitiveType::Kind::Null));
                    matchReturnType_ = std::make_unique<ast::ParameterizedType>(matchCase->getBody()->getLocation(), "|", std::move(args));
                    returnType = matchReturnType_.get();
                    bodyTypeCopy = nullptr; // moved
                } else {
                    errorReporter_.error(
                        matchCase->getBody()->getLocation(),
                        "Match case body type mismatch"
                    );
                }
            }
        }
    }
    
    // Exhaustiveness: if matched type is ADT
    // - "Sum of records" (each constructor has one arg that is a record type): require a default case.
    // - "Case class" style: require all constructors covered (catch-all also satisfies).
    if (auto* adtType = dynamic_cast<ast::ADTType*>(matchedType)) {
        std::set<std::string> covered;
        bool hasCatchAll = false;
        for (const auto& matchCase : expr->getCases()) {
            ast::Pattern* p = matchCase->getPattern();
            if (!p) continue;
            if (auto* cp = dynamic_cast<ast::ConstructorPattern*>(p)) {
                const std::string& name = cp->getConstructorName();
                for (const auto& c : adtType->getConstructors()) {
                    if (c->getName() == name) { covered.insert(name); break; }
                }
            } else if (dynamic_cast<ast::VariablePattern*>(p) || dynamic_cast<ast::WildcardPattern*>(p)) {
                hasCatchAll = true;
                break;
            } else if (auto* ap = dynamic_cast<ast::AsPattern*>(p)) {
                ast::Pattern* inner = ap->getPattern();
                if (inner && (dynamic_cast<ast::VariablePattern*>(inner) || dynamic_cast<ast::WildcardPattern*>(inner))) {
                    hasCatchAll = true;
                    break;
                }
                if (auto* innerCp = dynamic_cast<ast::ConstructorPattern*>(inner)) {
                    const std::string& name = innerCp->getConstructorName();
                    for (const auto& c : adtType->getConstructors()) {
                        if (c->getName() == name) { covered.insert(name); break; }
                    }
                }
            }
        }
        bool allConstructorsTakeRecord = true;
        for (const auto& c : adtType->getConstructors()) {
            if (c->getArgumentTypes().size() != 1) { allConstructorsTakeRecord = false; break; }
            ast::Type* arg = c->getArgumentTypes()[0].get();
            if (auto* gen = dynamic_cast<ast::GenericType*>(arg)) {
                auto it = typeDecls_.find(gen->getName());
                if (it == typeDecls_.end() || !dynamic_cast<ast::RecordType*>(it->second)) {
                    allConstructorsTakeRecord = false;
                    break;
                }
            } else if (!dynamic_cast<ast::RecordType*>(arg)) {
                allConstructorsTakeRecord = false;
                break;
            }
        }
        if (allConstructorsTakeRecord) {
            // Sum-of-records: require default case only when not exhaustive
            if (!hasCatchAll && covered.size() < adtType->getConstructors().size()) {
                errorReporter_.error(
                    expr->getLocation(),
                    "Match on sum-of-records type requires a default case (variable or _ pattern) or all constructors covered"
                );
            }
        } else {
            if (!hasCatchAll) {
                for (const auto& c : adtType->getConstructors()) {
                    if (covered.find(c->getName()) == covered.end()) {
                        errorReporter_.error(
                            expr->getLocation(),
                            "Non-exhaustive match: constructor '" + c->getName() + "' is not covered"
                        );
                        break;
                    }
                }
            }
        }
    }

    return returnType;
}

void TypeChecker::bindPatternVariables(ast::Pattern* pattern, ast::Type* matchedType) {
    if (!pattern || !matchedType) {
        return;
    }
    // Resolve type alias (GenericType) so record/constructor pattern binding sees concrete types
    if (auto* gen = dynamic_cast<ast::GenericType*>(matchedType)) {
        auto it = typeDecls_.find(gen->getName());
        if (it != typeDecls_.end() && it->second) {
            matchedType = it->second;
        }
    }
    if (auto* vp = dynamic_cast<ast::VariablePattern*>(pattern)) {
        std::unique_ptr<ast::Type> typeCopy = copyType(matchedType);
        if (typeCopy) {
            symbolTable_.insert(std::make_unique<VariableSymbol>(
                vp->getName(), vp->getLocation(), std::move(typeCopy)));
        }
        return;
    }
    if (auto* rp = dynamic_cast<ast::RecordPattern*>(pattern)) {
        auto* recType = dynamic_cast<ast::RecordType*>(matchedType);
        if (!recType) {
            return;
        }
        for (const auto& fp : rp->getFields()) {
            ast::RecordField* field = recType->getField(fp.name);
            if (field && fp.pattern) {
                bindPatternVariables(fp.pattern.get(), field->getType());
            }
        }
        return;
    }
    if (auto* ap = dynamic_cast<ast::AsPattern*>(pattern)) {
        std::unique_ptr<ast::Type> typeCopy = copyType(matchedType);
        if (typeCopy) {
            symbolTable_.insert(std::make_unique<VariableSymbol>(
                ap->getName(), ap->getLocation(), std::move(typeCopy)));
        }
        if (ap->getPattern()) {
            bindPatternVariables(ap->getPattern(), matchedType);
        }
        return;
    }
    if (auto* cp = dynamic_cast<ast::ConstructorPattern*>(pattern)) {
        ast::Type* typeForCtor = matchedType;
        if (auto* param = dynamic_cast<ast::ParameterizedType*>(matchedType)) {
            // Use AST ADT for constructor lookup; substitute each arg type into local copies
            // so we never hold a pointer to a temporary resolved ADT (avoids use-after-free in dynamic_cast).
            const std::string& baseName = param->getBaseName();
            const auto& typeArgs = param->getTypeArgs();
            auto it = typeDecls_.find(baseName);
            auto pit = typeDeclParams_.find(baseName);
            if (it != typeDecls_.end() && it->second && pit != typeDeclParams_.end() &&
                !pit->second.empty() && pit->second.size() == typeArgs.size()) {
                ast::Type* baseType = it->second;
                if (!baseType) { typeForCtor = matchedType; }
                else {
                    auto* astADT = dynamic_cast<ast::ADTType*>(baseType);
                    if (astADT) {
                        std::map<std::string, ast::Type*> subs;
                        for (size_t i = 0; i < pit->second.size(); ++i)
                            if (typeArgs[i]) subs[pit->second[i]] = typeArgs[i].get();
                        ast::Constructor* ctor = nullptr;
                        for (const auto& c : astADT->getConstructors()) {
                            if (c && c->getName() == cp->getConstructorName()) {
                                ctor = c.get();
                                break;
                            }
                        }
                        if (ctor) {
                            const auto& argTypes = ctor->getArgumentTypes();
                            const auto& argPatterns = cp->getArguments();
                            std::vector<std::unique_ptr<ast::Type>> substitutedArgTypes;
                            for (size_t i = 0; i < argTypes.size() && i < argPatterns.size(); ++i) {
                                if (!argPatterns[i] || !argTypes[i]) continue;
                                ast::Type* argTy = argTypes[i].get();
                                if (!argTy) continue;
                                auto sub = substituteType(argTy, subs);
                                if (sub)
                                    substitutedArgTypes.push_back(std::move(sub));
                            }
                            if (substitutedArgTypes.size() == argPatterns.size()) {
                                for (size_t i = 0; i < substitutedArgTypes.size(); ++i)
                                    bindPatternVariables(argPatterns[i].get(), substitutedArgTypes[i].get());
                                return;
                            }
                        }
                    }
                }
            }
        }
        auto* adtType = dynamic_cast<ast::ADTType*>(typeForCtor);
        if (!adtType) return;
        ast::Constructor* constructor = nullptr;
        for (const auto& c : adtType->getConstructors()) {
            if (c && c->getName() == cp->getConstructorName()) {
                constructor = c.get();
                break;
            }
        }
        if (!constructor) return;
        const auto& argTypes = constructor->getArgumentTypes();
        const auto& argPatterns = cp->getArguments();
        for (size_t i = 0; i < argTypes.size() && i < argPatterns.size(); ++i) {
            if (argPatterns[i] && argTypes[i])
                bindPatternVariables(argPatterns[i].get(), argTypes[i].get());
        }
        return;
    }
    // WildcardPattern, LiteralPattern: no variable bindings
}

bool TypeChecker::matchFunctionSignature(ast::FunctionDecl* func, const std::vector<ast::Type*>& argTypes) {
    if (!func) {
        return false;
    }
    
    const auto& params = func->getParameters();
    
    // Check argument count
    if (params.size() != argTypes.size()) {
        return false;
    }
    
    // Check each argument type
    for (size_t i = 0; i < params.size(); ++i) {
        ast::Type* paramType = params[i]->getType();
        ast::Type* argType = argTypes[i];
        
        if (!paramType || !argType) {
            return false;
        }
        
        // Check if argument type is assignable to parameter type
        if (!isAssignable(argType, paramType)) {
            return false;
        }
    }
    
    return true;
}

bool TypeChecker::matchFunctionSignature(ast::InteractionDecl* interaction, const std::vector<ast::Type*>& argTypes) {
    if (!interaction) {
        return false;
    }
    
    const auto& params = interaction->getParameters();
    
    // Check argument count
    if (params.size() != argTypes.size()) {
        return false;
    }
    
    // Check each argument type
    for (size_t i = 0; i < params.size(); ++i) {
        ast::Type* paramType = params[i]->getType();
        ast::Type* argType = argTypes[i];
        
        if (!paramType || !argType) {
            return false;
        }
        
        // Check if argument type is assignable to parameter type
        if (!isAssignable(argType, paramType)) {
            return false;
        }
    }
    
    return true;
}

} // namespace semantic
} // namespace first
