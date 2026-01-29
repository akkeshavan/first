#include "first/semantic/type_checker.h"
#include "first/ast/declarations.h"
#include "first/ast/statements.h"
#include "first/ast/expressions.h"
#include "first/ast/match_case.h"
#include "first/semantic/module_resolver.h"
#include <sstream>

namespace first {
namespace semantic {

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
}

void TypeChecker::checkInteraction(ast::InteractionDecl* interaction) {
    if (!interaction) return;
    
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
    }
    // TODO: Add array literal inference, record construction, etc.
    
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
            return nullptr; // Null type - will need proper handling later
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

ast::Type* TypeChecker::inferVariable(ast::VariableExpr* expr) {
    Symbol* symbol = symbolTable_.lookup(expr->getName());
    
    if (!symbol) {
        errorReporter_.error(
            expr->getLocation(),
            "Undefined variable: " + expr->getName()
        );
        return nullptr;
    }
    
    if (symbol->getKind() != SymbolKind::Variable) {
        errorReporter_.error(
            expr->getLocation(),
            "Expected variable, found: " + expr->getName()
        );
        return nullptr;
    }
    
    VariableSymbol* varSymbol = dynamic_cast<VariableSymbol*>(symbol);
    if (varSymbol && varSymbol->getType()) {
        return varSymbol->getType(); // Return non-owning pointer
    }
    
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
    // Array
    if (name == "arrayLength") {
        if (n != 1) return err("arrayLength(arr) expects 1 argument");
        if (!arg(0)) return nullptr;
        return createIntType().release();
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
    // Look up function
    std::vector<Symbol*> functions = symbolTable_.lookupAll(expr->getName());
    
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
    
    ast::Type* returnType = matchedFunc->getReturnType();
    if (returnType) {
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

    // Compare primitive types
    auto* prim1 = dynamic_cast<ast::PrimitiveType*>(type1);
    auto* prim2 = dynamic_cast<ast::PrimitiveType*>(type2);
    
    if (prim1 && prim2) {
        return prim1->getKind() == prim2->getKind();
    }
    
    // Compare array types
    auto* arr1 = dynamic_cast<ast::ArrayType*>(type1);
    auto* arr2 = dynamic_cast<ast::ArrayType*>(type2);
    
    if (arr1 && arr2) {
        return typesEqual(arr1->getElementType(), arr2->getElementType());
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
    
    // Compare parameterized types
    auto* param1 = dynamic_cast<ast::ParameterizedType*>(type1);
    auto* param2 = dynamic_cast<ast::ParameterizedType*>(type2);
    
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
    
    return false;
}

bool TypeChecker::isAssignable(ast::Type* from, ast::Type* to) {
    if (!from || !to) {
        return false;
    }
    from = getEffectiveType(from);
    to = getEffectiveType(to);

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
    // This is more restrictive but safer - we don't allow Array<Int> -> Array<Float>
    auto* fromArr = dynamic_cast<ast::ArrayType*>(from);
    auto* toArr = dynamic_cast<ast::ArrayType*>(to);
    
    if (fromArr && toArr) {
        return typesEqual(fromArr->getElementType(), toArr->getElementType());
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
    
    // Copy generic types
    if (auto* gen = dynamic_cast<ast::GenericType*>(type)) {
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

ast::Type* TypeChecker::inferConstructor(ast::ConstructorExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    // Look up constructor in symbol table
    // For now, we'll need to track ADT types and their constructors
    // TODO: Implement proper ADT constructor lookup
    // This is a placeholder - will be implemented when ADT type declarations are added
    
    errorReporter_.error(
        expr->getLocation(),
        "Constructor type inference not yet implemented: " + expr->getConstructorName()
    );
    
    return nullptr;
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
    ast::Type* returnType = nullptr;
    
    for (const auto& matchCase : expr->getCases()) {
        // Check pattern matches matched type
        // TODO: Implement pattern type checking
        
        // Infer body type
        ast::Type* bodyType = inferType(matchCase->getBody());
        if (!bodyType) {
            continue;
        }
        
        if (!returnType) {
            returnType = bodyType;
        } else {
            // All case bodies should have the same type
            if (!typesEqual(returnType, bodyType)) {
                errorReporter_.error(
                    matchCase->getBody()->getLocation(),
                    "Match case body type mismatch"
                );
            }
        }
    }
    
    // TODO: Check exhaustiveness
    
    return returnType;
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
