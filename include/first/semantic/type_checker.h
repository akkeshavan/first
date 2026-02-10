#pragma once

#include "first/ast/node.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/patterns.h"
#include "first/ast/program.h"
#include "first/semantic/symbol_table.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include <map>
#include <memory>
#include <set>
#include <string>

namespace first {
namespace semantic {

class ModuleResolver;

// Type checker for semantic analysis
class TypeChecker {
public:
    TypeChecker(ErrorReporter& errorReporter)
        : errorReporter_(errorReporter)
        , symbolTable_()
        , moduleResolver_(nullptr)
        , currentProgram_(nullptr) {}

    void setModuleResolver(ModuleResolver* resolver) { moduleResolver_ = resolver; }
    
    // Main entry point: type check a program
    bool check(ast::Program* program);
    
    // Type inference: infer the type of an expression
    ast::Type* inferType(ast::Expr* expr);
    
    // Type checking: check if expression has expected type
    bool checkType(ast::Expr* expr, ast::Type* expectedType);
    
    // Check if two types are equal
    bool typesEqual(ast::Type* type1, ast::Type* type2);
    
    // Check if a type is assignable to another (subtyping)
    bool isAssignable(ast::Type* from, ast::Type* to);
    
    // Type copying helper (creates a deep copy of a type) - public for testing
    std::unique_ptr<ast::Type> copyType(ast::Type* type);
    
    // Function signature matching - public for testing
    bool matchFunctionSignature(ast::FunctionDecl* func, const std::vector<ast::Type*>& argTypes);
    bool matchFunctionSignature(ast::InteractionDecl* interaction, const std::vector<ast::Type*>& argTypes);
    // Same parameter types (for redefinition check)
    bool sameParameterTypes(ast::FunctionDecl* a, ast::FunctionDecl* b);

private:
    ErrorReporter& errorReporter_;
    SymbolTable symbolTable_;
    ModuleResolver* moduleResolver_;
    ast::Program* currentProgram_;
    // Type declarations: type Name = ... (alias or sum type)
    std::map<std::string, ast::Type*> typeDecls_;
    // For generic types (e.g. List<T>): type name -> type parameter names
    std::map<std::string, std::vector<std::string>> typeDeclParams_;
    // Constructor name -> ADT type (for constructor call type-checking)
    std::map<std::string, ast::ADTType*> constructorToADT_;
    // Current function/interaction being type-checked (for generic constraint context)
    ast::FunctionDecl* currentFunction_ = nullptr;
    ast::InteractionDecl* currentInteraction_ = nullptr;
    // Owned copy of match expression return type (body types can point into exited scope)
    std::unique_ptr<ast::Type> matchReturnType_;
    // Substituted return types for generic function calls (so inferred type is concrete, e.g. List<Int> not List<T>)
    std::vector<std::unique_ptr<ast::Type>> substitutedReturnTypes_;

    // Eq, Ord, Iterator: check if a type has an implementation of the given interface
    bool typeImplementsInterface(ast::Type* type, const std::string& interfaceName);
    // Return the function name bound to interface member (e.g. "eq" for Eq, "compare" for Ord), or empty if none
    std::string getImplementationMemberFunctionName(ast::Type* type, const std::string& interfaceName, const std::string& memberName);

    // Resolve ParameterizedType to expanded type by substituting type decl body (for user-defined generics)
    std::unique_ptr<ast::Type> resolveParameterizedType(ast::ParameterizedType* type);
    // Substitute type parameters in a type (GenericType -> replacement)
    std::unique_ptr<ast::Type> substituteType(ast::Type* type,
        const std::map<std::string, ast::Type*>& substitutions);
    
    // Helper methods
    void checkProgram(ast::Program* program);
    void checkFunction(ast::FunctionDecl* func);
    void checkInteraction(ast::InteractionDecl* interaction);
    void checkImplementation(ast::ImplementationDecl* impl);
    void checkStatement(ast::Stmt* stmt);
    ast::Type* inferExpression(ast::Expr* expr);
    ast::Type* inferLiteral(ast::LiteralExpr* expr);
    ast::Type* inferBinary(ast::BinaryExpr* expr);
    ast::Type* inferUnary(ast::UnaryExpr* expr);
    ast::Type* inferVariable(ast::VariableExpr* expr);
    ast::Type* inferFunctionCall(ast::FunctionCallExpr* expr);
    ast::Type* inferStdlibCall(ast::FunctionCallExpr* expr);  // Phase 7.3: built-in stdlib
    ast::Type* inferConstructor(ast::ConstructorExpr* expr);
    ast::Type* inferMatch(ast::MatchExpr* expr);
    ast::Type* inferArrayLiteral(ast::ArrayLiteralExpr* expr);
    ast::Type* inferArrayIndex(ast::ArrayIndexExpr* expr);
    ast::Type* inferRecordLiteral(ast::RecordLiteralExpr* expr);
    ast::Type* inferFieldAccess(ast::FieldAccessExpr* expr);
    ast::Type* inferMethodCall(ast::MethodCallExpr* expr);
    ast::Type* inferBlockExpr(ast::BlockExpr* expr);
    ast::Type* inferIfExpr(ast::IfExpr* expr);
    ast::Type* inferRangeExpr(ast::RangeExpr* expr);

    // For for-in: get element type if iterable (Range -> Int, Array<T> -> T)
    ast::Type* getIterableElementType(ast::Expr* iterable);

    // Bind variables introduced by a pattern (for match case body type-checking)
    void bindPatternVariables(ast::Pattern* pattern, ast::Type* matchedType);

    // Type checking helpers
    bool checkExpression(ast::Expr* expr, ast::Type* expectedType);
    
    // Generic functions: every type parameter must appear in at least one parameter type
    // (no separate generic used only in return type)
    void collectTypeParamNamesInType(ast::Type* type, std::set<std::string>& out);
    void checkGenericParamsAppearInParameterTypes(ast::FunctionDecl* func);
    void checkGenericParamsAppearInParameterTypes(ast::InteractionDecl* interaction);

    // ArrayBuf is mutable and only allowed in interactions
    bool typeContainsArrayBuf(ast::Type* type);

    // Higher-kinded types: get type constructor name (for ParameterizedType or GenericType)
    static std::string getTypeConstructorName(ast::Type* type);
    ast::InterfaceDecl* findInterface(const std::string& name);

    // Error reporting helpers
    void reportTypeError(const SourceLocation& loc, 
                        const std::string& message,
                        ast::Type* actualType = nullptr,
                        ast::Type* expectedType = nullptr);
    
    // Type creation helpers
    std::unique_ptr<ast::Type> createPrimitiveType(ast::PrimitiveType::Kind kind);
    std::unique_ptr<ast::Type> createIntType();
    std::unique_ptr<ast::Type> createFloatType();
    std::unique_ptr<ast::Type> createBoolType();
    std::unique_ptr<ast::Type> createStringType();
    std::unique_ptr<ast::Type> createUnitType();
    std::unique_ptr<ast::Type> createNullType();
    std::unique_ptr<ast::Type> createArrayBufType();
    std::unique_ptr<ast::Type> createArrayType(std::unique_ptr<ast::Type> elementType);
    std::unique_ptr<ast::Type> createFunctionType(
        std::vector<std::unique_ptr<ast::Type>> paramTypes,
        std::unique_ptr<ast::Type> returnType,
        bool isInteraction = false
    );
};

} // namespace semantic
} // namespace first
