#include "first/semantic/derive_pass.h"
#include "first/ast/declarations.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/program.h"
#include "first/source_location.h"
#include <memory>
#include <string>
#include <vector>

namespace first {
namespace semantic {

using namespace ast;

static SourceLocation loc() { return SourceLocation(1, 1, "derive"); }

// Build left + right for string concatenation
static std::unique_ptr<Expr> concat(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right) {
    return std::make_unique<BinaryExpr>(loc(), BinaryExpr::Op::Add, std::move(left), std::move(right));
}

// Build toString(fieldAccess) as a call expression
static std::unique_ptr<Expr> toStringCall(std::unique_ptr<Expr> arg) {
    std::vector<std::unique_ptr<Expr>> args;
    args.push_back(std::move(arg));
    return std::make_unique<FunctionCallExpr>(loc(), "toString", std::move(args));
}

// Generate ToString implementation for a record type: helper function + ImplementationDecl
static void deriveToStringForRecord(Program* program, TypeDecl* typeDecl) {
    const std::string& typeName = typeDecl->getTypeName();
    Type* type = typeDecl->getType();
    auto* rec = dynamic_cast<RecordType*>(type);
    if (!rec || rec->getFields().empty()) return;

    const std::string paramName = "x";
    SourceLocation ploc = typeDecl->getLocation();

    // Parameter type: reference by name so type checker can resolve
    std::unique_ptr<Type> paramType = std::make_unique<GenericType>(ploc, typeName);
    auto param = std::make_unique<Parameter>(ploc, paramName, std::move(paramType));

    std::vector<std::unique_ptr<Parameter>> params;
    params.push_back(std::move(param));

    // Return type String
    std::unique_ptr<Type> retType = std::make_unique<PrimitiveType>(ploc, PrimitiveType::Kind::String);

    // Body: return "{ f1 = " + toString(x.f1) + ", f2 = " + toString(x.f2) + " }";
    std::unique_ptr<Expr> varX = std::make_unique<VariableExpr>(ploc, paramName);
    std::unique_ptr<Expr> result;
    std::string part = "{ ";
    for (size_t i = 0; i < rec->getFields().size(); ++i) {
        const std::string& fieldName = rec->getFields()[i]->getName();
        std::unique_ptr<Expr> varXCopy = std::make_unique<VariableExpr>(ploc, paramName);
        std::unique_ptr<Expr> fieldAcc = std::make_unique<FieldAccessExpr>(ploc, std::move(varXCopy), fieldName);
        std::unique_ptr<Expr> toStringArg = toStringCall(std::move(fieldAcc));
        std::unique_ptr<Expr> label = std::make_unique<LiteralExpr>(ploc, LiteralExpr::LiteralType::String, part + fieldName + " = ");
        if (!result)
            result = concat(std::move(label), std::move(toStringArg));
        else
            result = concat(std::move(result), concat(std::move(label), std::move(toStringArg)));
        part = ", ";
    }
    std::unique_ptr<Expr> closing = std::make_unique<LiteralExpr>(ploc, LiteralExpr::LiteralType::String, " }");
    result = concat(std::move(result), std::move(closing));

    std::vector<std::unique_ptr<Stmt>> body;
    body.push_back(std::make_unique<ReturnStmt>(ploc, std::move(result)));

    std::string helperName = "toString_" + typeName;
    auto func = std::make_unique<FunctionDecl>(ploc, helperName,
        std::vector<GenericParam>{}, std::move(params), std::move(retType), std::move(body), false);
    program->addFunction(std::move(func));

    // implementation ToString<TypeName> { toString = helperName; }
    std::unique_ptr<Expr> implValue = std::make_unique<VariableExpr>(ploc, helperName);
    std::vector<std::unique_ptr<ImplementationMember>> members;
    members.push_back(std::make_unique<ImplementationMember>(ploc, "toString", std::move(implValue)));

    std::vector<std::unique_ptr<Type>> implTypeArgs;
    implTypeArgs.push_back(std::make_unique<GenericType>(ploc, typeName));
    program->addImplementation(std::make_unique<ImplementationDecl>(ploc, "ToString", std::move(implTypeArgs), std::move(members)));
}

void runDerivePass(ast::Program* program) {
    if (!program) return;
    for (const auto& typeDecl : program->getTypeDecls()) {
        if (!typeDecl) continue;
        for (const std::string& iface : typeDecl->getDerivedInterfaces()) {
            if (iface == "ToString") {
                if (typeDecl->isGeneric()) continue; // TODO: derive ToString for generic types with constraint
                deriveToStringForRecord(program, typeDecl.get());
                break;
            }
            // Eq, Ord: reserved for future implementation
        }
    }
}

} // namespace semantic
} // namespace first
