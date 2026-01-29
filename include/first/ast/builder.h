#pragma once

#include "first/ast/node.h"
#include "first/ast/program.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/declarations.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "FirstParser.h"
#include <memory>

namespace first {
namespace ast {

// AST builder - converts ANTLR parse tree to AST
class ASTBuilder {
public:
    ASTBuilder(ErrorReporter& errorReporter, const std::string& filename)
        : errorReporter_(errorReporter), filename_(filename) {}

    // Build AST from parse tree
    std::unique_ptr<Program> buildProgram(FirstParser::ProgramContext* ctx);

    // Public methods for testing and incremental building
    // Helper to create source location from ANTLR context
    SourceLocation getLocation(antlr4::ParserRuleContext* ctx);
    
    // Expression builders
    std::unique_ptr<Expr> buildExpression(FirstParser::ExpressionContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::LogicalOrExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::LogicalAndExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::EqualityExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::ComparisonExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::AdditiveExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::MultiplicativeExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::MonadicExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::UnaryExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::PostfixExprContext* ctx);
    std::unique_ptr<Expr> buildExpression(FirstParser::PrimaryExprContext* ctx);
    std::unique_ptr<LiteralExpr> buildLiteral(FirstParser::LiteralContext* ctx);
    std::unique_ptr<ArrayLiteralExpr> buildArrayLiteral(FirstParser::ArrayLiteralContext* ctx);
    std::unique_ptr<RecordLiteralExpr> buildRecordLiteral(FirstParser::RecordLiteralContext* ctx);
    std::unique_ptr<LambdaExpr> buildLambdaExpr(FirstParser::LambdaExprContext* ctx);
    
    // Type builders
    std::unique_ptr<Type> buildType(FirstParser::Type_Context* ctx);
    std::unique_ptr<Type> buildType(FirstParser::PrimaryTypeContext* ctx);
    std::unique_ptr<PrimitiveType> buildPrimitiveType(FirstParser::BuiltinTypeContext* ctx);
    std::unique_ptr<Type> buildGenericType(FirstParser::GenericTypeContext* ctx);
    
    // Statement builders
    std::unique_ptr<Stmt> buildStatement(FirstParser::StatementContext* ctx);
    std::unique_ptr<VariableDecl> buildVariableDecl(FirstParser::VarDeclContext* ctx);
    std::unique_ptr<ASTNode> buildVariableDeclNode(FirstParser::VarDeclContext* ctx); // Helper for testing
    std::unique_ptr<ReturnStmt> buildReturnStmt(FirstParser::ReturnStmtContext* ctx);
    std::unique_ptr<IfStmt> buildIfStmt(FirstParser::IfStmtContext* ctx);
    std::unique_ptr<WhileStmt> buildWhileStmt(FirstParser::WhileStmtContext* ctx);
    std::unique_ptr<AssignmentStmt> buildAssignmentStmt(FirstParser::AssignmentContext* ctx);

private:
    ErrorReporter& errorReporter_;
    std::string filename_;
};

} // namespace ast
} // namespace first
