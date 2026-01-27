#include "first/ast/builder.h"
#include <antlr4-runtime.h>

namespace first {
namespace ast {

SourceLocation ASTBuilder::getLocation(antlr4::ParserRuleContext* ctx) {
    if (ctx && ctx->getStart()) {
        return SourceLocation(
            static_cast<size_t>(ctx->getStart()->getLine()),
            static_cast<size_t>(ctx->getStart()->getCharPositionInLine() + 1),
            filename_
        );
    }
    return SourceLocation(1, 1, filename_);
}

std::unique_ptr<Program> ASTBuilder::buildProgram(FirstParser::ProgramContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    auto program = std::make_unique<Program>(loc);
    
    // TODO: Build top-level declarations
    // For now, return empty program
    
    return program;
}

std::unique_ptr<LiteralExpr> ASTBuilder::buildLiteral(FirstParser::LiteralContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Check which type of literal
    if (ctx->INT_LITERAL()) {
        std::string value = ctx->INT_LITERAL()->getText();
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::Int, value);
    } else if (ctx->FLOAT_LITERAL()) {
        std::string value = ctx->FLOAT_LITERAL()->getText();
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::Float, value);
    } else if (ctx->TRUE()) {
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::Bool, "true");
    } else if (ctx->FALSE()) {
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::Bool, "false");
    } else if (ctx->STRING_LITERAL()) {
        std::string value = ctx->STRING_LITERAL()->getText();
        // Remove quotes
        if (value.size() >= 2 && value[0] == '"' && value.back() == '"') {
            value = value.substr(1, value.size() - 2);
        }
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::String, value);
    } else if (ctx->getToken(FirstParser::NULL_, 0)) {
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::Null, "null");
    } else if (ctx->unitLiteral()) {
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::Null, "()");
    }
    
    return nullptr;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::ExpressionContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    // Expression starts with logicalOrExpr
    if (ctx->logicalOrExpr()) {
        return buildExpression(ctx->logicalOrExpr());
    }
    
    return nullptr;
}

// Helper to build from logicalOrExpr (and other expression levels)
// Helper to convert token to binary operator
static BinaryExpr::Op tokenToBinaryOp(antlr4::Token* token) {
    if (!token) return BinaryExpr::Op::Add;
    
    size_t type = token->getType();
    if (type == FirstParser::OR) return BinaryExpr::Op::Or;
    if (type == FirstParser::AND) return BinaryExpr::Op::And;
    if (type == FirstParser::EQ) return BinaryExpr::Op::Eq;
    if (type == FirstParser::NE) return BinaryExpr::Op::Ne;
    if (type == FirstParser::LT) return BinaryExpr::Op::Lt;
    if (type == FirstParser::LE) return BinaryExpr::Op::Le;
    if (type == FirstParser::GT) return BinaryExpr::Op::Gt;
    if (type == FirstParser::GE) return BinaryExpr::Op::Ge;
    if (type == FirstParser::PLUS) return BinaryExpr::Op::Add;
    if (type == FirstParser::MINUS) return BinaryExpr::Op::Sub;
    if (type == FirstParser::MUL) return BinaryExpr::Op::Mul;
    if (type == FirstParser::DIV) return BinaryExpr::Op::Div;
    if (type == FirstParser::MOD) return BinaryExpr::Op::Mod;
    
    return BinaryExpr::Op::Add; // Default
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::LogicalOrExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    auto exprs = ctx->logicalAndExpr(); // Returns by value
    if (exprs.empty()) {
        return nullptr;
    }
    
    // Start with first operand
    auto result = buildExpression(exprs[0]);
    
    // Chain remaining operands with OR operator (left-associative)
    for (size_t i = 1; i < exprs.size(); ++i) {
        auto right = buildExpression(exprs[i]);
        if (right && result) {
            SourceLocation loc = getLocation(ctx);
            auto left = std::move(result);
            result = std::make_unique<BinaryExpr>(
                loc, BinaryExpr::Op::Or, std::move(left), std::move(right)
            );
        }
    }
    
    return result;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::LogicalAndExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    auto exprs = ctx->equalityExpr(); // Returns by value
    if (exprs.empty()) {
        return nullptr;
    }
    
    auto result = buildExpression(exprs[0]);
    
    // Chain with AND operator
    for (size_t i = 1; i < exprs.size(); ++i) {
        auto right = buildExpression(exprs[i]);
        if (right && result) {
            SourceLocation loc = getLocation(ctx);
            auto left = std::move(result);
            result = std::make_unique<BinaryExpr>(
                loc, BinaryExpr::Op::And, std::move(left), std::move(right)
            );
        }
    }
    
    return result;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::EqualityExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    auto exprs = ctx->comparisonExpr(); // Returns by value
    if (exprs.empty()) {
        return nullptr;
    }
    
    auto result = buildExpression(exprs[0]);
    
    // Chain with equality operators
    for (size_t i = 1; i < exprs.size(); ++i) {
        auto right = buildExpression(exprs[i]);
        if (right && result) {
            // Find the operator token between expressions
            antlr4::Token* opToken = nullptr;
            if (i - 1 < ctx->EQ().size() + ctx->NE().size()) {
                // Determine which operator it is
                size_t eqCount = ctx->EQ().size();
                if (i - 1 < eqCount) {
                    opToken = ctx->EQ(i - 1)->getSymbol();
                } else {
                    opToken = ctx->NE(i - 1 - eqCount)->getSymbol();
                }
            }
            
            SourceLocation loc = getLocation(ctx);
            auto left = std::move(result);
            BinaryExpr::Op op = tokenToBinaryOp(opToken);
            result = std::make_unique<BinaryExpr>(
                loc, op, std::move(left), std::move(right)
            );
        }
    }
    
    return result;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::ComparisonExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    auto exprs = ctx->additiveExpr(); // Returns by value
    if (exprs.empty()) {
        return nullptr;
    }
    
    auto result = buildExpression(exprs[0]);
    
    // Chain with comparison operators
    for (size_t i = 1; i < exprs.size(); ++i) {
        auto right = buildExpression(exprs[i]);
        if (right && result) {
            // Find operator token
            antlr4::Token* opToken = nullptr;
            // Check LT, LE, GT, GE tokens
            size_t idx = i - 1;
            if (idx < ctx->LT().size()) {
                opToken = ctx->LT(idx)->getSymbol();
            } else if (idx - ctx->LT().size() < ctx->LE().size()) {
                opToken = ctx->LE(idx - ctx->LT().size())->getSymbol();
            } else if (idx - ctx->LT().size() - ctx->LE().size() < ctx->GT().size()) {
                opToken = ctx->GT(idx - ctx->LT().size() - ctx->LE().size())->getSymbol();
            } else {
                size_t offset = ctx->LT().size() + ctx->LE().size() + ctx->GT().size();
                opToken = ctx->GE(idx - offset)->getSymbol();
            }
            
            SourceLocation loc = getLocation(ctx);
            auto left = std::move(result);
            BinaryExpr::Op op = tokenToBinaryOp(opToken);
            result = std::make_unique<BinaryExpr>(
                loc, op, std::move(left), std::move(right)
            );
        }
    }
    
    return result;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::AdditiveExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    auto exprs = ctx->multiplicativeExpr(); // Returns by value
    if (exprs.empty()) {
        return nullptr;
    }
    
    auto result = buildExpression(exprs[0]);
    
    // Chain with additive operators (+, -)
    for (size_t i = 1; i < exprs.size(); ++i) {
        auto right = buildExpression(exprs[i]);
        if (right && result) {
            // Find operator token
            antlr4::Token* opToken = nullptr;
            size_t idx = i - 1;
            if (idx < ctx->PLUS().size()) {
                opToken = ctx->PLUS(idx)->getSymbol();
            } else {
                opToken = ctx->MINUS(idx - ctx->PLUS().size())->getSymbol();
            }
            
            SourceLocation loc = getLocation(ctx);
            auto left = std::move(result);
            BinaryExpr::Op op = tokenToBinaryOp(opToken);
            result = std::make_unique<BinaryExpr>(
                loc, op, std::move(left), std::move(right)
            );
        }
    }
    
    return result;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::MultiplicativeExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    auto exprs = ctx->monadicExpr(); // Returns by value
    if (exprs.empty()) {
        return nullptr;
    }
    
    auto result = buildExpression(exprs[0]);
    
    // Chain with multiplicative operators (*, /, %)
    for (size_t i = 1; i < exprs.size(); ++i) {
        auto right = buildExpression(exprs[i]);
        if (right && result) {
            // Find operator token
            antlr4::Token* opToken = nullptr;
            size_t idx = i - 1;
            if (idx < ctx->MUL().size()) {
                opToken = ctx->MUL(idx)->getSymbol();
            } else if (idx - ctx->MUL().size() < ctx->DIV().size()) {
                opToken = ctx->DIV(idx - ctx->MUL().size())->getSymbol();
            } else {
                size_t offset = ctx->MUL().size() + ctx->DIV().size();
                opToken = ctx->MOD(idx - offset)->getSymbol();
            }
            
            SourceLocation loc = getLocation(ctx);
            auto left = std::move(result);
            BinaryExpr::Op op = tokenToBinaryOp(opToken);
            result = std::make_unique<BinaryExpr>(
                loc, op, std::move(left), std::move(right)
            );
        }
    }
    
    return result;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::MonadicExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    // Monadic operators are only for interactions - for now, just handle unary expressions
    auto exprs = ctx->unaryExpr(); // Returns by value
    if (exprs.empty()) {
        return nullptr;
    }
    
    // For now, just return the first unary expression
    // TODO: Handle monadic operators (>>=, >>, <$>, <*>) when we implement interaction analysis
    return buildExpression(exprs[0]);
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::UnaryExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    // Check for unary operators
    if (ctx->NOT() || ctx->MINUS() || ctx->PLUS()) {
        // Handle unary expression - unaryExpr() returns a single context, not a vector
        if (ctx->unaryExpr()) {
            auto operand = buildExpression(ctx->unaryExpr());
            if (operand) {
                SourceLocation loc = getLocation(ctx);
                UnaryExpr::Op op = ctx->NOT() ? UnaryExpr::Op::Not : UnaryExpr::Op::Neg;
                return std::make_unique<UnaryExpr>(loc, op, std::move(operand));
            }
        }
        return nullptr;
    }
    
    // Otherwise, it's a postfix expression
    if (ctx->postfixExpr()) {
        return buildExpression(ctx->postfixExpr());
    }
    
    return nullptr;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::PostfixExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Check for primary expression (base case)
    if (ctx->primaryExpr()) {
        auto base = buildExpression(ctx->primaryExpr());
        if (!base) {
            return nullptr;
        }
        
        // Check for postfix operations
        // Array indexing: postfixExpr LBRACKET expression RBRACKET
        if (ctx->LBRACKET() && ctx->expression()) {
            // Build the index expression
            auto index = buildExpression(ctx->expression());
            if (index) {
                return std::make_unique<ArrayIndexExpr>(loc, std::move(base), std::move(index));
            }
        }
        
        // Function call: postfixExpr LPAREN expressionList? RPAREN
        if (ctx->LPAREN()) {
            // This is handled as FunctionCallExpr in the grammar
            // For now, return base - function calls are handled separately
            return base;
        }
        
        // Member access: postfixExpr DOT IDENTIFIER
        // TODO: Handle member access
        
        return base;
    }
    
    // Recursive case: postfixExpr with operations
    // Build the base expression first
    if (ctx->postfixExpr()) {
        auto base = buildExpression(ctx->postfixExpr());
        if (!base) {
            return nullptr;
        }
        
        // Check for array indexing
        if (ctx->LBRACKET() && ctx->expression()) {
            auto index = buildExpression(ctx->expression());
            if (index) {
                return std::make_unique<ArrayIndexExpr>(loc, std::move(base), std::move(index));
            }
        }
        
        // Check for function call
        if (ctx->LPAREN()) {
            // Function calls are handled separately
            return base;
        }
        
        return base;
    }
    
    return nullptr;
}

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::PrimaryExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Check for literal
    if (ctx->literal()) {
        return buildLiteral(ctx->literal());
    }
    
    // Check for identifier (variable)
    if (ctx->IDENTIFIER()) {
        std::string name = ctx->IDENTIFIER()->getText();
        return std::make_unique<VariableExpr>(loc, name);
    }
    
    // Check for parenthesized expression
    if (ctx->parenthesizedExpr()) {
        if (ctx->parenthesizedExpr()->expression()) {
            return buildExpression(ctx->parenthesizedExpr()->expression());
        }
    }
    
    // Check for array literal
    if (ctx->arrayLiteral()) {
        return buildArrayLiteral(ctx->arrayLiteral());
    }
    
    // TODO: Handle other primary expressions (records, constructors, lambdas, conditionals)
    
    return nullptr;
}

std::unique_ptr<Type> ASTBuilder::buildType(FirstParser::Type_Context* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Type_ can be many things - check for primaryType first
    if (ctx->primaryType()) {
        return buildType(ctx->primaryType());
    }
    
    // TODO: Handle other type constructs (refinement, dependent, etc.)
    
    return nullptr;
}

std::unique_ptr<Type> ASTBuilder::buildType(FirstParser::PrimaryTypeContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    // Check for builtin type
    if (ctx->builtinType()) {
        return buildPrimitiveType(ctx->builtinType());
    }
    
    // TODO: Handle other primary types (arrays, generics, etc.)
    
    return nullptr;
}

std::unique_ptr<PrimitiveType> ASTBuilder::buildPrimitiveType(FirstParser::BuiltinTypeContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    if (ctx->INT()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::Int);
    } else if (ctx->FLOAT()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::Float);
    } else if (ctx->BOOL()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::Bool);
    } else if (ctx->STRING()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::String);
    } else if (ctx->UNIT()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::Unit);
    }
    
    return nullptr;
}

std::unique_ptr<Stmt> ASTBuilder::buildStatement(FirstParser::StatementContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    // Check for variable declaration
    if (ctx->varDecl()) {
        return buildVariableDecl(ctx->varDecl());
    }
    
    // Check for return statement
    if (ctx->returnStmt()) {
        return buildReturnStmt(ctx->returnStmt());
    }
    
    // Check for if statement
    if (ctx->ifStmt()) {
        return buildIfStmt(ctx->ifStmt());
    }
    
    // Check for while statement
    if (ctx->whileStmt()) {
        return buildWhileStmt(ctx->whileStmt());
    }
    
    // Check for assignment statement
    if (ctx->assignment()) {
        return buildAssignmentStmt(ctx->assignment());
    }
    
    // Check for expression statement
    if (ctx->exprStmt()) {
        SourceLocation loc = getLocation(ctx->exprStmt());
        auto expr = buildExpression(ctx->exprStmt()->expression());
        if (expr) {
            return std::make_unique<ExprStmt>(loc, std::move(expr));
        }
    }
    
    // TODO: Handle other statement types (match, block, etc.)
    
    return nullptr;
}

// Helper to build variable declaration (returns ASTNode since VariableDecl doesn't inherit from Stmt)
std::unique_ptr<ASTNode> ASTBuilder::buildVariableDeclNode(FirstParser::VarDeclContext* ctx) {
    return buildVariableDecl(ctx);
}

std::unique_ptr<VariableDecl> ASTBuilder::buildVariableDecl(FirstParser::VarDeclContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Get variable name
    if (!ctx->IDENTIFIER()) {
        return nullptr;
    }
    std::string name = ctx->IDENTIFIER()->getText();
    
    // Determine mutability
    VariableDecl::Mutability mutability = VariableDecl::Mutability::Immutable;
    if (ctx->VAR()) {
        mutability = VariableDecl::Mutability::Mutable;
    }
    
    // Get type (optional)
    std::unique_ptr<Type> type = nullptr;
    if (ctx->type_()) {
        type = buildType(ctx->type_());
    }
    
    // Get initializer (optional)
    std::unique_ptr<Expr> initializer = nullptr;
    if (ctx->expression()) {
        initializer = buildExpression(ctx->expression());
    }
    
    return std::make_unique<VariableDecl>(
        loc, name, mutability, std::move(type), std::move(initializer)
    );
}

std::unique_ptr<ReturnStmt> ASTBuilder::buildReturnStmt(FirstParser::ReturnStmtContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    std::unique_ptr<Expr> value = nullptr;
    if (ctx->expression()) {
        value = buildExpression(ctx->expression());
    }
    
    return std::make_unique<ReturnStmt>(loc, std::move(value));
}

std::unique_ptr<IfStmt> ASTBuilder::buildIfStmt(FirstParser::IfStmtContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Build condition expression
    auto condition = buildExpression(ctx->expression());
    if (!condition) {
        return nullptr;
    }
    
    // Build then branch
    auto thenBranch = buildStatement(ctx->statement(0));
    if (!thenBranch) {
        return nullptr;
    }
    
    // Build else branch if present
    std::unique_ptr<Stmt> elseBranch = nullptr;
    if (ctx->statement().size() > 1) {
        elseBranch = buildStatement(ctx->statement(1));
    }
    
    return std::make_unique<IfStmt>(loc, std::move(condition), std::move(thenBranch), std::move(elseBranch));
}

std::unique_ptr<WhileStmt> ASTBuilder::buildWhileStmt(FirstParser::WhileStmtContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Build condition expression
    auto condition = buildExpression(ctx->expression());
    if (!condition) {
        return nullptr;
    }
    
    // Build body statement
    auto body = buildStatement(ctx->statement());
    if (!body) {
        return nullptr;
    }
    
    return std::make_unique<WhileStmt>(loc, std::move(condition), std::move(body));
}

std::unique_ptr<AssignmentStmt> ASTBuilder::buildAssignmentStmt(FirstParser::AssignmentContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Build target expression (left-hand side)
    auto target = buildExpression(ctx->expression(0));
    if (!target) {
        return nullptr;
    }
    
    // Build value expression (right-hand side)
    auto value = buildExpression(ctx->expression(1));
    if (!value) {
        return nullptr;
    }
    
    return std::make_unique<AssignmentStmt>(loc, std::move(target), std::move(value));
}

std::unique_ptr<ArrayLiteralExpr> ASTBuilder::buildArrayLiteral(FirstParser::ArrayLiteralContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    std::vector<std::unique_ptr<Expr>> elements;
    
    // Build element expressions
    if (ctx->expressionList()) {
        for (auto* exprCtx : ctx->expressionList()->expression()) {
            auto expr = buildExpression(exprCtx);
            if (expr) {
                elements.push_back(std::move(expr));
            }
        }
    }
    
    return std::make_unique<ArrayLiteralExpr>(loc, std::move(elements));
}

} // namespace ast
} // namespace first
