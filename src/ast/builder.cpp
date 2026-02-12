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
    
    auto stripQuotes = [](const std::string& s) -> std::string {
        if (s.size() >= 2 && s.front() == '"' && s.back() == '"') {
            return s.substr(1, s.size() - 2);
        }
        return s;
    };

    auto buildGenericParams = [](FirstParser::GenericParamsContext* gctx) -> std::vector<GenericParam> {
        std::vector<GenericParam> params;
        if (!gctx) return params;
        for (auto* gpc : gctx->genericParam()) {
            if (!gpc) continue;
            GenericParam p;
            auto ids = gpc->IDENTIFIER();
            if (ids.size() >= 1) p.name = ids[0]->getText();
            if (ids.size() >= 2) p.constraint = ids[1]->getText();
            params.push_back(std::move(p));
        }
        return params;
    };

    auto buildParameters = [&](FirstParser::ParameterListContext* pctx) -> std::vector<std::unique_ptr<Parameter>> {
        std::vector<std::unique_ptr<Parameter>> params;
        if (!pctx) return params;
        for (auto* paramCtx : pctx->parameter()) {
            if (!paramCtx || !paramCtx->IDENTIFIER()) continue;
            std::string name = paramCtx->IDENTIFIER()->getText();
            std::unique_ptr<Type> ty = nullptr;
            if (paramCtx->type_()) {
                ty = buildType(paramCtx->type_());
            }
            if (!ty) {
                // Allow missing types (type inference later); use Unit placeholder to keep AST valid.
                ty = std::make_unique<PrimitiveType>(getLocation(paramCtx), PrimitiveType::Kind::Unit);
            }
            params.push_back(std::make_unique<Parameter>(getLocation(paramCtx), name, std::move(ty)));
        }
        return params;
    };

    auto buildFunctionBody = [&](FirstParser::FunctionBodyContext* bctx) -> std::vector<std::unique_ptr<Stmt>> {
        std::vector<std::unique_ptr<Stmt>> body;
        if (!bctx) return body;
        for (auto* stmtCtx : bctx->statement()) {
            auto stmt = buildStatement(stmtCtx);
            if (stmt) body.push_back(std::move(stmt));
        }
        return body;
    };

    auto buildFunctionDecl = [&](FirstParser::FunctionDeclContext* fctx, bool isExported) -> std::unique_ptr<FunctionDecl> {
        if (!fctx || !fctx->IDENTIFIER()) return nullptr;
        SourceLocation floc = getLocation(fctx);
        std::string name = fctx->IDENTIFIER()->getText();
        std::vector<GenericParam> generics = buildGenericParams(fctx->genericParams());
        std::vector<std::unique_ptr<Parameter>> params = buildParameters(fctx->parameterList());
        std::unique_ptr<Type> ret = nullptr;
        if (fctx->returnType()) {
            ret = buildType(fctx->returnType()->type_());
        }
        if (!ret) {
            // Default return type is Unit if omitted.
            ret = std::make_unique<PrimitiveType>(floc, PrimitiveType::Kind::Unit);
        }
        std::vector<std::unique_ptr<Stmt>> body;
        if (fctx->functionBody()) {
            body = buildFunctionBody(fctx->functionBody());
        }
        return std::make_unique<FunctionDecl>(floc, name, std::move(generics), std::move(params), std::move(ret), std::move(body), isExported);
    };

    // Build select top-level declarations needed for module/import/function workflows.
    // (Other top-level constructs may be added incrementally.)
    for (auto* top : ctx->topLevel()) {
        if (!top) continue;

        if (top->moduleDecl() && top->moduleDecl()->IDENTIFIER()) {
            program->setModuleName(top->moduleDecl()->IDENTIFIER()->getText());
            continue;
        }

        if (auto* imp = top->importDecl()) {
            SourceLocation iloc = getLocation(imp);
            auto* spec = imp->importSpec();
            if (!spec) continue;

            // import * "module"
            if (spec->MUL() && spec->STRING_LITERAL()) {
                program->addImport(std::make_unique<ImportDecl>(
                    iloc,
                    ImportDecl::ImportKind::All,
                    stripQuotes(spec->STRING_LITERAL()->getText())
                ));
                continue;
            }

            // import { a, b } "module"
            if (spec->LBRACE() && spec->RBRACE() && spec->STRING_LITERAL()) {
                std::vector<std::string> symbols;
                if (auto* list = spec->importList()) {
                    for (auto* id : list->IDENTIFIER()) {
                        if (id) symbols.push_back(id->getText());
                    }
                }
                program->addImport(std::make_unique<ImportDecl>(
                    iloc,
                    ImportDecl::ImportKind::Specific,
                    stripQuotes(spec->STRING_LITERAL()->getText()),
                    std::move(symbols)
                ));
                continue;
            }

            // import "module"
            if (spec->STRING_LITERAL()) {
                program->addImport(std::make_unique<ImportDecl>(
                    iloc,
                    ImportDecl::ImportKind::Default,
                    stripQuotes(spec->STRING_LITERAL()->getText())
                ));
                continue;
            }

            continue;
        }

        if (auto* ef = top->exportFunctionDecl()) {
            if (auto f = buildFunctionDecl(ef->functionDecl(), true)) {
                program->addFunction(std::move(f));
            }
            continue;
        }

        if (auto* f = top->functionDecl()) {
            if (auto fn = buildFunctionDecl(f, false)) {
                program->addFunction(std::move(fn));
            }
            continue;
        }

        if (auto* iface = top->interfaceDecl()) {
            if (auto decl = buildInterfaceDecl(iface)) {
                program->addInterface(std::move(decl));
            }
            continue;
        }

        if (auto* impl = top->implementationDecl()) {
            if (auto decl = buildImplementationDecl(impl)) {
                program->addImplementation(std::move(decl));
            }
            continue;
        }
    }
    
    return program;
}

std::unique_ptr<InterfaceDecl> ASTBuilder::buildInterfaceDecl(FirstParser::InterfaceDeclContext* ctx) {
    if (!ctx || !ctx->IDENTIFIER()) return nullptr;
    SourceLocation loc = getLocation(ctx);
    std::string name = ctx->IDENTIFIER()->getText();
    std::vector<GenericParam> genericParams;
    if (auto* gp = ctx->genericParams()) {
        for (auto* paramCtx : gp->genericParam()) {
            if (!paramCtx || paramCtx->IDENTIFIER().empty()) continue;
            GenericParam p;
            p.name = paramCtx->IDENTIFIER(0)->getText();
            p.constraint.clear();
            p.kind = GenericParamKind::Star;
            if (paramCtx->kindAnnotation()) {
                p.kind = GenericParamKind::StarArrowStar;
            } else if (paramCtx->IDENTIFIER().size() > 1) {
                p.constraint = paramCtx->IDENTIFIER(1)->getText();
            }
            genericParams.push_back(std::move(p));
        }
    }
    std::vector<std::unique_ptr<InterfaceMember>> members;
    for (auto* mctx : ctx->interfaceMember()) {
        if (!mctx || !mctx->IDENTIFIER() || !mctx->type_()) continue;
        SourceLocation mloc = getLocation(mctx);
        std::string mname = mctx->IDENTIFIER()->getText();
        auto mtype = buildType(mctx->type_());
        if (mtype) {
            members.push_back(std::make_unique<InterfaceMember>(mloc, mname, std::move(mtype)));
        }
    }
    return std::make_unique<InterfaceDecl>(loc, name, std::move(genericParams), std::move(members));
}

std::unique_ptr<ImplementationDecl> ASTBuilder::buildImplementationDecl(FirstParser::ImplementationDeclContext* ctx) {
    if (!ctx || !ctx->IDENTIFIER() || !ctx->typeList()) return nullptr;
    SourceLocation loc = getLocation(ctx);
    std::string interfaceName = ctx->IDENTIFIER()->getText();
    std::vector<std::unique_ptr<Type>> typeArgs;
    for (auto* tctx : ctx->typeList()->type_()) {
        auto ty = buildType(tctx);
        if (ty) typeArgs.push_back(std::move(ty));
    }
    std::vector<std::unique_ptr<ImplementationMember>> members;
    for (auto* mctx : ctx->implementationMember()) {
        if (!mctx || !mctx->IDENTIFIER()) continue;
        SourceLocation mloc = getLocation(mctx);
        std::string mname = mctx->IDENTIFIER()->getText();
        std::unique_ptr<Expr> value;
        if (mctx->functionBody()) {
            std::vector<std::unique_ptr<Stmt>> body;
            for (auto* sctx : mctx->functionBody()->statement()) {
                auto stmt = buildStatement(sctx);
                if (stmt) body.push_back(std::move(stmt));
            }
            value = std::make_unique<BlockExpr>(mloc, std::move(body), nullptr);
        } else if (mctx->expression()) {
            value = buildExpression(mctx->expression());
        }
        if (value) {
            members.push_back(std::make_unique<ImplementationMember>(mloc, mname, std::move(value)));
        }
    }
    return std::make_unique<ImplementationDecl>(loc, interfaceName, std::move(typeArgs), std::move(members));
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
        // Unescape: \\ \n \t \r \"
        std::string out;
        out.reserve(value.size());
        for (size_t i = 0; i < value.size(); ++i) {
            if (value[i] == '\\' && i + 1 < value.size()) {
                switch (value[i + 1]) {
                    case 'n':  out += '\n'; ++i; break;
                    case 't':  out += '\t'; ++i; break;
                    case 'r':  out += '\r'; ++i; break;
                    case '"':  out += '"';  ++i; break;
                    case '\\': out += '\\'; ++i; break;
                    default:   out += value[i + 1]; ++i; break;
                }
            } else {
                out += value[i];
            }
        }
        value = std::move(out);
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::String, value);
    } else if (ctx->CHAR_LITERAL()) {
        std::string v = ctx->CHAR_LITERAL()->getText();
        uint32_t codePoint = 0;
        if (v.size() >= 2 && v.front() == '\'' && v.back() == '\'') {
            std::string inner = v.substr(1, v.size() - 2);
            if (inner.size() == 1) {
                codePoint = static_cast<unsigned char>(inner[0]);
            } else if (inner.size() >= 2 && inner[0] == '\\') {
                if (inner.size() == 2) {
                    switch (inner[1]) {
                        case 'n': codePoint = 10; break;
                        case 't': codePoint = 9; break;
                        case 'r': codePoint = 13; break;
                        case '\\': codePoint = '\\'; break;
                        case '\'': codePoint = '\''; break;
                        case '"': codePoint = '"'; break;
                        default: codePoint = static_cast<unsigned char>(inner[1]); break;
                    }
                } else if (inner.size() >= 6 && inner[1] == 'u') {
                    codePoint = static_cast<uint32_t>(std::stoul(inner.substr(2, 4), nullptr, 16));
                }
            }
            if (codePoint >= 0xD800 && codePoint <= 0xDFFF) codePoint = 0;
            if (codePoint > 0x10FFFF) codePoint = 0;
        }
        return std::make_unique<LiteralExpr>(loc, LiteralExpr::LiteralType::Char, std::to_string(codePoint));
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
    
    auto exprs = ctx->rangeExpr(); // Returns by value
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

std::unique_ptr<Expr> ASTBuilder::buildExpression(FirstParser::RangeExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    auto exprs = ctx->additiveExpr();
    if (exprs.empty()) return nullptr;
    auto start = buildExpression(exprs[0]);
    if (!start) return nullptr;
    bool hasRangeOp = (ctx->RANGE_OP().size() > 0 || ctx->RANGE_INCLUSIVE().size() > 0);
    if (!hasRangeOp) return start;
    bool inclusive = (ctx->RANGE_INCLUSIVE().size() > 0);
    // Haskell-style: start..end (2 exprs) or start, second..end (3 exprs, step = second - first)
    if (exprs.size() == 2) {
        auto end = buildExpression(exprs[1]);
        if (!end) return start;
        SourceLocation loc = getLocation(ctx);
        return std::make_unique<RangeExpr>(loc, std::move(start), std::move(end), inclusive, nullptr);
    }
    if (exprs.size() >= 3) {
        auto stepHint = buildExpression(exprs[1]);
        auto end = buildExpression(exprs[2]);
        if (!stepHint || !end) return start;
        SourceLocation loc = getLocation(ctx);
        return std::make_unique<RangeExpr>(loc, std::move(start), std::move(end), inclusive, std::move(stepHint));
    }
    return start;
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
            // Build argument expressions
            std::vector<std::unique_ptr<Expr>> args;
            if (ctx->expressionList()) {
                for (auto* exprCtx : ctx->expressionList()->expression()) {
                    if (auto arg = buildExpression(exprCtx)) {
                        args.push_back(std::move(arg));
                    }
                }
            }

            // Currently we support direct calls like: foo(a, b)
            // where the callee is an identifier (VariableExpr).
            if (auto* var = dynamic_cast<VariableExpr*>(base.get())) {
                std::string calleeName = var->getName();
                return std::make_unique<FunctionCallExpr>(loc, calleeName, std::move(args));
            }

            // If the callee isn't a simple identifier yet (e.g. calling a lambda/field),
            // just return the base for now.
            return base;
        }
        
        // Member access: postfixExpr DOT IDENTIFIER
        if (ctx->DOT() && ctx->IDENTIFIER()) {
            std::string fieldName = ctx->IDENTIFIER()->getText();
            return std::make_unique<FieldAccessExpr>(loc, std::move(base), fieldName);
        }
        
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
            std::vector<std::unique_ptr<Expr>> args;
            if (ctx->expressionList()) {
                for (auto* exprCtx : ctx->expressionList()->expression()) {
                    if (auto arg = buildExpression(exprCtx)) {
                        args.push_back(std::move(arg));
                    }
                }
            }

            if (auto* var = dynamic_cast<VariableExpr*>(base.get())) {
                std::string calleeName = var->getName();
                return std::make_unique<FunctionCallExpr>(loc, calleeName, std::move(args));
            }

            return base;
        }
        
        // Check for field access
        if (ctx->DOT() && ctx->IDENTIFIER()) {
            std::string fieldName = ctx->IDENTIFIER()->getText();
            return std::make_unique<FieldAccessExpr>(loc, std::move(base), fieldName);
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
    
    // Check for record literal
    if (ctx->recordLiteral()) {
        return buildRecordLiteral(ctx->recordLiteral());
    }
    
    // Check for lambda expression
    if (ctx->lambdaExpr()) {
        return buildLambdaExpr(ctx->lambdaExpr());
    }
    
    // TODO: Handle other primary expressions (constructors, conditionals)
    
    return nullptr;
}

std::unique_ptr<LambdaExpr> ASTBuilder::buildLambdaExpr(FirstParser::LambdaExprContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Build parameters
    std::vector<std::unique_ptr<Parameter>> parameters;
    if (ctx->parameterList()) {
        auto params = ctx->parameterList()->parameter();
        for (auto* paramCtx : params) {
            if (!paramCtx) continue;
            
            std::string paramName = paramCtx->IDENTIFIER()->getText();
            auto paramType = buildType(paramCtx->type_());
            if (!paramType) {
                // Allow unannotated lambda params in the parser-only pipeline.
                // Full type inference can be added later; for now treat as a generic placeholder.
                paramType = std::make_unique<GenericType>(getLocation(paramCtx), "Any");
            }
            
            parameters.push_back(std::make_unique<Parameter>(
                getLocation(paramCtx),
                paramName,
                std::move(paramType)
            ));
        }
    }
    
    // Build return type (optional)
    std::unique_ptr<Type> returnType = nullptr;
    if (ctx->returnType()) {
        returnType = buildType(ctx->returnType()->type_());
    }
    
    // Build body
    std::vector<std::unique_ptr<Stmt>> body;
    if (ctx->functionBody()) {
        // Function body with statements
        auto stmts = ctx->functionBody()->statement();
        for (auto* stmtCtx : stmts) {
            auto stmt = buildStatement(stmtCtx);
            if (stmt) {
                body.push_back(std::move(stmt));
            }
        }
    } else if (ctx->expression()) {
        // Single expression body - convert to return statement
        auto expr = buildExpression(ctx->expression());
        if (expr) {
            SourceLocation exprLoc = expr->getLocation();
            body.push_back(std::make_unique<ReturnStmt>(exprLoc, std::move(expr)));
        }
    }
    
    return std::make_unique<LambdaExpr>(loc, std::move(parameters), std::move(returnType), std::move(body));
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
    
    // Check for generic type (e.g., Option<T>, Array<Int>)
    if (ctx->genericType()) {
        return buildGenericType(ctx->genericType());
    }
    
    // TODO: Handle other primary types (arrays, etc.)
    
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
    } else if (ctx->CHAR()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::Char);
    } else if (ctx->UNIT()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::Unit);
    } else if (ctx->ARRAYBUF()) {
        return std::make_unique<PrimitiveType>(loc, PrimitiveType::Kind::ArrayBuf);
    }
    
    return nullptr;
}

std::unique_ptr<Type> ASTBuilder::buildGenericType(FirstParser::GenericTypeContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    // Get the base identifier name
    if (!ctx->IDENTIFIER()) {
        return nullptr;
    }
    
    std::string baseName = ctx->IDENTIFIER()->getText();
    
    // Check if there are type arguments (e.g., Option<T>, Array<Int>)
    // The grammar is: genericType: IDENTIFIER (LT typeList GT)?
    // typeList: type_ (COMMA type_)*
    if (ctx->typeList()) {
        // This is a parameterized type (e.g., Option<T>, Array<Int>)
        std::vector<std::unique_ptr<Type>> typeArgs;
        auto types = ctx->typeList()->type_();
        for (auto* typeCtx : types) {
            auto type = buildType(typeCtx);
            if (type) {
                typeArgs.push_back(std::move(type));
            } else {
                errorReporter_.error(
                    getLocation(typeCtx),
                    "Invalid type argument in generic type"
                );
            }
        }
        
        return std::make_unique<ParameterizedType>(loc, baseName, std::move(typeArgs));
    } else {
        // This could be either:
        // 1. A generic type parameter (e.g., T in function<T>(x: T))
        // 2. A type name (e.g., Int, String, Option)
        // For now, we'll treat it as a GenericType (type parameter)
        // In the future, we might need to look it up in the symbol table
        // to distinguish between type parameters and actual type names
        // TODO: Look up in symbol table to determine if it's a type parameter or type name
        return std::make_unique<GenericType>(loc, baseName);
    }
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
    
    // Check for for-in statement
    if (ctx->forStmt()) {
        return buildForInStmt(ctx->forStmt());
    }
    
    // Check for if statement
    if (ctx->ifStmt()) {
        return buildIfStmt(ctx->ifStmt());
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

std::unique_ptr<ForInStmt> ASTBuilder::buildForInStmt(FirstParser::ForStmtContext* ctx) {
    if (!ctx || !ctx->IDENTIFIER() || !ctx->expression()) return nullptr;
    SourceLocation loc = getLocation(ctx);
    std::string varName = ctx->IDENTIFIER()->getText();
    auto iterable = buildExpression(ctx->expression());
    if (!iterable) return nullptr;
    std::vector<std::unique_ptr<Stmt>> body;
    for (auto* stmtCtx : ctx->statement()) {
        auto stmt = buildStatement(stmtCtx);
        if (stmt) body.push_back(std::move(stmt));
    }
    return std::make_unique<ForInStmt>(loc, varName, std::move(iterable), std::move(body));
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

std::unique_ptr<RecordLiteralExpr> ASTBuilder::buildRecordLiteral(FirstParser::RecordLiteralContext* ctx) {
    if (!ctx) {
        return nullptr;
    }
    
    SourceLocation loc = getLocation(ctx);
    
    std::vector<RecordLiteralExpr::Field> fields;
    
    // Build field expressions
    if (ctx->recordLiteralFieldList()) {
        for (auto* fieldCtx : ctx->recordLiteralFieldList()->recordLiteralField()) {
            if (fieldCtx->IDENTIFIER() && fieldCtx->expression()) {
                std::string fieldName = fieldCtx->IDENTIFIER()->getText();
                auto fieldValue = buildExpression(fieldCtx->expression());
                if (fieldValue) {
                    fields.emplace_back(fieldName, std::move(fieldValue));
                }
            }
        }
    }
    
    return std::make_unique<RecordLiteralExpr>(loc, std::move(fields));
}

} // namespace ast
} // namespace first
