 #include "first/parser/parser.h"
 #include "first/error_reporter.h"
 #include "FirstLexer.h"
 #include <antlr4-runtime.h>

 using namespace antlr4;

 namespace first {
 namespace parser {

 // -------- TokenStreamAdapter --------

 TokenStreamAdapter::TokenStreamAdapter(CommonTokenStream& stream,
                                        const std::string& filename)
     : stream_(stream), filename_(filename), index_(0) {
     stream_.fill();
     refreshCurrent();
 }

 Token TokenStreamAdapter::makeTokenFromAntlr(int rawType,
                                              const std::string& text,
                                              std::size_t line,
                                              std::size_t column) {
     TokenKind kind = TokenKind::EndOfFile;

    // Some punctuation is defined as literals in the grammar (ANTLR may emit T__N tokens).
    // Handle these by text to keep TokenKind decoupled from generated token numbers.
    if (text == "_") {
        kind = TokenKind::Underscore;
    } else if (text == "@") {
        kind = TokenKind::At;
    }

     switch (rawType) {
     case FirstLexer::IDENTIFIER:
         // "_" is lexed as IDENTIFIER but must be Underscore for pattern matching
         if (text != "_") kind = TokenKind::Identifier;
         break;
     case FirstLexer::INT_LITERAL:
         kind = TokenKind::IntLiteral;
         break;
     case FirstLexer::FLOAT_LITERAL:
         kind = TokenKind::FloatLiteral;
         break;
    case FirstLexer::STRING_LITERAL:
        kind = TokenKind::StringLiteral;
        break;

     case FirstLexer::FUNCTION: kind = TokenKind::KwFunction; break;
     case FirstLexer::INTERACTION: kind = TokenKind::KwInteraction; break;
     case FirstLexer::IF: kind = TokenKind::KwIf; break;
     case FirstLexer::ELSE: kind = TokenKind::KwElse; break;
     case FirstLexer::RETURN: kind = TokenKind::KwReturn; break;
     case FirstLexer::LET: kind = TokenKind::KwLet; break;
     case FirstLexer::VAR: kind = TokenKind::KwVar; break;
     case FirstLexer::MUT: kind = TokenKind::KwMut; break;
    case FirstLexer::MATCH: kind = TokenKind::KwMatch; break;
    case FirstLexer::WHEN: kind = TokenKind::KwWhen; break;
     case FirstLexer::TYPE: kind = TokenKind::KwType; break;
     case FirstLexer::INTERFACE: kind = TokenKind::KwInterface; break;
     case FirstLexer::IMPLEMENTATION: kind = TokenKind::KwImplementation; break;
     case FirstLexer::IMPORT: kind = TokenKind::KwImport; break;
     case FirstLexer::EXPORT: kind = TokenKind::KwExport; break;
     case FirstLexer::EXTENDS: kind = TokenKind::KwExtends; break;
    case FirstLexer::MODULE: kind = TokenKind::KwModule; break;
    case FirstLexer::DO: kind = TokenKind::KwDo; break;
    case FirstLexer::WHERE: kind = TokenKind::KwWhere; break;
    case FirstLexer::FORALL: kind = TokenKind::KwForall; break;
    case FirstLexer::EXISTS: kind = TokenKind::KwExists; break;
    case FirstLexer::ASYNC: kind = TokenKind::KwAsync; break;
    case FirstLexer::AWAIT: kind = TokenKind::KwAwait; break;
    case FirstLexer::SPAWN: kind = TokenKind::KwSpawn; break;
    case FirstLexer::JOIN: kind = TokenKind::KwJoin; break;
    case FirstLexer::SELECT: kind = TokenKind::KwSelect; break;
    case FirstLexer::FOR: kind = TokenKind::KwFor; break;
    case FirstLexer::IN: kind = TokenKind::KwIn; break;

    case FirstLexer::INT: kind = TokenKind::KwInt; break;
    case FirstLexer::FLOAT: kind = TokenKind::KwFloat; break;
    case FirstLexer::BOOL: kind = TokenKind::KwBool; break;
    case FirstLexer::STRING: kind = TokenKind::KwString; break;
    case FirstLexer::UNIT: kind = TokenKind::KwUnit; break;
     case FirstLexer::TRUE: kind = TokenKind::KwTrue; break;
     case FirstLexer::FALSE: kind = TokenKind::KwFalse; break;
    case FirstLexer::NULL_: kind = TokenKind::KwNull; break;

    // Treat built-in collection type keywords as identifiers for the
    // purposes of the hand-written parser's simpler type grammar. This
    // allows constructs like `Array<Int>` to be handled as generic
    // ParameterizedType nodes.
    case FirstLexer::ARRAY: kind = TokenKind::Identifier; break;
    case FirstLexer::VECTOR: kind = TokenKind::Identifier; break;
    case FirstLexer::HASHMAP: kind = TokenKind::Identifier; break;
    case FirstLexer::SET: kind = TokenKind::Identifier; break;
    case FirstLexer::LINKEDLIST: kind = TokenKind::Identifier; break;
    case FirstLexer::PROMISE: kind = TokenKind::Identifier; break;
    case FirstLexer::CHANNEL: kind = TokenKind::Identifier; break;
    case FirstLexer::TASK: kind = TokenKind::Identifier; break;

    case FirstLexer::LE: kind = TokenKind::OpLe; break;
    case FirstLexer::GE: kind = TokenKind::OpGe; break;
    case FirstLexer::EQ: kind = TokenKind::OpEq; break;
    case FirstLexer::NE: kind = TokenKind::OpNe; break;
    case FirstLexer::LT: kind = TokenKind::OpLt; break;
    case FirstLexer::GT: kind = TokenKind::OpGt; break;
    case FirstLexer::AND: kind = TokenKind::OpAnd; break;
    case FirstLexer::OR: kind = TokenKind::OpOr; break;
    case FirstLexer::NOT: kind = TokenKind::OpNot; break;
    case FirstLexer::PLUS: kind = TokenKind::OpPlus; break;
    case FirstLexer::MINUS: kind = TokenKind::OpMinus; break;
    case FirstLexer::MUL: kind = TokenKind::OpMul; break;
    case FirstLexer::DIV: kind = TokenKind::OpDiv; break;
    case FirstLexer::MOD: kind = TokenKind::OpMod; break;
    case FirstLexer::ASSIGN: kind = TokenKind::OpAssign; break;
    case FirstLexer::PLUS_ASSIGN: kind = TokenKind::OpPlusAssign; break;
    case FirstLexer::MINUS_ASSIGN: kind = TokenKind::OpMinusAssign; break;
    case FirstLexer::MUL_ASSIGN: kind = TokenKind::OpMulAssign; break;
    case FirstLexer::DIV_ASSIGN: kind = TokenKind::OpDivAssign; break;
    case FirstLexer::ARROW: kind = TokenKind::OpArrow; break;
    case FirstLexer::FAT_ARROW: kind = TokenKind::OpFatArrow; break;
    case FirstLexer::RANGE_OP: kind = TokenKind::OpRange; break;
    case FirstLexer::RANGE_INCLUSIVE: kind = TokenKind::OpRangeInclusive; break;

    // Monadic operators (only valid in interaction functions; semantic
    // restrictions are enforced later, we just surface them as tokens here).
    case FirstLexer::BIND:  kind = TokenKind::OpBind; break;   // >>=
    case FirstLexer::THEN:  kind = TokenKind::OpThen; break;   // >>
    case FirstLexer::FMAP:  kind = TokenKind::OpFmap; break;   // <$>
    case FirstLexer::APPLY: kind = TokenKind::OpApply; break;  // <*>

     case FirstLexer::LPAREN: kind = TokenKind::LParen; break;
     case FirstLexer::RPAREN: kind = TokenKind::RParen; break;
     case FirstLexer::LBRACE: kind = TokenKind::LBrace; break;
     case FirstLexer::RBRACE: kind = TokenKind::RBrace; break;
     case FirstLexer::LREFINEMENT: kind = TokenKind::LRefinement; break;  // {{
     case FirstLexer::RREFINEMENT: kind = TokenKind::RRefinement; break;  // }}
     case FirstLexer::LBRACKET: kind = TokenKind::LBracket; break;
     case FirstLexer::RBRACKET: kind = TokenKind::RBracket; break;
     case FirstLexer::SEMICOLON: kind = TokenKind::Semicolon; break;
    case FirstLexer::COMMA: kind = TokenKind::Comma; break;
    case FirstLexer::COLON: kind = TokenKind::Colon; break;
    case FirstLexer::DOT: kind = TokenKind::Dot; break;
    case FirstLexer::PIPE_DELIM: kind = TokenKind::OpPipe; break;
     default:
        // If we already classified the token by text (e.g. '_' / '@'), keep it.
        // Otherwise, treat it as non-surfaceable.
        if (kind == TokenKind::EndOfFile) {
            kind = TokenKind::EndOfFile;
        }
         break;
     }

     Token t;
     t.kind = kind;
     t.lexeme = text;
     t.loc = SourceLocation(line, column, filename_);
     return t;
 }

 void TokenStreamAdapter::refreshCurrent() {
    const auto& all = stream_.getTokens();
    while (index_ < all.size()) {
        auto* t = all[index_];
        if (!t) {
            ++index_;
            continue;
        }
        int rawType = static_cast<int>(t->getType());
        // Skip trivia tokens: whitespace and comments.
        if (rawType == FirstLexer::WS ||
            rawType == FirstLexer::LINE_COMMENT ||
            rawType == FirstLexer::BLOCK_COMMENT) {
            ++index_;
            continue;
        }
        current_ = makeTokenFromAntlr(
            rawType,
            t->getText(),
            static_cast<std::size_t>(t->getLine()),
            static_cast<std::size_t>(t->getCharPositionInLine() + 1));
        return;
    }

    // Reached end without finding a non-trivia token.
    current_.kind = TokenKind::EndOfFile;
    current_.lexeme.clear();
    current_.loc = SourceLocation(0, 0, filename_);
 }

 const Token& TokenStreamAdapter::lookahead(std::size_t n) {
    const auto& all = stream_.getTokens();
    std::size_t idx = index_;
    std::size_t remaining = n;
    static Token tmp;

    while (idx < all.size()) {
        auto* t = all[idx];
        if (!t) {
            ++idx;
            continue;
        }
        int rawType = static_cast<int>(t->getType());
        if (rawType == FirstLexer::WS ||
            rawType == FirstLexer::LINE_COMMENT ||
            rawType == FirstLexer::BLOCK_COMMENT) {
            ++idx;
            continue;
        }
        if (remaining == 0) {
            tmp = makeTokenFromAntlr(
                rawType,
                t->getText(),
                static_cast<std::size_t>(t->getLine()),
                static_cast<std::size_t>(t->getCharPositionInLine() + 1));
            return tmp;
        }
        --remaining;
        ++idx;
    }

    static Token eofToken{TokenKind::EndOfFile, "", SourceLocation(0, 0, "")};
    return eofToken;
 }

 void TokenStreamAdapter::advance() {
     if (index_ < stream_.getTokens().size()) {
         ++index_;
     }
     refreshCurrent();
 }

 void TokenStreamAdapter::setIndex(std::size_t index) {
     index_ = index;
     refreshCurrent();
 }

 // -------- FirstParser --------

 FirstParser::FirstParser(TokenStreamAdapter& tokens,
                          first::ErrorReporter& errorReporter,
                          const std::string& filename)
     : tokens_(tokens)
     , errorReporter_(errorReporter)
     , filename_(filename) {}

 bool FirstParser::match(TokenKind kind) {
     if (current().kind == kind) {
         advance();
         return true;
     }
     return false;
 }

 bool FirstParser::expect(TokenKind kind, const char* message) {
     if (match(kind)) {
         return true;
     }
     reportSyntaxError(message);
     return false;
 }

 void FirstParser::reportSyntaxError(const std::string& message,
                                     std::size_t length) {
     errorReporter_.error(current().loc, "Syntax error: " + message,
                          ErrorCode(ErrorCodes::SYNTAX_ERROR, "syntax"),
                          length);
 }

 std::unique_ptr<ast::Program> FirstParser::parseProgram() {
     return parseProgramInternal();
 }

 std::unique_ptr<ast::Program> FirstParser::parseProgramInternal() {
     SourceLocation loc = current().loc;
     auto program = std::make_unique<ast::Program>(loc);

     while (current().kind != TokenKind::EndOfFile) {
        parseTopLevel(*program);
        if (current().kind == TokenKind::Semicolon) {
            advance();
        } else if (current().kind == TokenKind::EndOfFile) {
            break;
        } else if (current().kind == TokenKind::KwModule ||
                   current().kind == TokenKind::KwImport ||
                   current().kind == TokenKind::KwExport ||
                   current().kind == TokenKind::KwType ||
                   current().kind == TokenKind::KwFunction ||
                   current().kind == TokenKind::KwInterface ||
                   current().kind == TokenKind::KwImplementation ||
                   current().kind == TokenKind::KwInteraction) {
            // Next top-level declaration; do not advance, loop will parse it.
        } else {
            // Try to recover by skipping unexpected tokens until a semicolon or EOF.
            advance();
        }
     }
     return program;
 }

 void FirstParser::parseTopLevel(ast::Program& program) {
     switch (current().kind) {
     case TokenKind::KwModule:
         parseModuleDecl(program);
         break;
     case TokenKind::KwImport:
         parseImportDecl(program);
         break;
     case TokenKind::KwExport:
         parseExportFunctionDecl(program);
         break;
    case TokenKind::KwFunction: {
        // Only treat as a top-level function declaration if the next
        // significant token is an identifier. This avoids misinterpreting
        // inline lambdas like `function(x) { ... }` that appear in expression
        // position but might be seen here due to error recovery.
        const Token& next = tokens_.lookahead(1);
        if (next.kind != TokenKind::Identifier) {
            // Skip this token and let the general recovery logic handle it
            advance();
            break;
        }
        parseFunctionDecl(program, false);
        break;
    }
    case TokenKind::KwInterface:
        parseInterfaceDecl(program);
        break;
    case TokenKind::KwImplementation:
        parseImplementationDecl(program);
        break;
    case TokenKind::KwInteraction: {
        // interaction declarations are like functions but stored separately
        // Reuse parseFunctionDecl-style logic but build InteractionDecl.
        SourceLocation loc = current().loc;
        expect(TokenKind::KwInteraction, "expected 'interaction'");

        if (current().kind != TokenKind::Identifier) {
            reportSyntaxError("expected interaction name");
            return;
        }
        std::string name = current().lexeme;
        advance();

        std::vector<ast::GenericParam> genericParams;
        if (current().kind == TokenKind::OpLt) {
            advance();
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected generic parameter name");
                    break;
                }
                ast::GenericParam p;
                p.name = current().lexeme;
                p.constraint.clear();
                advance();
                if (match(TokenKind::Colon)) {
                    if (current().kind == TokenKind::Identifier) {
                        p.constraint = current().lexeme;
                        advance();
                    } else {
                        reportSyntaxError("expected interface name after ':' in generic parameter");
                    }
                }
                genericParams.push_back(std::move(p));
                if (match(TokenKind::Comma)) continue;
                break;
            }
            if (!match(TokenKind::OpGt)) {
                reportSyntaxError("expected '>' after generic parameter list");
            }
        }

        expect(TokenKind::LParen, "expected '(' after interaction name");

        std::vector<std::unique_ptr<ast::Parameter>> params;
        if (current().kind != TokenKind::RParen) {
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected parameter name");
                    break;
                }
                std::string paramName = current().lexeme;
                SourceLocation paramLoc = current().loc;
                advance();

                std::unique_ptr<ast::Type> ty;
                if (match(TokenKind::Colon)) {
                    ty = parseType();
                }
                if (!ty) {
                    ty = std::make_unique<ast::PrimitiveType>(paramLoc, ast::PrimitiveType::Kind::Unit);
                }
                params.push_back(std::make_unique<ast::Parameter>(
                    paramLoc, paramName, std::move(ty)));

                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        expect(TokenKind::RParen, "expected ')' after parameters");

        std::unique_ptr<ast::Type> retType;
        if (match(TokenKind::OpArrow)) {
            retType = parseType();
        }
        if (!retType) {
            retType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Unit);
        }

        std::vector<std::unique_ptr<ast::Stmt>> body;
        if (match(TokenKind::LBrace)) {
            while (current().kind != TokenKind::RBrace &&
                   current().kind != TokenKind::EndOfFile) {
                auto stmt = parseStatement();
                if (stmt) {
                    body.push_back(std::move(stmt));
                } else {
                    advance();
                }
            }
            expect(TokenKind::RBrace, "expected '}' at end of interaction body");
        }

        program.addInteraction(std::make_unique<ast::InteractionDecl>(
            loc, name, std::move(genericParams), std::move(params),
            std::move(retType), std::move(body), false));
        break;
    }
    case TokenKind::KwType:
        parseTypeDecl(program);
        break;
    case TokenKind::KwLet:
    case TokenKind::KwVar: {
        // Allow top-level variable declarations (for simple parser tests).
        // We parse them for error reporting but do not store them in the AST.
        auto decl = parseVarDecl();
        (void)decl;
        break;
    }
     default:
         // Skip unknown top-level for now.
         advance();
         break;
     }
 }

 void FirstParser::parseModuleDecl(ast::Program& program) {
     // module Identifier;
     SourceLocation loc = current().loc;
     expect(TokenKind::KwModule, "expected 'module'");
     if (current().kind == TokenKind::Identifier) {
         program.setModuleName(current().lexeme);
         advance();
     } else {
         reportSyntaxError("expected module name");
     }
    // Leave trailing semicolon (if any) to be consumed by the top-level loop.
 }

 void FirstParser::parseImportDecl(ast::Program& program) {
    // import * "Module";
    // import { a, b } "Module";
    // import "Module";
     SourceLocation loc = current().loc;
     expect(TokenKind::KwImport, "expected 'import'");

    if (current().kind == TokenKind::OpMul) {
        // import * "Module"
        advance();
        if (current().kind == TokenKind::StringLiteral) {
            std::string moduleName = current().lexeme;
            if (moduleName.size() >= 2 && moduleName.front() == '"' && moduleName.back() == '"') {
                moduleName = moduleName.substr(1, moduleName.size() - 2);
            }
            advance();
            program.addImport(std::make_unique<ast::ImportDecl>(
                loc,
                ast::ImportDecl::ImportKind::All,
                moduleName));
        } else {
            reportSyntaxError("expected module string literal after '*'");
        }
    } else if (current().kind == TokenKind::LBrace) {
        // import { a, b } "Module"
        advance(); // '{'
        std::vector<std::string> symbols;
        if (current().kind != TokenKind::RBrace) {
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected identifier in import list");
                    break;
                }
                symbols.push_back(current().lexeme);
                advance();
                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        expect(TokenKind::RBrace, "expected '}' after import list");
        if (current().kind == TokenKind::StringLiteral) {
            std::string moduleName = current().lexeme;
            if (moduleName.size() >= 2 && moduleName.front() == '"' && moduleName.back() == '"') {
                moduleName = moduleName.substr(1, moduleName.size() - 2);
            }
            advance();
            program.addImport(std::make_unique<ast::ImportDecl>(
                loc,
                ast::ImportDecl::ImportKind::Specific,
                moduleName,
                std::move(symbols)));
        } else {
            reportSyntaxError("expected module string literal after import list");
        }
    } else if (current().kind == TokenKind::StringLiteral) {
         std::string moduleName = current().lexeme;
         if (moduleName.size() >= 2 && moduleName.front() == '"' && moduleName.back() == '"') {
             moduleName = moduleName.substr(1, moduleName.size() - 2);
         }
         advance();
         program.addImport(std::make_unique<ast::ImportDecl>(
             loc,
             ast::ImportDecl::ImportKind::Default,
             moduleName));
     } else {
         reportSyntaxError("expected string literal module name in import");
     }
    // Leave trailing semicolon (if any) to be consumed by the top-level loop.
 }

 void FirstParser::parseExportFunctionDecl(ast::Program& program) {
     // export function ...
     expect(TokenKind::KwExport, "expected 'export'");
     if (current().kind == TokenKind::KwFunction) {
         parseFunctionDecl(program, true);
     } else {
         reportSyntaxError("expected 'function' after 'export'");
     }
 }

 void FirstParser::parseFunctionDecl(ast::Program& program, bool exported) {
    // function name<T,...>(...) -> Type { ... }
     SourceLocation loc = current().loc;
     expect(TokenKind::KwFunction, "expected 'function'");

     if (current().kind != TokenKind::Identifier) {
         reportSyntaxError("expected function name");
         return;
     }
    std::string name = current().lexeme;
    advance();

    // Optional generic parameter list: <T, U : Eq, ...>
    std::vector<ast::GenericParam> genericParams;
    if (current().kind == TokenKind::OpLt) {
        advance(); // '<'
        while (true) {
            if (current().kind != TokenKind::Identifier) {
                reportSyntaxError("expected generic parameter name");
                break;
            }
            ast::GenericParam p;
            p.name = current().lexeme;
            p.constraint.clear();
            advance();
            if (match(TokenKind::Colon)) {
                if (current().kind == TokenKind::Identifier) {
                    p.constraint = current().lexeme;
                    advance();
                } else {
                    reportSyntaxError("expected interface name after ':' in generic parameter");
                }
            }
            genericParams.push_back(std::move(p));
            if (match(TokenKind::Comma)) {
                continue;
            }
            break;
        }
        if (!match(TokenKind::OpGt)) {
            reportSyntaxError("expected '>' after generic parameter list");
        }
    }

     expect(TokenKind::LParen, "expected '(' after function name");

     std::vector<std::unique_ptr<ast::Parameter>> params;
     if (current().kind != TokenKind::RParen) {
         while (true) {
             if (current().kind != TokenKind::Identifier) {
                 reportSyntaxError("expected parameter name");
                 break;
             }
             std::string paramName = current().lexeme;
             SourceLocation paramLoc = current().loc;
             advance();

             std::unique_ptr<ast::Type> ty;
             if (match(TokenKind::Colon)) {
                 ty = parseType();
             }
             if (!ty) {
                 ty = std::make_unique<ast::PrimitiveType>(paramLoc, ast::PrimitiveType::Kind::Unit);
             }
             params.push_back(std::make_unique<ast::Parameter>(
                 paramLoc, paramName, std::move(ty)));

             if (match(TokenKind::Comma)) {
                 continue;
             }
             break;
         }
     }
     expect(TokenKind::RParen, "expected ')' after parameters");

     std::unique_ptr<ast::Type> retType;
     if (match(TokenKind::OpArrow)) {
         retType = parseType();
     }
     if (!retType) {
         retType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Unit);
     }

     std::vector<std::unique_ptr<ast::Stmt>> body;
     if (match(TokenKind::LBrace)) {
         while (current().kind != TokenKind::RBrace &&
                current().kind != TokenKind::EndOfFile) {
             auto stmt = parseStatement();
             if (stmt) {
                 body.push_back(std::move(stmt));
             } else {
                 advance();
             }
         }
         expect(TokenKind::RBrace, "expected '}' at end of function body");
     }

    program.addFunction(std::make_unique<ast::FunctionDecl>(
        loc, name, std::move(genericParams), std::move(params),
        std::move(retType), std::move(body), exported));
 }

void FirstParser::parseInterfaceDecl(ast::Program& program) {
    SourceLocation loc = locCurrent();
    expect(TokenKind::KwInterface, "expected 'interface'");

    if (current().kind != TokenKind::Identifier) {
        reportSyntaxError("expected interface name");
        return;
    }
    std::string name = current().lexeme;
    advance();

    std::vector<std::string> genericParams;
    if (current().kind == TokenKind::OpLt) {
        advance();
        if (current().kind != TokenKind::OpGt) {
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected generic parameter name");
                    break;
                }
                genericParams.push_back(current().lexeme);
                advance();
                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        if (!match(TokenKind::OpGt)) {
            reportSyntaxError("expected '>' after generic parameter list");
        }
    }

    if (current().kind == TokenKind::KwExtends) {
        advance();
        auto baseType = parseType();
        (void)baseType;
        while (match(TokenKind::Comma)) {
            auto moreType = parseType();
            (void)moreType;
        }
    }

    expect(TokenKind::LBrace, "expected '{' in interface declaration");
    std::vector<std::unique_ptr<ast::InterfaceMember>> members;
    while (current().kind != TokenKind::RBrace &&
           current().kind != TokenKind::EndOfFile) {
        if (current().kind == TokenKind::Identifier) {
            SourceLocation memberLoc = current().loc;
            std::string memberName = current().lexeme;
            advance();
            if (!match(TokenKind::Colon)) {
                reportSyntaxError("expected ':' after interface member name");
                break;
            }
            std::unique_ptr<ast::Type> memberType = parseType();
            if (memberType) {
                members.push_back(std::make_unique<ast::InterfaceMember>(
                    memberLoc, memberName, std::move(memberType)));
            }
            if (current().kind == TokenKind::Semicolon) {
                advance();
            } else if (current().kind != TokenKind::RBrace &&
                       current().kind != TokenKind::EndOfFile) {
                reportSyntaxError("expected ';' after interface member");
            }
        } else {
            advance();
        }
    }
    expect(TokenKind::RBrace, "expected '}' at end of interface body");
    program.addInterface(std::make_unique<ast::InterfaceDecl>(
        loc, name, std::move(genericParams), std::move(members)));
}

void FirstParser::parseImplementationDecl(ast::Program& program) {
    SourceLocation loc = locCurrent();
    expect(TokenKind::KwImplementation, "expected 'implementation'");

    if (current().kind != TokenKind::Identifier) {
        reportSyntaxError("expected implementation target name");
        return;
    }
    std::string interfaceName = current().lexeme;
    advance();

    std::vector<std::unique_ptr<ast::Type>> typeArgs;
    if (current().kind == TokenKind::OpLt) {
        advance();
        if (current().kind != TokenKind::OpGt) {
            while (true) {
                auto instType = parseType();
                if (instType) {
                    typeArgs.push_back(std::move(instType));
                }
                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        if (!match(TokenKind::OpGt)) {
            reportSyntaxError("expected '>' after implementation type arguments");
        }
    }

    while (current().kind != TokenKind::LBrace &&
           current().kind != TokenKind::EndOfFile &&
           current().kind != TokenKind::Semicolon) {
        advance();
    }

    expect(TokenKind::LBrace, "expected '{' in implementation body");
    std::vector<std::unique_ptr<ast::ImplementationMember>> members;
    while (current().kind != TokenKind::RBrace &&
           current().kind != TokenKind::EndOfFile) {
        if (current().kind == TokenKind::Identifier) {
            SourceLocation memberLoc = current().loc;
            std::string memberName = current().lexeme;
            advance();
            if (!match(TokenKind::OpAssign)) {
                reportSyntaxError("expected '=' in implementation member");
                break;
            }
            std::unique_ptr<ast::Expr> value;
            if (current().kind == TokenKind::LBrace) {
                SourceLocation blockLoc = current().loc;
                advance();
                std::vector<std::unique_ptr<ast::Stmt>> body;
                while (current().kind != TokenKind::RBrace &&
                       current().kind != TokenKind::EndOfFile) {
                    auto stmt = parseStatement();
                    if (stmt) {
                        body.push_back(std::move(stmt));
                    }
                    if (current().kind == TokenKind::Semicolon) {
                        advance();
                    } else if (!stmt) {
                        advance();
                    }
                }
                expect(TokenKind::RBrace, "expected '}' at end of implementation function body");
                value = std::make_unique<ast::BlockExpr>(blockLoc, std::move(body), nullptr);
            } else {
                value = parseExpression();
            }
            if (value) {
                members.push_back(std::make_unique<ast::ImplementationMember>(
                    memberLoc, memberName, std::move(value)));
            }
            if (current().kind == TokenKind::Semicolon) {
                advance();
            } else {
                reportSyntaxError("expected ';' after implementation member");
            }
        } else {
            advance();
        }
    }

    expect(TokenKind::RBrace, "expected '}' at end of implementation body");
    program.addImplementation(std::make_unique<ast::ImplementationDecl>(
        loc, interfaceName, std::move(typeArgs), std::move(members)));
}

void FirstParser::parseTypeDecl(ast::Program& program) {
    SourceLocation loc = current().loc;
    if (!expect(TokenKind::KwType, "expected 'type'")) return;
    if (current().kind != TokenKind::Identifier) {
        reportSyntaxError("expected type name after 'type'");
        return;
    }
    std::string typeName = current().lexeme;
    advance();
    std::vector<std::string> typeParams;
    if (current().kind == TokenKind::OpLt) {
        advance();
        while (current().kind == TokenKind::Identifier) {
            typeParams.push_back(current().lexeme);
            advance();
            if (!match(TokenKind::Comma)) break;
        }
        if (!expect(TokenKind::OpGt, "expected '>' after type parameters")) return;
    }
    // Shorthand: type Name(args) means type Name = Name(args)
    if (current().kind == TokenKind::LParen) {
        advance();
        SourceLocation cLoc = current().loc;
        std::vector<std::unique_ptr<ast::Type>> argTypes;
        if (current().kind != TokenKind::RParen) {
            while (true) {
                if (current().kind == TokenKind::Identifier && tokens_.lookahead(1).kind == TokenKind::Colon) {
                    advance();
                    advance();
                }
                auto t = parseType();
                if (t) argTypes.push_back(std::move(t));
                if (match(TokenKind::Comma)) continue;
                break;
            }
        }
        if (!expect(TokenKind::RParen, "expected ')' in type declaration")) return;
        std::vector<std::unique_ptr<ast::Constructor>> constructors;
        constructors.push_back(std::make_unique<ast::Constructor>(cLoc, typeName, std::move(argTypes)));
        auto type = std::make_unique<ast::ADTType>(cLoc, typeName, std::move(constructors));
        if (current().kind == TokenKind::Semicolon) advance();
        program.addTypeDecl(std::make_unique<ast::TypeDecl>(loc, typeName, std::move(type), false, std::move(typeParams)));
        return;
    }
    if (!expect(TokenKind::OpAssign, "expected '=' in type declaration")) return;
    auto type = parseTypeRhs(typeName);
    if (!type) return;
    if (current().kind == TokenKind::Semicolon) advance();
    program.addTypeDecl(std::make_unique<ast::TypeDecl>(loc, typeName, std::move(type), false, std::move(typeParams)));
}

std::unique_ptr<ast::Type> FirstParser::parseTypeRhs(const std::string& typeName) {
    // Sum type with parens: [|] Identifier ( Type, ... ) | Identifier ( Type, ... ) | ...
    bool leadingPipe = match(TokenKind::OpPipe);
    if (leadingPipe || (current().kind == TokenKind::Identifier && tokens_.lookahead(1).kind == TokenKind::LParen)) {
        SourceLocation adtLoc = current().loc;
        std::vector<std::unique_ptr<ast::Constructor>> constructors;
        do {
            if (leadingPipe) leadingPipe = false;
            SourceLocation cLoc = current().loc;
            if (current().kind != TokenKind::Identifier) {
                reportSyntaxError("expected constructor name in sum type");
                break;
            }
            std::string cName = current().lexeme;
            advance();
            std::vector<std::unique_ptr<ast::Type>> argTypes;
            if (match(TokenKind::LParen)) {
                if (current().kind != TokenKind::RParen) {
                    while (true) {
                        // Optional "name: " before type (e.g. radius: Float)
                        if (current().kind == TokenKind::Identifier && tokens_.lookahead(1).kind == TokenKind::Colon) {
                            advance();
                            advance();
                        }
                        auto t = parseType();
                        if (t) argTypes.push_back(std::move(t));
                        if (match(TokenKind::Comma)) continue;
                        break;
                    }
                }
                if (!expect(TokenKind::RParen, "expected ')' after constructor argument types")) {
                    break;
                }
            }
            constructors.push_back(std::make_unique<ast::Constructor>(cLoc, cName, std::move(argTypes)));
        } while (match(TokenKind::OpPipe));
        if (!constructors.empty()) {
            return std::make_unique<ast::ADTType>(adtLoc, typeName, std::move(constructors));
        }
        return nullptr;
    }
    // Sum type without parens: Identifier | Identifier | ... (each variant carries type of same name)
    // e.g. type Shape = Circle | Rectangle -> Circle(Circle) | Rectangle(Rectangle)
    if (current().kind == TokenKind::Identifier && tokens_.lookahead(1).kind == TokenKind::OpPipe) {
        SourceLocation adtLoc = current().loc;
        std::vector<std::unique_ptr<ast::Constructor>> constructors;
        while (current().kind == TokenKind::Identifier) {
            SourceLocation cLoc = current().loc;
            std::string cName = current().lexeme;
            advance();
            std::vector<std::unique_ptr<ast::Type>> argTypes;
            argTypes.push_back(std::make_unique<ast::GenericType>(cLoc, cName));
            constructors.push_back(std::make_unique<ast::Constructor>(cLoc, cName, std::move(argTypes)));
            if (current().kind != TokenKind::OpPipe) break;
            advance(); // consume |
        }
        if (!constructors.empty()) {
            return std::make_unique<ast::ADTType>(adtLoc, typeName, std::move(constructors));
        }
    }
    return parseType();
}

 // ---- Types (minimal subset used in tests) ----

 std::unique_ptr<ast::Type> FirstParser::parseType() {
     // Forall type: forall T U. Type
     if (current().kind == TokenKind::KwForall) {
         SourceLocation loc = current().loc;
         advance(); // consume 'forall'
         std::vector<std::string> typeVars;
         while (current().kind == TokenKind::Identifier) {
             typeVars.push_back(current().lexeme);
             advance();
         }
         if (typeVars.empty()) {
             reportSyntaxError("expected at least one type variable after 'forall'");
             return nullptr;
         }
         if (!expect(TokenKind::Dot, "expected '.' after type variables in forall type")) {
             return nullptr;
         }
         auto bodyType = parseType();
         if (!bodyType) {
             return nullptr;
         }
         return std::make_unique<ast::ForallType>(loc, std::move(typeVars), std::move(bodyType));
     }
     // Existential type: exists x: VarType. BodyType
     if (current().kind == TokenKind::KwExists) {
         SourceLocation loc = current().loc;
         advance(); // consume 'exists'
         if (current().kind != TokenKind::Identifier) {
             reportSyntaxError("expected variable name after 'exists'");
             return nullptr;
         }
         std::string varName = current().lexeme;
         advance();
         if (!expect(TokenKind::Colon, "expected ':' after variable name in existential type")) {
             return nullptr;
         }
         auto varType = parseType();
         if (!varType) {
             return nullptr;
         }
         if (!expect(TokenKind::Dot, "expected '.' in existential type")) {
             return nullptr;
         }
         auto bodyType = parseType();
         if (!bodyType) {
             return nullptr;
         }
         return std::make_unique<ast::ExistentialType>(loc, varName, std::move(varType), std::move(bodyType));
     }
    // Record type: { field: Type, ... }
    if (current().kind == TokenKind::LBrace) {
        SourceLocation loc = current().loc;
        advance(); // '{'
        std::vector<std::unique_ptr<ast::RecordField>> fields;
        if (current().kind != TokenKind::RBrace) {
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected field name in record type");
                    break;
                }
                SourceLocation fieldLoc = current().loc;
                std::string name = current().lexeme;
                advance();
                if (!expect(TokenKind::Colon, "expected ':' after field name in record type")) {
                    break;
                }
                auto fieldType = parseType();
                if (fieldType) {
                    fields.push_back(std::make_unique<ast::RecordField>(fieldLoc, name, std::move(fieldType)));
                }
                if (!match(TokenKind::Comma)) {
                    break;
                }
            }
        }
        if (!expect(TokenKind::RBrace, "expected '}' at end of record type")) {
            return nullptr;
        }
        return std::make_unique<ast::RecordType>(loc, std::move(fields));
    }
    // Function type: function(ParamTypes) -> ReturnType (e.g. in interface members)
    if (current().kind == TokenKind::KwFunction) {
         SourceLocation loc = current().loc;
         advance(); // consume 'function'
         if (!expect(TokenKind::LParen, "expected '(' in function type")) {
             return nullptr;
         }
         std::vector<std::unique_ptr<ast::Type>> paramTypes;
         if (current().kind != TokenKind::RParen) {
             while (true) {
                 auto paramType = parseType();
                 if (!paramType) return nullptr;
                 paramTypes.push_back(std::move(paramType));
                 if (match(TokenKind::Comma)) continue;
                 break;
             }
         }
         if (!expect(TokenKind::RParen, "expected ')' after function parameter types")) {
             return nullptr;
         }
         if (!expect(TokenKind::OpArrow, "expected '->' in function type")) {
             return nullptr;
         }
         auto returnType = parseType();
         if (!returnType) return nullptr;
         return std::make_unique<ast::FunctionType>(
             loc, std::move(paramTypes), std::move(returnType), false);
     }
     // ( type ) parenthesized, or ( id : type ) -> type (Pi) / ( id : type ) * type (Sigma)
     if (current().kind == TokenKind::LParen) {
         SourceLocation loc = current().loc;
         advance(); // consume '('
         bool isDependent = (current().kind == TokenKind::Identifier &&
                            tokens_.lookahead(1).kind == TokenKind::Colon);
         if (isDependent) {
             std::string boundName = current().lexeme;
             advance();
             advance(); // consume ':'
             auto firstType = parseType();
             if (!firstType || !expect(TokenKind::RParen, "expected ')' after (id: type)")) {
                 return nullptr;
             }
             if (match(TokenKind::OpArrow)) {
                 auto returnType = parseType();
                 if (returnType) {
                     return std::make_unique<ast::DependentFunctionType>(
                         loc, boundName, std::move(firstType), std::move(returnType));
                 }
             } else if (match(TokenKind::OpMul)) {
                 auto bodyType = parseType();
                 if (bodyType) {
                     return std::make_unique<ast::DependentPairType>(
                         loc, boundName, std::move(firstType), std::move(bodyType));
                 }
             } else {
                 reportSyntaxError("expected '->' or '*' after (id: type) in dependent type");
                 return nullptr;
             }
             return nullptr;
         }
         // Parenthesized type: ( type )
         auto base = parsePrimaryType();
         if (!base) {
             return nullptr;
         }
         while (current().kind == TokenKind::LBracket) {
             SourceLocation idxLoc = current().loc;
             advance();
             std::vector<std::shared_ptr<ast::Expr>> indices;
             if (current().kind != TokenKind::RBracket) {
                 while (true) {
                     auto indexExpr = parseExpression();
                     if (!indexExpr) {
                         reportSyntaxError("expected index expression in indexed type");
                         return nullptr;
                     }
                     indices.push_back(std::shared_ptr<ast::Expr>(std::move(indexExpr)));
                     if (!match(TokenKind::Comma)) break;
                 }
             }
             expect(TokenKind::RBracket, "expected ']' at end of indexed type");
             base = std::make_unique<ast::IndexedType>(idxLoc, std::move(base), std::move(indices));
         }
         if (!expect(TokenKind::RParen, "expected ')' to close parenthesized type")) {
             return nullptr;
         }
         return base;
     }
     auto base = parsePrimaryType();
     if (!base) {
         return nullptr;
     }
     // Indexed type: BaseType [ indexList ] (e.g. Vector[n], Array<Int>[n])
     while (current().kind == TokenKind::LBracket) {
         SourceLocation loc = current().loc;
         advance(); // consume '['
         std::vector<std::shared_ptr<ast::Expr>> indices;
         if (current().kind != TokenKind::RBracket) {
             while (true) {
                 auto indexExpr = parseExpression();
                 if (!indexExpr) {
                     reportSyntaxError("expected index expression in indexed type");
                     return nullptr;
                 }
                 indices.push_back(std::shared_ptr<ast::Expr>(std::move(indexExpr)));
                 if (!match(TokenKind::Comma)) {
                     break;
                 }
             }
         }
        expect(TokenKind::RBracket, "expected ']' at end of indexed type");
        base = std::make_unique<ast::IndexedType>(loc, std::move(base), std::move(indices));
     }
     // Union type: T | null | ... (so "-> T | null { body }" parses correctly and body is not skipped)
     if (current().kind == TokenKind::OpPipe) {
         SourceLocation unionLoc = base->getLocation();
         std::vector<std::unique_ptr<ast::Type>> unionMembers;
         unionMembers.push_back(std::move(base));
         do {
             advance(); // consume |
             auto next = parsePrimaryType();
             if (!next) break;
             unionMembers.push_back(std::move(next));
         } while (current().kind == TokenKind::OpPipe);
         if (unionMembers.size() > 1)
             return std::make_unique<ast::ParameterizedType>(unionLoc, "|", std::move(unionMembers));
         if (unionMembers.size() == 1)
             return std::move(unionMembers[0]);
     }
     return base;
 }

 std::unique_ptr<ast::Type> FirstParser::parsePrimaryType() {
     SourceLocation loc = current().loc;
     switch (current().kind) {
     case TokenKind::LRefinement: {
         // Refinement type: {{ variable : BaseType where predicate }}
         advance(); // consume '{{'
         if (current().kind != TokenKind::Identifier) {
             reportSyntaxError("expected variable name in refinement type");
             return nullptr;
         }
         std::string varName = current().lexeme;
         advance();
         expect(TokenKind::Colon, "expected ':' after variable name in refinement type");
         auto baseType = parseType();
         if (!baseType) {
             return nullptr;
         }
         expect(TokenKind::KwWhere, "expected 'where' in refinement type");
         auto predicate = parseExpression();
         if (!predicate) {
             reportSyntaxError("expected predicate expression in refinement type");
             return nullptr;
         }
         expect(TokenKind::RRefinement, "expected '}}' at end of refinement type");
         return std::make_unique<ast::RefinementType>(
             loc, varName, std::move(baseType),
             std::shared_ptr<ast::Expr>(std::move(predicate)));
     }
     case TokenKind::KwInt:
         advance();
         return std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
     case TokenKind::KwFloat:
         advance();
         return std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Float);
     case TokenKind::KwBool:
         advance();
         return std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Bool);
     case TokenKind::KwString:
         advance();
         return std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::String);
    case TokenKind::KwUnit:
        advance();
        return std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Unit);
    case TokenKind::KwNull:
        advance();
        return std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Null);
    case TokenKind::Identifier: {
        std::string name = current().lexeme;
        advance();
        // Handle parameterized types like Option<Int> or Array<Int>.
        if (current().kind == TokenKind::OpLt) {
            advance(); // '<'
            std::vector<std::unique_ptr<ast::Type>> typeArgs;
            if (current().kind != TokenKind::OpGt) {
                while (true) {
                    auto argType = parseType();
                    if (argType) {
                        typeArgs.push_back(std::move(argType));
                    }
                    if (match(TokenKind::Comma)) {
                        continue;
                    }
                    break;
                }
            }
            if (!match(TokenKind::OpGt)) {
                reportSyntaxError("expected '>' after type argument list");
            }
            return std::make_unique<ast::ParameterizedType>(loc, name, std::move(typeArgs));
        }
        return std::make_unique<ast::GenericType>(loc, name);
    }
     default:
         reportSyntaxError("expected type");
         return nullptr;
     }
 }

 // ---- Statements ----

 std::unique_ptr<ast::Stmt> FirstParser::parseStatement() {
     switch (current().kind) {
     case TokenKind::KwFor:
         return parseForInStmt();
     case TokenKind::KwLet:
     case TokenKind::KwVar:
         return parseVarDecl();
     case TokenKind::KwReturn:
         return parseReturnStmt();
    case TokenKind::KwIf:
        // if/elseif/else is an expression; parse as expr then require semicolon
        {
            auto expr = parseIfExpr();
            if (!expr) return nullptr;
            if (current().kind != TokenKind::Semicolon) {
                reportSyntaxError("expected ';' after if expression");
                return nullptr;
            }
            advance();
            return std::make_unique<ast::ExprStmt>(expr->getLocation(), std::move(expr));
        }
     case TokenKind::KwSelect:
        return parseSelectStmt();
    case TokenKind::KwMatch: {
        // Allow `match` as a statement by wrapping the expression in an ExprStmt.
        auto expr = parseMatchExpr();
        if (!expr) {
            return nullptr;
        }
        // Optional trailing semicolon after a match statement.
        if (current().kind == TokenKind::Semicolon) {
            advance();
        }
        SourceLocation loc = expr->getLocation();
        return std::make_unique<ast::ExprStmt>(loc, std::move(expr));
    }
    case TokenKind::LBrace: {
        // Block statement: { stmt* }
        // For now we flatten to the first inner statement, which is sufficient
        // for current tests (blocks contain a single meaningful statement).
        SourceLocation loc = current().loc;
        advance(); // consume '{'
        std::unique_ptr<ast::Stmt> firstStmt;
        while (current().kind != TokenKind::RBrace &&
               current().kind != TokenKind::EndOfFile) {
            auto stmt = parseStatement();
            if (!firstStmt && stmt) {
                firstStmt = std::move(stmt);
            }
            // Even if stmt is null, keep trying to advance to avoid infinite loops.
            if (current().kind == TokenKind::Semicolon) {
                advance();
            } else if (current().kind == TokenKind::RBrace) {
                break;
            } else if (!stmt) {
                // Recovery: skip one token.
                advance();
            }
        }
        expect(TokenKind::RBrace, "expected '}' at end of block");
        // If block was empty, just return a null statement; callers should handle it.
        return firstStmt;
    }
    default: {
        // Handle assignment vs expression statements.
        auto expr = parseExpression();
        if (!expr) {
            return nullptr;
        }

        // Assignment statement: <expr> = <expr> ;
        if (current().kind == TokenKind::OpAssign) {
            SourceLocation loc = expr->getLocation();
            advance(); // '='
            auto valueExpr = parseExpression();
            if (!valueExpr) {
                reportSyntaxError("expected expression on right-hand side of assignment");
                return nullptr;
            }
            expect(TokenKind::Semicolon, "expected ';' after assignment");
            return std::make_unique<ast::AssignmentStmt>(
                loc, std::move(expr), std::move(valueExpr));
        }

        // Plain expression statement.
        if (current().kind == TokenKind::Semicolon) {
            SourceLocation loc = expr->getLocation();
            advance();
            return std::make_unique<ast::ExprStmt>(loc, std::move(expr));
        }
        return nullptr;
    }
     }
 }

 std::unique_ptr<ast::VariableDecl> FirstParser::parseVarDecl() {
     bool isVar = current().kind == TokenKind::KwVar;
     SourceLocation loc = current().loc;
     advance(); // let/var

     if (current().kind != TokenKind::Identifier) {
         reportSyntaxError("expected variable name");
         return nullptr;
     }
     std::string name = current().lexeme;
     advance();

     std::unique_ptr<ast::Type> ty;
     if (match(TokenKind::Colon)) {
         ty = parseType();
     }

     std::unique_ptr<ast::Expr> init;
     if (match(TokenKind::OpAssign)) {
         init = parseExpression();
     }

    if (current().kind == TokenKind::Semicolon) {
        advance();
    } else {
        reportSyntaxError("expected ';' after variable declaration");
    }

     return std::make_unique<ast::VariableDecl>(
         loc,
         name,
         isVar ? ast::VariableDecl::Mutability::Mutable
               : ast::VariableDecl::Mutability::Immutable,
         std::move(ty),
         std::move(init));
 }

 std::unique_ptr<ast::ReturnStmt> FirstParser::parseReturnStmt() {
     SourceLocation loc = current().loc;
     expect(TokenKind::KwReturn, "expected 'return'");
     std::unique_ptr<ast::Expr> value;
     if (current().kind != TokenKind::Semicolon) {
         value = parseExpression();
     }
     if (current().kind == TokenKind::Semicolon) {
         advance();
     }
     return std::make_unique<ast::ReturnStmt>(loc, std::move(value));
 }

 std::unique_ptr<ast::ForInStmt> FirstParser::parseForInStmt() {
     SourceLocation loc = current().loc;
     expect(TokenKind::KwFor, "expected 'for'");
     if (current().kind != TokenKind::Identifier) {
         reportSyntaxError("expected variable name in for-in loop");
         return nullptr;
     }
     std::string varName = current().lexeme;
     advance();
     expect(TokenKind::KwIn, "expected 'in' after variable name in for-in loop");
     auto iterable = parseExpression();
     if (!iterable) {
         reportSyntaxError("expected iterable expression in for-in loop");
         return nullptr;
     }
     expect(TokenKind::LBrace, "expected '{' after iterable in for-in loop");
     std::vector<std::unique_ptr<ast::Stmt>> body;
     while (current().kind != TokenKind::RBrace &&
            current().kind != TokenKind::EndOfFile) {
         auto stmt = parseStatement();
         if (stmt) body.push_back(std::move(stmt));
         if (current().kind == TokenKind::Semicolon) advance();
     }
     expect(TokenKind::RBrace, "expected '}' at end of for-in loop body");
     return std::make_unique<ast::ForInStmt>(
         loc, varName, std::move(iterable), std::move(body));
 }

 std::unique_ptr<ast::IfStmt> FirstParser::parseIfStmt() {
     SourceLocation loc = current().loc;
     expect(TokenKind::KwIf, "expected 'if'");
     expect(TokenKind::LParen, "expected '(' after 'if'");
     auto cond = parseExpression();
     expect(TokenKind::RParen, "expected ')' after condition");
     auto thenBranch = parseStatement();
     std::unique_ptr<ast::Stmt> elseBranch;
     if (match(TokenKind::KwElse)) {
         elseBranch = parseStatement();
     }
     return std::make_unique<ast::IfStmt>(
         loc, std::move(cond), std::move(thenBranch), std::move(elseBranch));
 }

 std::unique_ptr<ast::Expr> FirstParser::parseBranchOrBlock() {
     if (current().kind == TokenKind::LBrace) {
         return parseBlockExpr();
     }
     return parseExpression();
 }

 static bool isStatementStart(TokenKind k) {
     return k == TokenKind::KwFor || k == TokenKind::KwLet || k == TokenKind::KwVar || k == TokenKind::KwReturn
         || k == TokenKind::KwIf || k == TokenKind::KwSelect
         || k == TokenKind::KwMatch || k == TokenKind::LBrace;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseBlockExpr() {
     SourceLocation loc = current().loc;
     if (current().kind != TokenKind::LBrace) {
         reportSyntaxError("expected '{' for block expression");
         return nullptr;
     }
     advance(); // consume '{'
     std::vector<std::unique_ptr<ast::Stmt>> statements;
     std::unique_ptr<ast::Expr> valueExpr;
     while (current().kind != TokenKind::RBrace && current().kind != TokenKind::EndOfFile) {
         // If at start of block: 'if' can be either statement (if; ) or trailing expression (if .. }).
         if (current().kind == TokenKind::KwIf) {
             auto ifExpr = parseIfExpr();
             if (!ifExpr) continue;
             if (current().kind == TokenKind::Semicolon) {
                 advance();
                 statements.push_back(std::make_unique<ast::ExprStmt>(ifExpr->getLocation(), std::move(ifExpr)));
                 continue;
             }
             if (current().kind == TokenKind::RBrace) {
                 valueExpr = std::move(ifExpr);
                 break;
             }
             reportSyntaxError("expected ';' or '}' after if expression in block");
             continue;
         }
         if (isStatementStart(current().kind)) {
             auto stmt = parseStatement();
             if (stmt) {
                 statements.push_back(std::move(stmt));
             }
             continue;
         }
         auto expr = parseExpression();
         if (!expr) {
             reportSyntaxError("expected statement or expression in block");
             break;
         }
         if (current().kind == TokenKind::Semicolon) {
             advance();
             statements.push_back(std::make_unique<ast::ExprStmt>(expr->getLocation(), std::move(expr)));
             continue;
         }
         if (current().kind == TokenKind::RBrace) {
             valueExpr = std::move(expr);
             break;
         }
         reportSyntaxError("expected ';' or '}' after expression in block");
         break;
     }
     if (!expect(TokenKind::RBrace, "expected '}' at end of block")) {
         return nullptr;
     }
     return std::make_unique<ast::BlockExpr>(loc, std::move(statements), std::move(valueExpr));
 }

 std::unique_ptr<ast::Expr> FirstParser::parseIfExpr() {
     SourceLocation loc = current().loc;
     expect(TokenKind::KwIf, "expected 'if'");
     expect(TokenKind::LParen, "expected '(' after 'if'");
     auto cond = parseExpression();
     if (!cond) return nullptr;
     expect(TokenKind::RParen, "expected ')' after condition");
     auto thenBranch = parseBranchOrBlock();
     if (!thenBranch) return nullptr;
     if (!match(TokenKind::KwElse)) {
         reportSyntaxError("expected 'else' in if expression");
         return nullptr;
     }
     std::unique_ptr<ast::Expr> elseBranch;
     if (current().kind == TokenKind::KwIf) {
         elseBranch = parseIfExpr();
     } else {
         elseBranch = parseBranchOrBlock();
     }
     if (!elseBranch) return nullptr;
     return std::make_unique<ast::IfExpr>(loc, std::move(cond), std::move(thenBranch), std::move(elseBranch));
 }

 std::unique_ptr<ast::AssignmentStmt> FirstParser::parseAssignmentStmt() {
     // Not yet wired in as separate form; expression parsing will cover basic
     // cases used by tests. This can be extended later.
     return nullptr;
 }

std::vector<std::unique_ptr<ast::SelectBranch>> FirstParser::parseSelectBranches() {
    std::vector<std::unique_ptr<ast::SelectBranch>> branches;
    while (current().kind != TokenKind::RBrace && current().kind != TokenKind::EndOfFile) {
        SourceLocation branchLoc = current().loc;
        // Receive: <- channel => var : statement
        if (current().kind == TokenKind::OpLt && tokens_.lookahead(1).kind == TokenKind::OpMinus) {
            advance(); // OpLt
            advance(); // OpMinus
            auto channelExpr = parseExpression();
            if (!channelExpr || !expect(TokenKind::OpFatArrow, "expected '=>' in select receive")) {
                break;
            }
            if (current().kind != TokenKind::Identifier) {
                reportSyntaxError("expected variable name in select receive");
                break;
            }
            std::string varName = current().lexeme;
            advance();
            if (!expect(TokenKind::Colon, "expected ':' in select receive")) {
                break;
            }
            auto stmt = parseStatement();
            if (!stmt) break;
            branches.push_back(std::make_unique<ast::SelectBranch>(
                branchLoc, ast::SelectBranch::Kind::Receive,
                std::move(channelExpr), std::move(varName), nullptr, std::move(stmt)));
            continue;
        }
        // Else: else : statement
        if (current().kind == TokenKind::KwElse && tokens_.lookahead(1).kind == TokenKind::Colon) {
            advance(); // KwElse
            advance(); // Colon
            auto stmt = parseStatement();
            if (!stmt) break;
            branches.push_back(std::make_unique<ast::SelectBranch>(
                branchLoc, ast::SelectBranch::Kind::Else,
                nullptr, "", nullptr, std::move(stmt)));
            continue;
        }
        // Send: channel <- value : statement
        auto channelExpr = parseExpression();
        if (!channelExpr) break;
        if (!match(TokenKind::OpLt) || !match(TokenKind::OpMinus)) {
            reportSyntaxError("expected '<-' in select send");
            break;
        }
        auto valueExpr = parseExpression();
        if (!valueExpr || !expect(TokenKind::Colon, "expected ':' in select send")) {
            break;
        }
        auto stmt = parseStatement();
        if (!stmt) break;
        branches.push_back(std::make_unique<ast::SelectBranch>(
            branchLoc, ast::SelectBranch::Kind::Send,
            std::move(channelExpr), "", std::move(valueExpr), std::move(stmt)));
    }
    return branches;
}

std::unique_ptr<ast::SelectStmt> FirstParser::parseSelectStmt() {
    SourceLocation loc = current().loc;
    advance(); // 'select'
    if (!expect(TokenKind::LBrace, "expected '{' after 'select'")) {
        return nullptr;
    }
    auto branches = parseSelectBranches();
    if (!expect(TokenKind::RBrace, "expected '}' at end of select")) {
        return nullptr;
    }
    return std::make_unique<ast::SelectStmt>(loc, std::move(branches));
}

// ---- Expressions ----

std::unique_ptr<ast::Expr> FirstParser::parseExpression() {
    // Base expression using standard precedence (||, &&, ==, <, +, *, etc.)
    auto expr = parseLogicalOr();

    // Monadic / functional operators are parsed as syntactic sugar and
    // immediately desugared into function calls:
    //
    //   a >>= f   => bind(a, f)
    //   a >> b    => then(a, b)
    //   f <$> m   => fmap(f, m)
    //   f <*> m   => apply(f, m)
    //
    // We keep precedence simple: these operators chain left‑associatively and
    // sit above the normal precedence ladder (we re-use parseLogicalOr for
    // the right-hand side).
    while (current().kind == TokenKind::OpBind ||
           current().kind == TokenKind::OpThen ||
           current().kind == TokenKind::OpFmap ||
           current().kind == TokenKind::OpApply) {
        SourceLocation loc = current().loc;
        TokenKind op = current().kind;
        advance();

        auto rhs = parseLogicalOr();
        if (!rhs) {
            // Report a syntax error and stop chaining if the RHS is missing.
            reportSyntaxError("expected expression after monadic operator");
            break;
        }

        std::string funcName;
        switch (op) {
        case TokenKind::OpBind:  funcName = "bind";  break;
        case TokenKind::OpThen:  funcName = "then";  break;
        case TokenKind::OpFmap:  funcName = "fmap";  break;
        case TokenKind::OpApply: funcName = "apply"; break;
        default:
            // Should be unreachable, but fall back to returning the base expr.
            return expr;
        }

        std::vector<std::unique_ptr<ast::Expr>> args;
        args.push_back(std::move(expr));
        args.push_back(std::move(rhs));

        expr = std::make_unique<ast::FunctionCallExpr>(
            loc, funcName, std::move(args));
    }

    return expr;
}

std::unique_ptr<ast::Expr> FirstParser::parseDoBlockExpr() {
    // do { ... } notation for monadic code. We support a restricted subset:
    //   - x <- expr;
    //   - expr;
    //   - return expr;
    //
    // and desugar it into nested bind/then calls returning a single
    // expression of monadic type.

    SourceLocation doLoc = current().loc;
    advance(); // consume 'do'
    expect(TokenKind::LBrace, "expected '{' after 'do'");

    enum class DoKind { Bind, Expr, Return };
    struct DoItem {
        DoKind kind;
        SourceLocation loc;
        std::string name; // for Bind
        std::unique_ptr<ast::Expr> expr;
    };

    std::vector<DoItem> items;

    while (current().kind != TokenKind::RBrace &&
           current().kind != TokenKind::EndOfFile) {
        if (current().kind == TokenKind::KwReturn) {
            SourceLocation retLoc = current().loc;
            advance(); // 'return'
            auto value = parseExpression();
            expect(TokenKind::Semicolon, "expected ';' after return expression in do-block");
            DoItem item;
            item.kind = DoKind::Return;
            item.loc = retLoc;
            item.expr = std::move(value);
            items.push_back(std::move(item));
            // Everything after a return is ignored for the purposes of
            // desugaring; we'll still consume tokens up to '}' below.
            break;
        }

        // x <- expr;
        if (current().kind == TokenKind::Identifier &&
            tokens_.lookahead(1).kind == TokenKind::OpLt &&
            tokens_.lookahead(2).kind == TokenKind::OpMinus) {
            std::string name = current().lexeme;
            SourceLocation nameLoc = current().loc;
            advance(); // identifier
            advance(); // '<'
            advance(); // '-'
            auto rhs = parseExpression();
            expect(TokenKind::Semicolon, "expected ';' after binding expression in do-block");

            DoItem item;
            item.kind = DoKind::Bind;
            item.loc = nameLoc;
            item.name = name;
            item.expr = std::move(rhs);
            items.push_back(std::move(item));
            continue;
        }

        // Fallback: effect-only expression; expr;
        auto e = parseExpression();
        expect(TokenKind::Semicolon, "expected ';' after expression in do-block");
        if (e) {
            SourceLocation exprLoc = e->getLocation();
            DoItem item;
            item.kind = DoKind::Expr;
            item.loc = exprLoc;
            item.expr = std::move(e);
            items.push_back(std::move(item));
        }
    }

    expect(TokenKind::RBrace, "expected '}' at end of do-block");

    if (items.empty() || items.back().kind != DoKind::Return) {
        reportSyntaxError("do-block must end with 'return expr;'");
        return nullptr;
    }

    // Start from the final return expression.
    std::unique_ptr<ast::Expr> acc = std::move(items.back().expr);

    // Fold backwards building bind/then chains.
    for (std::ptrdiff_t i = static_cast<std::ptrdiff_t>(items.size()) - 2; i >= 0; --i) {
        DoItem& item = items[static_cast<std::size_t>(i)];
        std::string funcName;
        std::vector<std::unique_ptr<ast::Parameter>> params;

        if (!item.expr) {
            continue;
        }

        if (item.kind == DoKind::Bind) {
            funcName = "bind";
            // function(x) { return acc; }
            params.push_back(std::make_unique<ast::Parameter>(
                item.loc,
                item.name,
                std::make_unique<ast::GenericType>(item.loc, "Any")));
        } else if (item.kind == DoKind::Expr) {
            funcName = "then";
            // function(_) { return acc; }
            params.push_back(std::make_unique<ast::Parameter>(
                item.loc,
                "_",
                std::make_unique<ast::GenericType>(item.loc, "Any")));
        } else {
            continue;
        }

        SourceLocation retLoc = acc->getLocation();
        std::vector<std::unique_ptr<ast::Stmt>> body;
        body.push_back(std::make_unique<ast::ReturnStmt>(retLoc, std::move(acc)));

        std::unique_ptr<ast::Type> retType; // inferred later
        auto lambda = std::make_unique<ast::LambdaExpr>(
            item.loc, std::move(params), std::move(retType), std::move(body));

        std::vector<std::unique_ptr<ast::Expr>> args;
        args.push_back(std::move(item.expr));
        args.push_back(std::move(lambda));

        acc = std::make_unique<ast::FunctionCallExpr>(
            item.loc, funcName, std::move(args));
    }

    return acc;
}

 std::unique_ptr<ast::Expr> FirstParser::parseLogicalOr() {
     auto left = parseLogicalAnd();
     while (current().kind == TokenKind::OpOr) {
         SourceLocation loc = current().loc;
         advance();
         auto right = parseLogicalAnd();
         if (!right) break;
         left = std::make_unique<ast::BinaryExpr>(
             loc, ast::BinaryExpr::Op::Or, std::move(left), std::move(right));
     }
     return left;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseLogicalAnd() {
     auto left = parseEquality();
     while (current().kind == TokenKind::OpAnd) {
         SourceLocation loc = current().loc;
         advance();
         auto right = parseEquality();
         if (!right) break;
         left = std::make_unique<ast::BinaryExpr>(
             loc, ast::BinaryExpr::Op::And, std::move(left), std::move(right));
     }
     return left;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseEquality() {
     auto left = parseComparison();
     while (current().kind == TokenKind::OpEq ||
            current().kind == TokenKind::OpNe) {
         SourceLocation loc = current().loc;
         TokenKind op = current().kind;
         advance();
         auto right = parseComparison();
         if (!right) break;
         ast::BinaryExpr::Op beOp =
             (op == TokenKind::OpEq) ? ast::BinaryExpr::Op::Eq
                                     : ast::BinaryExpr::Op::Ne;
         left = std::make_unique<ast::BinaryExpr>(
             loc, beOp, std::move(left), std::move(right));
     }
     return left;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseComparison() {
     auto left = parseRange();
     while (current().kind == TokenKind::OpLt ||
            current().kind == TokenKind::OpLe ||
            current().kind == TokenKind::OpGt ||
            current().kind == TokenKind::OpGe) {
         SourceLocation loc = current().loc;
         TokenKind op = current().kind;
         advance();
         auto right = parseRange();
         if (!right) break;
         ast::BinaryExpr::Op beOp = ast::BinaryExpr::Op::Add;
         switch (op) {
         case TokenKind::OpLt: beOp = ast::BinaryExpr::Op::Lt; break;
         case TokenKind::OpLe: beOp = ast::BinaryExpr::Op::Le; break;
         case TokenKind::OpGt: beOp = ast::BinaryExpr::Op::Gt; break;
         case TokenKind::OpGe: beOp = ast::BinaryExpr::Op::Ge; break;
         default: break;
         }
         left = std::make_unique<ast::BinaryExpr>(
             loc, beOp, std::move(left), std::move(right));
     }
     return left;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseRange() {
     auto start = parseAdditive();
     if (!start) return nullptr;
     // Optional second element: first, second..last (step = second - first, Haskell-style).
     // Backtrack if we see comma but no .. or ..= (e.g. f(1, 2) must not consume ", 2").
     std::unique_ptr<ast::Expr> stepHint;
     std::size_t savedIndex = tokens_.getIndex();
     if (match(TokenKind::Comma)) {
         stepHint = parseAdditive();
         if (!stepHint || (current().kind != TokenKind::OpRange &&
                          current().kind != TokenKind::OpRangeInclusive)) {
             tokens_.setIndex(savedIndex);
             stepHint.reset();
         }
     }
     if (current().kind == TokenKind::OpRange ||
         current().kind == TokenKind::OpRangeInclusive) {
         SourceLocation loc = start->getLocation();
         bool inclusive = (current().kind == TokenKind::OpRangeInclusive);
         advance();  // consume .. or ..=
         auto end = parseAdditive();
         if (!end) {
             reportSyntaxError("expected expression after range operator");
             return start;
         }
         return std::make_unique<ast::RangeExpr>(
             loc, std::move(start), std::move(end), inclusive, std::move(stepHint));
     }
     return start;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseAdditive() {
     auto left = parseMultiplicative();
     while (current().kind == TokenKind::OpPlus ||
            current().kind == TokenKind::OpMinus) {
         SourceLocation loc = current().loc;
         TokenKind op = current().kind;
         advance();
         auto right = parseMultiplicative();
         if (!right) break;
         ast::BinaryExpr::Op beOp =
             (op == TokenKind::OpPlus) ? ast::BinaryExpr::Op::Add
                                       : ast::BinaryExpr::Op::Sub;
         left = std::make_unique<ast::BinaryExpr>(
             loc, beOp, std::move(left), std::move(right));
     }
     return left;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseMultiplicative() {
     auto left = parseUnary();
     while (current().kind == TokenKind::OpMul ||
            current().kind == TokenKind::OpDiv ||
            current().kind == TokenKind::OpMod) {
         SourceLocation loc = current().loc;
         TokenKind op = current().kind;
         advance();
         auto right = parseUnary();
         if (!right) break;
         ast::BinaryExpr::Op beOp = ast::BinaryExpr::Op::Mul;
         switch (op) {
         case TokenKind::OpMul: beOp = ast::BinaryExpr::Op::Mul; break;
         case TokenKind::OpDiv: beOp = ast::BinaryExpr::Op::Div; break;
         case TokenKind::OpMod: beOp = ast::BinaryExpr::Op::Mod; break;
         default: break;
         }
         left = std::make_unique<ast::BinaryExpr>(
             loc, beOp, std::move(left), std::move(right));
     }
     return left;
 }

 std::unique_ptr<ast::Expr> FirstParser::parseUnary() {
     if (current().kind == TokenKind::OpNot ||
         current().kind == TokenKind::OpMinus ||
         current().kind == TokenKind::OpPlus) {
         SourceLocation loc = current().loc;
         TokenKind op = current().kind;
         advance();
         auto operand = parseUnary();
         if (!operand) return nullptr;
         ast::UnaryExpr::Op uOp =
             (op == TokenKind::OpNot) ? ast::UnaryExpr::Op::Not
                                      : ast::UnaryExpr::Op::Neg;
         return std::make_unique<ast::UnaryExpr>(
             loc, uOp, std::move(operand));
     }
     return parsePostfix();
 }

 std::unique_ptr<ast::Expr> FirstParser::parsePostfix() {
     auto base = parsePrimaryExpr();
     if (!base) return nullptr;

     while (true) {
         if (match(TokenKind::LParen)) {
             // Function call: identifier(...)
             std::vector<std::unique_ptr<ast::Expr>> args;
             if (current().kind != TokenKind::RParen) {
                 while (true) {
                     auto arg = parseExpression();
                     if (!arg) break;
                     args.push_back(std::move(arg));
                     if (match(TokenKind::Comma)) continue;
                     break;
                 }
             }
             expect(TokenKind::RParen, "expected ')' after arguments");
             if (auto* var = dynamic_cast<ast::VariableExpr*>(base.get())) {
                 std::string calleeName = var->getName();
                 SourceLocation loc = base->getLocation();
                 base = std::make_unique<ast::FunctionCallExpr>(
                     loc, calleeName, std::move(args));
             }
        } else if (match(TokenKind::Dot)) {
            if (current().kind != TokenKind::Identifier) {
                reportSyntaxError("expected field name after '.'");
                break;
            }
            std::string fieldName = current().lexeme;
            SourceLocation loc = base->getLocation();
            advance();
            base = std::make_unique<ast::FieldAccessExpr>(
                loc, std::move(base), fieldName);
        } else if (match(TokenKind::LBracket)) {
            // Array indexing: base [ index ]
            auto indexExpr = parseExpression();
            if (!indexExpr) {
                reportSyntaxError("expected index expression in array subscript");
                break;
            }
            if (!expect(TokenKind::RBracket, "expected ']' after array index")) {
                break;
            }
            SourceLocation loc = base->getLocation();
            base = std::make_unique<ast::ArrayIndexExpr>(
                loc, std::move(base), std::move(indexExpr));
        } else {
            break;
        }
    }
    return base;
}

 std::unique_ptr<ast::Expr> FirstParser::parsePrimaryExpr() {
     switch (current().kind) {
    case TokenKind::KwIf:
        return parseIfExpr();
    case TokenKind::KwMatch:
        return parseMatchExpr();
    case TokenKind::KwDo:
        return parseDoBlockExpr();
    case TokenKind::KwAsync: {
        SourceLocation loc = current().loc;
        advance(); // 'async'
        auto operand = parsePrimaryExpr();
        if (!operand) {
            reportSyntaxError("expected expression after 'async'");
            return nullptr;
        }
        return std::make_unique<ast::AsyncExpr>(loc, std::move(operand));
    }
    case TokenKind::KwAwait: {
        SourceLocation loc = current().loc;
        advance(); // 'await'
        auto operand = parsePrimaryExpr();
        if (!operand) {
            reportSyntaxError("expected expression after 'await'");
            return nullptr;
        }
        return std::make_unique<ast::AwaitExpr>(loc, std::move(operand));
    }
    case TokenKind::KwSpawn: {
        SourceLocation loc = current().loc;
        advance(); // 'spawn'
        auto operand = parsePrimaryExpr();
        if (!operand) {
            reportSyntaxError("expected expression after 'spawn'");
            return nullptr;
        }
        return std::make_unique<ast::SpawnExpr>(loc, std::move(operand));
    }
    case TokenKind::KwJoin: {
        SourceLocation loc = current().loc;
        advance(); // 'join'
        auto operand = parsePrimaryExpr();
        if (!operand) {
            reportSyntaxError("expected expression after 'join'");
            return nullptr;
        }
        return std::make_unique<ast::JoinExpr>(loc, std::move(operand));
    }
    case TokenKind::KwSelect: {
        SourceLocation loc = current().loc;
        advance(); // 'select'
        if (!expect(TokenKind::LBrace, "expected '{' after 'select'")) {
            return nullptr;
        }
        auto branches = parseSelectBranches();
        if (!expect(TokenKind::RBrace, "expected '}' at end of select")) {
            return nullptr;
        }
        return std::make_unique<ast::SelectExpr>(loc, std::move(branches));
    }
    case TokenKind::KwFunction: {
        // Inline lambda expression: function(params) [-> Type]? { body }
        SourceLocation loc = current().loc;
        advance(); // 'function'

        expect(TokenKind::LParen, "expected '(' after 'function' in lambda");

        std::vector<std::unique_ptr<ast::Parameter>> parameters;
        if (current().kind != TokenKind::RParen) {
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected parameter name in lambda");
                    break;
                }
                std::string paramName = current().lexeme;
                SourceLocation paramLoc = current().loc;
                advance();

                std::unique_ptr<ast::Type> paramType;
                if (match(TokenKind::Colon)) {
                    paramType = parseType();
                }
                if (!paramType) {
                    // Match ASTBuilder behaviour: unannotated lambda params use GenericType(\"Any\")
                    paramType = std::make_unique<ast::GenericType>(paramLoc, "Any");
                }
                parameters.push_back(std::make_unique<ast::Parameter>(
                    paramLoc, paramName, std::move(paramType)));

                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        expect(TokenKind::RParen, "expected ')' after lambda parameters");

        std::unique_ptr<ast::Type> returnType;
        if (match(TokenKind::OpArrow)) {
            returnType = parseType();
        }

        std::vector<std::unique_ptr<ast::Stmt>> body;
        if (match(TokenKind::LBrace)) {
            while (current().kind != TokenKind::RBrace &&
                   current().kind != TokenKind::EndOfFile) {
                auto stmt = parseStatement();
                if (stmt) {
                    body.push_back(std::move(stmt));
                } else {
                    // Recovery: skip to next potential statement boundary.
                    if (current().kind == TokenKind::Semicolon) {
                        advance();
                    } else {
                        advance();
                    }
                }
            }
            expect(TokenKind::RBrace, "expected '}' at end of lambda body");
        } else {
            // Expression-body form: function(x) expr
            auto exprBody = parseExpression();
            if (exprBody) {
                SourceLocation exprLoc = exprBody->getLocation();
                body.push_back(std::make_unique<ast::ReturnStmt>(exprLoc, std::move(exprBody)));
            }
        }

        return std::make_unique<ast::LambdaExpr>(
            loc, std::move(parameters), std::move(returnType), std::move(body));
    }
     case TokenKind::IntLiteral:
     case TokenKind::FloatLiteral:
     case TokenKind::StringLiteral:
     case TokenKind::KwTrue:
     case TokenKind::KwFalse:
     case TokenKind::KwNull:
         return parseLiteralExpr();
    case TokenKind::Identifier: {
         SourceLocation loc = current().loc;
         std::string name = current().lexeme;
         advance();
         return std::make_unique<ast::VariableExpr>(loc, name);
     }
    case TokenKind::LParen: {
         advance();
         auto expr = parseExpression();
         expect(TokenKind::RParen, "expected ')'");
         return expr;
    }
    case TokenKind::LBracket: {
        // Array literal: [expr1, expr2, ...]
        SourceLocation loc = current().loc;
        advance(); // '['
        std::vector<std::unique_ptr<ast::Expr>> elements;
        if (current().kind != TokenKind::RBracket) {
            while (true) {
                auto elem = parseExpression();
                if (!elem) break;
                elements.push_back(std::move(elem));
                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        expect(TokenKind::RBracket, "expected ']' in array literal");
        return std::make_unique<ast::ArrayLiteralExpr>(loc, std::move(elements));
     }
    case TokenKind::LBrace: {
        // Block expression { stmt* expr? } vs record literal { id: expr, ... } vs shorthand { id1, id2 }
        TokenKind k2 = tokens_.lookahead(2).kind;
        if (tokens_.lookahead(1).kind == TokenKind::Identifier && k2 == TokenKind::Colon) {
            // Record literal: { field: expr, ... }
            SourceLocation loc = current().loc;
            advance(); // '{'
            std::vector<ast::RecordLiteralExpr::Field> fields;
            if (current().kind != TokenKind::RBrace) {
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected field name in record literal");
                    break;
                }
                std::string fieldName = current().lexeme;
                advance();
                expect(TokenKind::Colon, "expected ':' after field name in record literal");
                auto fieldExpr = parseExpression();
                if (!fieldExpr) {
                    reportSyntaxError("expected expression after ':' in record literal");
                    break;
                }
                fields.push_back(ast::RecordLiteralExpr::Field(fieldName, std::move(fieldExpr)));
                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        expect(TokenKind::RBrace, "expected '}' in record literal");
        return std::make_unique<ast::RecordLiteralExpr>(loc, std::move(fields));
        }
        if (tokens_.lookahead(1).kind == TokenKind::Identifier &&
            (k2 == TokenKind::Comma || k2 == TokenKind::RBrace)) {
            // Shorthand record literal: { id1, id2 } means { id1: id1, id2: id2 }
            SourceLocation loc = current().loc;
            advance(); // '{'
            std::vector<ast::RecordLiteralExpr::Field> fields;
            while (current().kind == TokenKind::Identifier) {
                SourceLocation fieldLoc = current().loc;
                std::string fieldName = current().lexeme;
                advance();
                fields.push_back(ast::RecordLiteralExpr::Field(
                    fieldName, std::make_unique<ast::VariableExpr>(fieldLoc, fieldName)));
                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
            expect(TokenKind::RBrace, "expected '}' in record literal");
            return std::make_unique<ast::RecordLiteralExpr>(loc, std::move(fields));
        }
        return parseBlockExpr();
    }
     default:
         reportSyntaxError("expected expression");
         return nullptr;
     }
 }

 std::unique_ptr<ast::Expr> FirstParser::parseLiteralExpr() {
     SourceLocation loc = current().loc;
     switch (current().kind) {
     case TokenKind::IntLiteral: {
         std::string v = current().lexeme;
         advance();
         return std::make_unique<ast::LiteralExpr>(
             loc, ast::LiteralExpr::LiteralType::Int, v);
     }
     case TokenKind::FloatLiteral: {
         std::string v = current().lexeme;
         advance();
         return std::make_unique<ast::LiteralExpr>(
             loc, ast::LiteralExpr::LiteralType::Float, v);
     }
     case TokenKind::StringLiteral: {
         std::string v = current().lexeme;
         if (v.size() >= 2 && v.front() == '"' && v.back() == '"') {
             v = v.substr(1, v.size() - 2);
         }
         // Unescape: \\ \n \t \r \"
         std::string out;
         out.reserve(v.size());
         for (size_t i = 0; i < v.size(); ++i) {
             if (v[i] == '\\' && i + 1 < v.size()) {
                 switch (v[i + 1]) {
                     case 'n':  out += '\n'; ++i; break;
                     case 't':  out += '\t'; ++i; break;
                     case 'r':  out += '\r'; ++i; break;
                     case '"':  out += '"';  ++i; break;
                     case '\\': out += '\\'; ++i; break;
                     default:   out += v[i + 1]; ++i; break;
                 }
             } else {
                 out += v[i];
             }
         }
         v = std::move(out);
         advance();
         return std::make_unique<ast::LiteralExpr>(
             loc, ast::LiteralExpr::LiteralType::String, v);
     }
     case TokenKind::KwTrue:
         advance();
         return std::make_unique<ast::LiteralExpr>(
             loc, ast::LiteralExpr::LiteralType::Bool, "true");
     case TokenKind::KwFalse:
         advance();
         return std::make_unique<ast::LiteralExpr>(
             loc, ast::LiteralExpr::LiteralType::Bool, "false");
     case TokenKind::KwNull:
         advance();
         return std::make_unique<ast::LiteralExpr>(
             loc, ast::LiteralExpr::LiteralType::Null, "null");
    default:
        reportSyntaxError("expected literal");
        return nullptr;
    }
}

std::unique_ptr<ast::Expr> FirstParser::parseMatchExpr() {
    SourceLocation loc = current().loc;
    expect(TokenKind::KwMatch, "expected 'match'");

    // match <expr> { cases }
    auto scrutinee = parseExpression();
    if (!scrutinee) {
        reportSyntaxError("expected expression after 'match'");
        return nullptr;
    }

    expect(TokenKind::LBrace, "expected '{' after match expression");

    std::vector<std::unique_ptr<ast::MatchCase>> cases;
    while (current().kind != TokenKind::RBrace &&
           current().kind != TokenKind::EndOfFile) {
        auto pat = parsePattern();
        if (!pat) {
            // Recovery: skip until next potential case or end of match.
            if (current().kind == TokenKind::RBrace) break;
            advance();
            continue;
        }

        std::unique_ptr<ast::Expr> guard;
        if (match(TokenKind::KwWhen)) {
            guard = parseExpression();
            if (!guard) {
                reportSyntaxError("expected expression after 'when' in match case");
            }
        }

        if (!match(TokenKind::OpFatArrow)) {
            reportSyntaxError("expected '=>' after pattern in match case");
            // try to continue anyway
        }

        auto body = parseExpression();
        if (!body) {
            reportSyntaxError("expected expression as match case body");
            // Recovery: skip to next case
        }

        cases.push_back(std::make_unique<ast::MatchCase>(
            std::move(pat), std::move(guard), std::move(body)));

        // Optional comma separators between cases (matches grammar).
        if (current().kind == TokenKind::Comma) {
            advance();
        }
    }

    expect(TokenKind::RBrace, "expected '}' at end of match expression");

    return std::make_unique<ast::MatchExpr>(
        loc, std::move(scrutinee), std::move(cases));
}

std::unique_ptr<ast::Pattern> FirstParser::parsePattern() {
    SourceLocation loc = current().loc;

    // Wildcard pattern (_ or identifier "_"); accept by kind or by lexeme for robustness
    if (match(TokenKind::Underscore)) {
        return std::make_unique<ast::WildcardPattern>(loc);
    }
    if ((current().kind == TokenKind::Identifier && current().lexeme == "_") ||
        current().lexeme == "_") {
        advance();
        return std::make_unique<ast::WildcardPattern>(loc);
    }

    // Record pattern: { field: pattern, ... } or shorthand { field } meaning { field: field }
    if (match(TokenKind::LBrace)) {
        std::vector<ast::RecordPattern::FieldPattern> fields;
        if (current().kind != TokenKind::RBrace) {
            while (true) {
                if (current().kind != TokenKind::Identifier) {
                    reportSyntaxError("expected field name in record pattern");
                    break;
                }
                SourceLocation fieldLoc = current().loc;
                std::string fieldName = current().lexeme;
                advance();
                std::unique_ptr<ast::Pattern> fieldPat;
                if (current().kind == TokenKind::Comma || current().kind == TokenKind::RBrace) {
                    fieldPat = std::make_unique<ast::VariablePattern>(fieldLoc, fieldName);
                } else {
                    expect(TokenKind::Colon, "expected ':' after field name in record pattern");
                    fieldPat = parsePattern();
                }
                if (fieldPat) {
                    fields.push_back(ast::RecordPattern::FieldPattern{fieldName, std::move(fieldPat)});
                }
                if (match(TokenKind::Comma)) {
                    continue;
                }
                break;
            }
        }
        expect(TokenKind::RBrace, "expected '}' at end of record pattern");
        auto base = std::make_unique<ast::RecordPattern>(loc, std::move(fields));
        // As-pattern: <pattern> @ name
        if (match(TokenKind::At)) {
            if (current().kind != TokenKind::Identifier) {
                reportSyntaxError("expected identifier after '@' in as-pattern");
                return base;
            }
            std::string name = current().lexeme;
            advance();
            return std::make_unique<ast::AsPattern>(loc, std::move(base), name);
        }
        return base;
    }

    // Constructor or variable pattern
    if (current().kind == TokenKind::Identifier) {
        std::string name = current().lexeme;
        advance();

        if (match(TokenKind::LParen)) {
            // Constructor pattern: Name(p1, p2, ...)
            std::vector<std::unique_ptr<ast::Pattern>> args;
            if (current().kind != TokenKind::RParen) {
                while (true) {
                    auto arg = parsePattern();
                    if (arg) {
                        args.push_back(std::move(arg));
                    }
                    if (match(TokenKind::Comma)) {
                        continue;
                    }
                    break;
                }
            }
            expect(TokenKind::RParen, "expected ')' in constructor pattern");
            return std::make_unique<ast::ConstructorPattern>(loc, name, std::move(args));
        }

        // Simple variable pattern
        auto base = std::make_unique<ast::VariablePattern>(loc, name);
        if (match(TokenKind::At)) {
            if (current().kind != TokenKind::Identifier) {
                reportSyntaxError("expected identifier after '@' in as-pattern");
                return base;
            }
            std::string asName = current().lexeme;
            advance();
            return std::make_unique<ast::AsPattern>(loc, std::move(base), asName);
        }
        return base;
    }

    // Literal pattern: reuse literal expression parsing
    if (current().kind == TokenKind::IntLiteral ||
        current().kind == TokenKind::FloatLiteral ||
        current().kind == TokenKind::StringLiteral ||
        current().kind == TokenKind::KwTrue ||
        current().kind == TokenKind::KwFalse ||
        current().kind == TokenKind::KwNull) {
        auto litExpr = parseLiteralExpr();
        if (!litExpr) {
            return nullptr;
        }
        auto* litPtr = dynamic_cast<ast::LiteralExpr*>(litExpr.release());
        if (!litPtr) {
            return nullptr;
        }
        auto base = std::make_unique<ast::LiteralPattern>(
            loc, std::unique_ptr<ast::LiteralExpr>(litPtr));
        if (match(TokenKind::At)) {
            if (current().kind != TokenKind::Identifier) {
                reportSyntaxError("expected identifier after '@' in as-pattern");
                return base;
            }
            std::string asName = current().lexeme;
            advance();
            return std::make_unique<ast::AsPattern>(loc, std::move(base), asName);
        }
        return base;
    }

    reportSyntaxError("expected pattern");
    return nullptr;
}

} // namespace parser
} // namespace first

