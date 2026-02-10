 #pragma once

 #include "first/ast/program.h"
 #include "first/ast/expressions.h"
 #include "first/ast/statements.h"
 #include "first/ast/types.h"
#include "first/ast/declarations.h"
#include "first/ast/patterns.h"
#include "first/ast/match_case.h"
 #include "first/error_reporter.h"
 #include "first/parser/token.h"
 #include <memory>
 #include <vector>

 namespace antlr4 {
 class CommonTokenStream;
 }

 namespace first {

 class ErrorReporter;

 namespace parser {

 // Adapter that consumes an ANTLR CommonTokenStream and exposes Token objects
 // for the custom recursive-descent parser.
 class TokenStreamAdapter {
 public:
     TokenStreamAdapter(antlr4::CommonTokenStream& stream,
                        const std::string& filename);

     const Token& current() const { return current_; }
     const Token& lookahead(std::size_t n);

    void advance();

    std::size_t getIndex() const { return index_; }
    void setIndex(std::size_t index);

 private:
    antlr4::CommonTokenStream& stream_;
     std::string filename_;
     std::size_t index_;
     Token current_;

     Token makeTokenFromAntlr(int rawType, const std::string& text,
                              std::size_t line, std::size_t column);
     void refreshCurrent();
 };

 // Custom recursive-descent parser for the First language.
 class FirstParser {
 public:
     FirstParser(TokenStreamAdapter& tokens,
                 first::ErrorReporter& errorReporter,
                 const std::string& filename);

     std::unique_ptr<ast::Program> parseProgram();

 private:
     TokenStreamAdapter& tokens_;
     first::ErrorReporter& errorReporter_;
     std::string filename_;

     // Utility
     const Token& current() const { return tokens_.current(); }
     void advance() { tokens_.advance(); }
     bool match(TokenKind kind);
     bool expect(TokenKind kind, const char* message);

     SourceLocation locCurrent() const { return current().loc; }

     // Top-level constructs
     std::unique_ptr<ast::Program> parseProgramInternal();
     void parseTopLevel(ast::Program& program);
     void parseModuleDecl(ast::Program& program);
     void parseImportDecl(ast::Program& program);
     void parseExportFunctionDecl(ast::Program& program);
     void parseFunctionDecl(ast::Program& program, bool exported);
    void parseInterfaceDecl(ast::Program& program);
    void parseImplementationDecl(ast::Program& program);
    void parseTypeDecl(ast::Program& program, const std::vector<std::string>& derivedInterfaces = {});

     // Types
     std::unique_ptr<ast::Type> parseType();
     std::unique_ptr<ast::Type> parseTypeRhs(const std::string& typeName);
     std::unique_ptr<ast::Type> parsePrimaryType();

     // Statements
     std::unique_ptr<ast::Stmt> parseStatement();
     std::unique_ptr<ast::ForInStmt> parseForInStmt();
     std::unique_ptr<ast::VariableDecl> parseVarDecl();
     std::unique_ptr<ast::ReturnStmt> parseReturnStmt();
     std::unique_ptr<ast::IfStmt> parseIfStmt();
     std::unique_ptr<ast::AssignmentStmt> parseAssignmentStmt();
     std::unique_ptr<ast::SelectStmt> parseSelectStmt();

     // Expressions (Pratt / precedence climbing)
     std::unique_ptr<ast::Expr> parseExpression();
     std::unique_ptr<ast::Expr> parseLogicalOr();
     std::unique_ptr<ast::Expr> parseLogicalAnd();
     std::unique_ptr<ast::Expr> parseEquality();
     std::unique_ptr<ast::Expr> parseComparison();
     std::unique_ptr<ast::Expr> parseRange();
     std::unique_ptr<ast::Expr> parseAdditive();
     std::unique_ptr<ast::Expr> parseMultiplicative();
     std::unique_ptr<ast::Expr> parseUnary();
     std::unique_ptr<ast::Expr> parsePostfix();
     std::unique_ptr<ast::Expr> parsePrimaryExpr();
     std::unique_ptr<ast::Expr> parseBlockExpr();
     std::unique_ptr<ast::Expr> parseIfExpr();
     std::unique_ptr<ast::Expr> parseBranchOrBlock();

     std::unique_ptr<ast::Expr> parseLiteralExpr();
     std::unique_ptr<ast::Expr> parseDoBlockExpr();

     // Pattern matching
     std::unique_ptr<ast::Expr> parseMatchExpr();
     std::unique_ptr<ast::Pattern> parsePattern();

     // Concurrency: select branches (shared by SelectStmt and SelectExpr)
     std::vector<std::unique_ptr<ast::SelectBranch>> parseSelectBranches();

     // Helpers
     void reportSyntaxError(const std::string& message,
                            std::size_t length = 1);
     // Scala-like kind: F<_> or F<_, _>. Returns true if parsed, and sets outArity (1, 2, ...).
     // If the closing token is >> (OpThen), sets outConsumedDoubleGt true so caller skips expecting '>'.
     bool parseScalaStyleKind(int& outArity, bool* outConsumedDoubleGt = nullptr);
};

 } // namespace parser
 } // namespace first

