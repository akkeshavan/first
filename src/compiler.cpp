#include "first/compiler.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "first/ast/builder.h"
#include "first/ast/validator.h"
#include "first/semantic/type_checker.h"
#include "first/semantic/semantic_checker.h"
#include "first/semantic/module_resolver.h"
#include "first/ir/ir_generator.h"
#include <fstream>
#include <sstream>
#include <iostream>

// ANTLR4 includes (will be generated)
#include "FirstLexer.h"
#include "FirstParser.h"
#include <antlr4-runtime.h>

namespace first {

Compiler::Compiler()
    : errorReporter_(std::make_unique<ErrorReporter>()) {
}

Compiler::~Compiler() = default;

bool Compiler::compile(const std::string& sourceFile) {
    errorReporter_->clear();

    // Read source file
    std::ifstream file(sourceFile);
    if (!file.is_open()) {
        errorReporter_->error(
            SourceLocation(1, 1, sourceFile),
            "Cannot open file: " + sourceFile
        );
        return false;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    return compileFromString(source, sourceFile);
}

bool Compiler::compileFromString(const std::string& source) {
    return compileFromString(source, "");
}

bool Compiler::compileFromString(const std::string& source, const std::string& virtualFile) {
    errorReporter_->clear();
    errorReporter_->setSource(virtualFile, source);

    // Create input stream
    antlr4::ANTLRInputStream input(source);
    
    // Create lexer
    FirstLexer lexer(&input);
    lexer.removeErrorListeners();
    
    // Add custom error listener
    class CustomErrorListener : public antlr4::BaseErrorListener {
    public:
        CustomErrorListener(ErrorReporter& reporter, const std::string& file)
            : reporter_(reporter), file_(file) {}
        
        void syntaxError(antlr4::Recognizer* recognizer,
                        antlr4::Token* offendingSymbol,
                        size_t line,
                        size_t charPositionInLine,
                        const std::string& msg,
                        std::exception_ptr e) override {
            SourceLocation loc(line, charPositionInLine + 1, file_);
            size_t len = 1;
            std::string tokenText;
            
            if (offendingSymbol) {
                tokenText = offendingSymbol->getText();
                if (!tokenText.empty()) {
                    len = tokenText.size();
                }
            }
            
            // Determine error code based on message content
            ErrorCode code(ErrorCodes::SYNTAX_ERROR, "syntax");
            std::string errorMsg = "Syntax error: " + msg;
            
            // Try to categorize the error
            if (msg.find("mismatched") != std::string::npos || 
                msg.find("extraneous") != std::string::npos) {
                code = ErrorCode(ErrorCodes::UNEXPECTED_TOKEN, "syntax");
                if (!tokenText.empty()) {
                    errorMsg = "Unexpected token: '" + tokenText + "'";
                }
            } else if (msg.find("missing") != std::string::npos) {
                code = ErrorCode(ErrorCodes::MISSING_TOKEN, "syntax");
            }
            
            reporter_.error(loc, errorMsg, code, len);
            
            // Add helpful notes for common errors
            if (!tokenText.empty()) {
                if (tokenText == "var") {
                    reporter_.addNote("'var' is only allowed in interaction functions");
                    reporter_.addHelp("Use 'let' for immutable bindings, or mark the function as 'interaction'");
                } else if (tokenText == "while") {
                    reporter_.addNote("'while' loops are only allowed in interaction functions");
                    reporter_.addHelp("Use recursion or mark the function as 'interaction'");
                } else if (tokenText == ">>=" || tokenText == ">>" || tokenText == "<$>" || tokenText == "<*>") {
                    reporter_.addNote("Monadic operators are only allowed in interaction functions");
                    reporter_.addHelp("Mark the function as 'interaction' to use monadic operators");
                }
            }
        }
        
    private:
        ErrorReporter& reporter_;
        std::string file_;
    };
    
    CustomErrorListener errorListener(*errorReporter_, virtualFile);
    lexer.addErrorListener(&errorListener);
    
    // Tokenize
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    // Check for lexer errors
    if (errorReporter_->hasErrors()) {
        return false;
    }
    
    // Create parser
    FirstParser parser(&tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(&errorListener);
    
    // Parse
    antlr4::tree::ParseTree* tree = parser.program();
    
    // Check for parser errors
    if (errorReporter_->hasErrors()) {
        return false;
    }
    
    // Phase 2: Build AST from parse tree
    ast::ASTBuilder astBuilder(*errorReporter_, virtualFile);
    auto programCtx = parser.program();
    ast_ = astBuilder.buildProgram(programCtx);
    
    if (!ast_) {
        errorReporter_->error(
            SourceLocation(1, 1, virtualFile),
            "Failed to build AST"
        );
        return false;
    }
    
    // Phase 2.3: Validate AST structure
    ast::ASTValidator validator(*errorReporter_);
    if (!validator.validate(ast_.get())) {
        // Validation errors already reported
        return false;
    }
    
    // Phase 3: Semantic analysis
    // 3.1: Type checking
    semantic::TypeChecker typeChecker(*errorReporter_);
    typeChecker.check(ast_.get());
    
    if (errorReporter_->hasErrors()) {
        return false;
    }
    
    // 3.2: Semantic restrictions (pure function rules)
    semantic::SemanticChecker semanticChecker(*errorReporter_);
    for (const auto& func : ast_->getFunctions()) {
        semanticChecker.checkFunction(func.get());
    }
    for (const auto& interaction : ast_->getInteractions()) {
        semanticChecker.checkInteraction(interaction.get());
    }
    
    if (errorReporter_->hasErrors()) {
        return false;
    }
    
    // 3.3: Module system (import resolution)
    semantic::ModuleResolver moduleResolver(*errorReporter_);
    if (!moduleResolver.resolveImports(ast_.get())) {
        // Import errors already reported
        return false;
    }
    
    if (errorReporter_->hasErrors()) {
        return false;
    }
    
    // Phase 5: Generate LLVM IR
    ir::IRGenerator irGenerator(*errorReporter_, virtualFile);
    if (!irGenerator.generate(ast_.get())) {
        // IR generation errors already reported
        return false;
    }
    
    // Print IR for debugging (can be removed later or made optional)
    // irGenerator.printIR();
    
    return true;
}

} // namespace first
