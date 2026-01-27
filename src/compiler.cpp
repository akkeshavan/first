#include "first/compiler.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
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

    return compileFromString(source);
}

bool Compiler::compileFromString(const std::string& source) {
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
            reporter_.error(loc, "Syntax error: " + msg);
        }
        
    private:
        ErrorReporter& reporter_;
        std::string file_;
    };
    
    CustomErrorListener errorListener(*errorReporter_, "");
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
    
    // TODO: Phase 2 - Build AST from parse tree
    // TODO: Phase 3 - Semantic analysis
    // TODO: Phase 5 - Generate LLVM IR
    
    return true;
}

} // namespace first
