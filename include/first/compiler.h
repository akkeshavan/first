#pragma once

#include <string>
#include <vector>
#include <memory>

namespace first {

class ErrorReporter;
class SourceLocation;

namespace ast {
    class Program;
}

class Compiler {
public:
    Compiler();
    ~Compiler();

    // Compile a source file
    bool compile(const std::string& sourceFile);
    
    // Compile from source string (for testing)
    bool compileFromString(const std::string& source);
    
    // Compile from source string with a virtual filename (for better diagnostics)
    bool compileFromString(const std::string& source, const std::string& virtualFile);

    // Get error reporter
    ErrorReporter& getErrorReporter() { return *errorReporter_; }
    
    // Get the AST (nullptr if compilation failed or AST not built yet)
    ast::Program* getAST() const { return ast_.get(); }

private:
    std::unique_ptr<ErrorReporter> errorReporter_;
    std::unique_ptr<ast::Program> ast_;
};

} // namespace first
