#pragma once

#include <string>
#include <vector>
#include <memory>

namespace first {

class ErrorReporter;
class SourceLocation;

class Compiler {
public:
    Compiler();
    ~Compiler();

    // Compile a source file
    bool compile(const std::string& sourceFile);
    
    // Compile from source string (for testing)
    bool compileFromString(const std::string& source);

    // Get error reporter
    ErrorReporter& getErrorReporter() { return *errorReporter_; }

private:
    std::unique_ptr<ErrorReporter> errorReporter_;
};

} // namespace first
