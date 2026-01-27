#pragma once

#include <string>
#include <vector>
#include <memory>
#include "source_location.h"

namespace first {

enum class ErrorLevel {
    Error,
    Warning,
    Info
};

struct CompilerError {
    ErrorLevel level;
    SourceLocation location;
    std::string message;
    
    CompilerError(ErrorLevel lvl, const SourceLocation& loc, const std::string& msg)
        : level(lvl), location(loc), message(msg) {}
};

class ErrorReporter {
public:
    ErrorReporter();
    ~ErrorReporter();

    // Report an error
    void error(const SourceLocation& location, const std::string& message);
    
    // Report a warning
    void warning(const SourceLocation& location, const std::string& message);
    
    // Report info
    void info(const SourceLocation& location, const std::string& message);

    // Check if there are any errors
    bool hasErrors() const { return errorCount_ > 0; }
    
    // Get error count
    size_t getErrorCount() const { return errorCount_; }
    
    // Get warning count
    size_t getWarningCount() const { return warningCount_; }

    // Get all errors
    const std::vector<CompilerError>& getErrors() const { return errors_; }

    // Print all errors to stderr
    void printErrors() const;

    // Clear all errors
    void clear();

private:
    std::vector<CompilerError> errors_;
    size_t errorCount_;
    size_t warningCount_;
    
    void addError(ErrorLevel level, const SourceLocation& location, const std::string& message);
};

} // namespace first
