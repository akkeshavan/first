#pragma once

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include "source_location.h"

namespace first {

enum class ErrorLevel {
    Error,
    Warning,
    Info
};

// Common error codes
namespace ErrorCodes {
    // Syntax errors (E001-E099)
    constexpr const char* SYNTAX_ERROR = "E001";
    constexpr const char* UNEXPECTED_TOKEN = "E002";
    constexpr const char* MISSING_TOKEN = "E003";
    constexpr const char* MISMATCHED_BRACKET = "E004";
    
    // Semantic errors (E100-E199)
    constexpr const char* UNDEFINED_VARIABLE = "E100";
    constexpr const char* REDEFINED_VARIABLE = "E101";
    constexpr const char* TYPE_MISMATCH = "E102";
    constexpr const char* MUTABLE_IN_PURE_FUNCTION = "E103";
    constexpr const char* LOOP_IN_PURE_FUNCTION = "E104";
    constexpr const char* MONADIC_OP_IN_PURE_FUNCTION = "E105";
    
    // Type errors (E200-E299)
    constexpr const char* TYPE_NOT_FOUND = "E200";
    constexpr const char* GENERIC_TYPE_MISMATCH = "E201";
    
    // Warnings (W001-W099)
    constexpr const char* UNUSED_VARIABLE = "W001";
    constexpr const char* UNREACHABLE_CODE = "W002";
}

// Error code structure: E001, E002, etc. for errors, W001, W002 for warnings
struct ErrorCode {
    std::string code;  // e.g., "E001", "W002"
    std::string category;  // e.g., "syntax", "type", "semantic"
    
    ErrorCode() : code(""), category("") {}
    ErrorCode(const std::string& c, const std::string& cat) : code(c), category(cat) {}
    
    bool empty() const { return code.empty(); }
};

// Multi-line span information
struct SourceSpan {
    SourceLocation start;
    SourceLocation end;
    
    SourceSpan() {}
    SourceSpan(const SourceLocation& s, const SourceLocation& e) : start(s), end(e) {}
    
    bool isValid() const {
        return start.getLine() > 0 && end.getLine() > 0;
    }
};

struct CompilerError {
    ErrorLevel level;
    SourceLocation location;
    std::string message;
    ErrorCode errorCode;           // Error code (e.g., "E001")
    std::vector<std::string> notes; // Additional context notes
    std::vector<std::string> help;  // Helpful suggestions
    
    // Single-line error display
    std::string sourceLine;        // Line of source code
    size_t underlineStart = 0;     // Visual column position (after tab expansion)
    size_t underlineLength = 1;    // Length of underline
    
    // Multi-line span support
    SourceSpan span;               // Optional span for multi-line errors
    std::vector<std::pair<size_t, std::string>> spanLines; // Line number -> source line pairs
    
    CompilerError(ErrorLevel lvl, const SourceLocation& loc, const std::string& msg)
        : level(lvl), location(loc), message(msg) {}
    
    CompilerError(ErrorLevel lvl, const SourceLocation& loc, const std::string& msg, const ErrorCode& code)
        : level(lvl), location(loc), message(msg), errorCode(code) {}
};

class ErrorReporter {
public:
    ErrorReporter();
    ~ErrorReporter();

    // Provide source text for nicer diagnostics (call per compile unit)
    void setSource(const std::string& file, const std::string& source);
    
    // Set tab width for tab expansion (default: 4)
    void setTabWidth(size_t width) { tabWidth_ = width; }
    size_t getTabWidth() const { return tabWidth_; }

    // Report an error (with optional error code)
    void error(const SourceLocation& location, const std::string& message);
    void error(const SourceLocation& location, const std::string& message, size_t underlineLength);
    void error(const SourceLocation& location, const std::string& message, const ErrorCode& code);
    void error(const SourceLocation& location, const std::string& message, const ErrorCode& code, size_t underlineLength);
    
    // Report an error with span (multi-line)
    void error(const SourceSpan& span, const std::string& message, const ErrorCode& code);
    
    // Report a warning (with optional error code)
    void warning(const SourceLocation& location, const std::string& message);
    void warning(const SourceLocation& location, const std::string& message, size_t underlineLength);
    void warning(const SourceLocation& location, const std::string& message, const ErrorCode& code);
    void warning(const SourceLocation& location, const std::string& message, const ErrorCode& code, size_t underlineLength);
    
    // Report info
    void info(const SourceLocation& location, const std::string& message);
    void info(const SourceLocation& location, const std::string& message, size_t underlineLength);
    
    // Add note/help to the most recent error
    void addNote(const std::string& note);
    void addHelp(const std::string& help);

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
    size_t tabWidth_ = 4;  // Default tab width

    std::unordered_map<std::string, std::vector<std::string>> sourcesByFile_;
    
    // Helper functions
    void addError(ErrorLevel level, const SourceLocation& location, const std::string& message, 
                  const ErrorCode& code, size_t underlineLength);
    void addError(ErrorLevel level, const SourceSpan& span, const std::string& message, 
                  const ErrorCode& code);
    
    // Tab expansion helper
    size_t expandTabs(const std::string& line, size_t column) const;
    std::string expandTabsToString(const std::string& line) const;
    
    // Get source lines for span
    void populateSpanLines(CompilerError& err) const;
};

} // namespace first
