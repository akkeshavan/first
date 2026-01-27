#pragma once

#include <string>
#include <cstddef>

// Forward declaration
namespace antlr4 {
    class Token;
}

namespace first {

class SourceLocation {
public:
    SourceLocation()
        : line_(1), column_(1), file_("") {}
    
    SourceLocation(size_t line, size_t column, const std::string& file = "")
        : line_(line), column_(column), file_(file) {}

    size_t getLine() const { return line_; }
    size_t getColumn() const { return column_; }
    const std::string& getFile() const { return file_; }

    void setLine(size_t line) { line_ = line; }
    void setColumn(size_t column) { column_ = column; }
    void setFile(const std::string& file) { file_ = file; }

    // Create a source location from ANTLR token
    static SourceLocation fromToken(antlr4::Token* token, const std::string& file = "");

    std::string toString() const;

private:
    size_t line_;
    size_t column_;
    std::string file_;
};

} // namespace first
