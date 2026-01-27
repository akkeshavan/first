#include "first/error_reporter.h"
#include <iostream>
#include <sstream>
#include <algorithm>
#include <iomanip>

namespace first {

static std::vector<std::string> splitLinesPreserveEmpty(const std::string& src) {
    std::vector<std::string> lines;
    std::string current;
    current.reserve(128);

    for (char ch : src) {
        if (ch == '\n') {
            if (!current.empty() && current.back() == '\r') {
                current.pop_back();
            }
            lines.push_back(current);
            current.clear();
        } else {
            current.push_back(ch);
        }
    }

    // last line (even if empty)
    if (!current.empty() && current.back() == '\r') {
        current.pop_back();
    }
    lines.push_back(current);
    return lines;
}

static std::string levelToString(ErrorLevel level) {
    switch (level) {
        case ErrorLevel::Error: return "error";
        case ErrorLevel::Warning: return "warning";
        case ErrorLevel::Info: return "info";
    }
    return "error";
}

ErrorReporter::ErrorReporter()
    : errorCount_(0), warningCount_(0), tabWidth_(4) {
}

ErrorReporter::~ErrorReporter() = default;

void ErrorReporter::setSource(const std::string& file, const std::string& source) {
    sourcesByFile_[file] = splitLinesPreserveEmpty(source);
}

// Tab expansion helper: calculate visual column position
size_t ErrorReporter::expandTabs(const std::string& line, size_t column) const {
    size_t visualCol = 0;
    for (size_t i = 0; i < line.size() && i < column; ++i) {
        if (line[i] == '\t') {
            visualCol = ((visualCol / tabWidth_) + 1) * tabWidth_;
        } else {
            visualCol++;
        }
    }
    return visualCol;
}

// Expand tabs to spaces for display
std::string ErrorReporter::expandTabsToString(const std::string& line) const {
    std::string result;
    result.reserve(line.size() * 2); // Reserve space for expansion
    
    for (char ch : line) {
        if (ch == '\t') {
            size_t spaces = tabWidth_ - (result.size() % tabWidth_);
            result.append(spaces, ' ');
        } else {
            result.push_back(ch);
        }
    }
    return result;
}

void ErrorReporter::populateSpanLines(CompilerError& err) const {
    if (!err.span.isValid()) return;
    
    auto it = sourcesByFile_.find(err.span.start.getFile());
    if (it == sourcesByFile_.end()) return;
    
    const auto& lines = it->second;
    size_t startLine = err.span.start.getLine() > 0 ? err.span.start.getLine() - 1 : 0;
    size_t endLine = err.span.end.getLine() > 0 ? err.span.end.getLine() - 1 : 0;
    
    if (startLine >= lines.size() || endLine >= lines.size()) return;
    if (startLine > endLine) return;
    
    for (size_t i = startLine; i <= endLine && i < lines.size(); ++i) {
        err.spanLines.push_back({i + 1, lines[i]});
    }
}

void ErrorReporter::error(const SourceLocation& location, const std::string& message) {
    addError(ErrorLevel::Error, location, message, ErrorCode(), 1);
    errorCount_++;
}

void ErrorReporter::error(const SourceLocation& location, const std::string& message, size_t underlineLength) {
    addError(ErrorLevel::Error, location, message, ErrorCode(), underlineLength);
    errorCount_++;
}

void ErrorReporter::error(const SourceLocation& location, const std::string& message, const ErrorCode& code) {
    addError(ErrorLevel::Error, location, message, code, 1);
    errorCount_++;
}

void ErrorReporter::error(const SourceLocation& location, const std::string& message, const ErrorCode& code, size_t underlineLength) {
    addError(ErrorLevel::Error, location, message, code, underlineLength);
    errorCount_++;
}

void ErrorReporter::error(const SourceSpan& span, const std::string& message, const ErrorCode& code) {
    addError(ErrorLevel::Error, span, message, code);
    errorCount_++;
}

void ErrorReporter::warning(const SourceLocation& location, const std::string& message) {
    addError(ErrorLevel::Warning, location, message, ErrorCode(), 1);
    warningCount_++;
}

void ErrorReporter::warning(const SourceLocation& location, const std::string& message, size_t underlineLength) {
    addError(ErrorLevel::Warning, location, message, ErrorCode(), underlineLength);
    warningCount_++;
}

void ErrorReporter::warning(const SourceLocation& location, const std::string& message, const ErrorCode& code) {
    addError(ErrorLevel::Warning, location, message, code, 1);
    warningCount_++;
}

void ErrorReporter::warning(const SourceLocation& location, const std::string& message, const ErrorCode& code, size_t underlineLength) {
    addError(ErrorLevel::Warning, location, message, code, underlineLength);
    warningCount_++;
}

void ErrorReporter::info(const SourceLocation& location, const std::string& message) {
    addError(ErrorLevel::Info, location, message, ErrorCode(), 1);
}

void ErrorReporter::info(const SourceLocation& location, const std::string& message, size_t underlineLength) {
    addError(ErrorLevel::Info, location, message, ErrorCode(), underlineLength);
}

void ErrorReporter::addNote(const std::string& note) {
    if (!errors_.empty()) {
        errors_.back().notes.push_back(note);
    }
}

void ErrorReporter::addHelp(const std::string& help) {
    if (!errors_.empty()) {
        errors_.back().help.push_back(help);
    }
}

void ErrorReporter::addError(ErrorLevel level, const SourceLocation& location, const std::string& message, 
                             const ErrorCode& code, size_t underlineLength) {
    CompilerError err(level, location, message, code);
    err.underlineLength = underlineLength == 0 ? 1 : underlineLength;

    auto it = sourcesByFile_.find(location.getFile());
    if (it != sourcesByFile_.end()) {
        const auto& lines = it->second;
        const size_t lineIdx = (location.getLine() > 0) ? (location.getLine() - 1) : 0;
        if (lineIdx < lines.size()) {
            err.sourceLine = lines[lineIdx];
            const size_t col1 = location.getColumn() > 0 ? location.getColumn() : 1;
            // Use tab-expanded position for underline
            err.underlineStart = expandTabs(err.sourceLine, col1 - 1);
        }
    }

    errors_.push_back(std::move(err));
}

void ErrorReporter::addError(ErrorLevel level, const SourceSpan& span, const std::string& message, 
                             const ErrorCode& code) {
    CompilerError err(level, span.start, message, code);
    err.span = span;
    populateSpanLines(err);
    
    // Set primary location display
    auto it = sourcesByFile_.find(span.start.getFile());
    if (it != sourcesByFile_.end()) {
        const auto& lines = it->second;
        const size_t lineIdx = (span.start.getLine() > 0) ? (span.start.getLine() - 1) : 0;
        if (lineIdx < lines.size()) {
            err.sourceLine = lines[lineIdx];
            const size_t col1 = span.start.getColumn() > 0 ? span.start.getColumn() : 1;
            err.underlineStart = expandTabs(err.sourceLine, col1 - 1);
            err.underlineLength = 1; // Will be shown across multiple lines
        }
    }
    
    errors_.push_back(std::move(err));
}

void ErrorReporter::printErrors() const {
    for (const auto& err : errors_) {
        // Error code and level
        std::cerr << levelToString(err.level);
        if (!err.errorCode.empty()) {
            std::cerr << "[" << err.errorCode.code << "]";
        }

        // File location
        if (!err.location.getFile().empty()) {
            std::cerr << " [" << err.location.getFile() << "]";
        }

        // Line and column
        std::cerr << ":" << err.location.getLine() 
                  << ":" << err.location.getColumn()
                  << ": " << err.message << "\n";

        // Multi-line span display
        if (err.span.isValid() && !err.spanLines.empty()) {
            for (const auto& linePair : err.spanLines) {
                size_t lineNum = linePair.first;
                const std::string& line = linePair.second;
                std::string expandedLine = expandTabsToString(line);
                
                std::cerr << "  " << std::setw(4) << lineNum << " | " << expandedLine << "\n";
                
                // Show underline for this line
                if (lineNum == err.span.start.getLine()) {
                    // Start of span
                    size_t startCol = expandTabs(line, err.span.start.getColumn() - 1);
                    std::cerr << "      | " << std::string(startCol, ' ') << "^";
                    if (err.span.start.getLine() == err.span.end.getLine()) {
                        // Same line - show length
                        size_t endCol = expandTabs(line, err.span.end.getColumn() - 1);
                        if (endCol > startCol + 1) {
                            std::cerr << std::string(endCol - startCol - 1, '~');
                        }
                    } else {
                        // Multi-line - continue to end of line
                        if (expandedLine.size() > startCol + 1) {
                            std::cerr << std::string(expandedLine.size() - startCol - 1, '~');
                        }
                    }
                    std::cerr << "\n";
                } else if (lineNum == err.span.end.getLine()) {
                    // End of span
                    size_t endCol = expandTabs(line, err.span.end.getColumn() - 1);
                    std::cerr << "      | " << std::string(endCol, ' ') << "^";
                    if (endCol > 0) {
                        std::cerr << std::string(expandedLine.size() - endCol, '~');
                    }
                    std::cerr << "\n";
                } else if (lineNum > err.span.start.getLine() && lineNum < err.span.end.getLine()) {
                    // Middle lines of span
                    std::cerr << "      | " << std::string(expandedLine.size(), '~') << "\n";
                }
            }
        } else if (!err.sourceLine.empty()) {
            // Single-line display with tab expansion
            std::string expandedLine = expandTabsToString(err.sourceLine);
            std::cerr << "  " << std::setw(4) << err.location.getLine() << " | " << expandedLine << "\n";
            std::cerr << "      | " << std::string(err.underlineStart, ' ')
                      << "^" << std::string(err.underlineLength > 1 ? err.underlineLength - 1 : 0, '~')
                      << "\n";
        }

        // Notes
        for (const auto& note : err.notes) {
            std::cerr << "  note: " << note << "\n";
        }

        // Help text
        for (const auto& help : err.help) {
            std::cerr << "  help: " << help << "\n";
        }
        
        // Empty line between errors for readability
        std::cerr << "\n";
    }
}

void ErrorReporter::clear() {
    errors_.clear();
    errorCount_ = 0;
    warningCount_ = 0;
    sourcesByFile_.clear();
}

} // namespace first
