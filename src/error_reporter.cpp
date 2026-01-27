#include "first/error_reporter.h"
#include <iostream>
#include <iomanip>

namespace first {

ErrorReporter::ErrorReporter()
    : errorCount_(0), warningCount_(0) {
}

ErrorReporter::~ErrorReporter() = default;

void ErrorReporter::error(const SourceLocation& location, const std::string& message) {
    addError(ErrorLevel::Error, location, message);
    errorCount_++;
}

void ErrorReporter::warning(const SourceLocation& location, const std::string& message) {
    addError(ErrorLevel::Warning, location, message);
    warningCount_++;
}

void ErrorReporter::info(const SourceLocation& location, const std::string& message) {
    addError(ErrorLevel::Info, location, message);
}

void ErrorReporter::addError(ErrorLevel level, const SourceLocation& location, const std::string& message) {
    errors_.emplace_back(level, location, message);
}

void ErrorReporter::printErrors() const {
    for (const auto& err : errors_) {
        std::cerr << (err.level == ErrorLevel::Error ? "error" : 
                     err.level == ErrorLevel::Warning ? "warning" : "info");
        
        if (!err.location.getFile().empty()) {
            std::cerr << " [" << err.location.getFile() << "]";
        }
        
        std::cerr << ":" << err.location.getLine() 
                  << ":" << err.location.getColumn()
                  << ": " << err.message << "\n";
    }
}

void ErrorReporter::clear() {
    errors_.clear();
    errorCount_ = 0;
    warningCount_ = 0;
}

} // namespace first
