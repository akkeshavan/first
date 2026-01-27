#include "first/source_location.h"
#include <antlr4-runtime.h>
#include <sstream>

namespace first {

std::string SourceLocation::toString() const {
    std::ostringstream oss;
    if (!file_.empty()) {
        oss << file_ << ":";
    }
    oss << line_ << ":" << column_;
    return oss.str();
}

SourceLocation SourceLocation::fromToken(antlr4::Token* token, const std::string& file) {
    if (token) {
        return SourceLocation(
            static_cast<size_t>(token->getLine()),
            static_cast<size_t>(token->getCharPositionInLine() + 1),
            file
        );
    }
    return SourceLocation(1, 1, file);
}

} // namespace first
