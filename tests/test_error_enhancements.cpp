#include "first/error_reporter.h"
#include "first/source_location.h"
#include "test_framework.h"

TEST(error_code_display) {
    first::ErrorReporter reporter;
    
    first::SourceLocation loc(5, 10, "test.first");
    first::ErrorCode code(first::ErrorCodes::TYPE_MISMATCH, "type");
    reporter.error(loc, "Type mismatch: Int expected, got String", code);
    
    ASSERT(reporter.hasErrors(), "Should have errors");
    const auto& errors = reporter.getErrors();
    ASSERT_EQ(errors.size(), 1, "Should have one error");
    ASSERT_EQ(errors[0].errorCode.code, "E102", "Error code should be E102");
}

TEST(error_with_notes_and_help) {
    first::ErrorReporter reporter;
    reporter.setSource("test.first", "function test() -> Int {\n    var x = 10;\n    return x;\n}");
    
    first::SourceLocation loc(2, 5, "test.first");
    first::ErrorCode code(first::ErrorCodes::MUTABLE_IN_PURE_FUNCTION, "semantic");
    reporter.error(loc, "Mutable variable 'var' not allowed in pure function", code);
    reporter.addNote("Pure functions cannot use mutable variables");
    reporter.addHelp("Use 'let' for immutable bindings, or mark the function as 'interaction'");
    
    ASSERT(reporter.hasErrors(), "Should have errors");
    const auto& errors = reporter.getErrors();
    ASSERT_EQ(errors.size(), 1, "Should have one error");
    ASSERT_EQ(errors[0].notes.size(), 1, "Should have one note");
    ASSERT_EQ(errors[0].help.size(), 1, "Should have one help message");
}

TEST(multi_line_span) {
    first::ErrorReporter reporter;
    reporter.setSource("test.first", "function test() -> Int {\n    if (x > 0 {\n        return x;\n    }\n}");
    
    first::SourceLocation start(2, 15, "test.first");
    first::SourceLocation end(4, 5, "test.first");
    first::SourceSpan span(start, end);
    first::ErrorCode code(first::ErrorCodes::MISMATCHED_BRACKET, "syntax");
    
    reporter.error(span, "Missing closing parenthesis", code);
    
    ASSERT(reporter.hasErrors(), "Should have errors");
    const auto& errors = reporter.getErrors();
    ASSERT_EQ(errors.size(), 1, "Should have one error");
    ASSERT(errors[0].span.isValid(), "Span should be valid");
    ASSERT(!errors[0].spanLines.empty(), "Should have span lines");
}

TEST(tab_expansion) {
    first::ErrorReporter reporter;
    reporter.setTabWidth(4);
    reporter.setSource("test.first", "\tlet x = 10;");  // Tab before 'let'
    
    first::SourceLocation loc(1, 2, "test.first");  // Column 2 (after tab)
    reporter.error(loc, "Test error");
    
    const auto& errors = reporter.getErrors();
    ASSERT_EQ(errors.size(), 1, "Should have one error");
    // The underline should be at visual column 4 (tab expanded), not column 1
    ASSERT_EQ(errors[0].underlineStart, 4, "Underline should account for tab expansion");
}
