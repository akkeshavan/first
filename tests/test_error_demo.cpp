#include "first/error_reporter.h"
#include "first/source_location.h"
#include <iostream>

int main() {
    first::ErrorReporter reporter;
    
    // Test 1: Error with code, notes, and help
    std::string source = "function test() -> Int {\n    var x = 10;\n    return x;\n}";
    reporter.setSource("demo.first", source);
    
    first::SourceLocation loc(2, 5, "demo.first");
    first::ErrorCode code(first::ErrorCodes::MUTABLE_IN_PURE_FUNCTION, "semantic");
    reporter.error(loc, "Mutable variable 'var' not allowed in pure function", code, 3);
    reporter.addNote("Pure functions cannot use mutable variables");
    reporter.addHelp("Use 'let' for immutable bindings, or mark the function as 'interaction'");
    
    std::cout << "=== Test 1: Error with code, notes, and help ===\n";
    reporter.printErrors();
    reporter.clear();
    
    // Test 2: Multi-line span
    std::string source2 = "function test() -> Int {\n    if (x > 0 {\n        return x;\n    }\n}";
    reporter.setSource("demo2.first", source2);
    
    first::SourceLocation start(2, 15, "demo2.first");
    first::SourceLocation end(4, 5, "demo2.first");
    first::SourceSpan span(start, end);
    first::ErrorCode code2(first::ErrorCodes::MISMATCHED_BRACKET, "syntax");
    reporter.error(span, "Missing closing parenthesis", code2);
    reporter.addNote("Opening parenthesis at line 2, column 15");
    reporter.addHelp("Add ')' before the opening brace");
    
    std::cout << "\n=== Test 2: Multi-line span ===\n";
    reporter.printErrors();
    reporter.clear();
    
    // Test 3: Tab expansion
    std::string source3 = "\tlet x = 10;\n\treturn x;";
    reporter.setSource("demo3.first", source3);
    reporter.setTabWidth(4);
    
    first::SourceLocation loc3(1, 2, "demo3.first"); // Column 2 (after tab)
    first::ErrorCode code3(first::ErrorCodes::SYNTAX_ERROR, "syntax");
    reporter.error(loc3, "Test error with tab", code3, 1);
    
    std::cout << "\n=== Test 3: Tab expansion (tab width = 4) ===\n";
    reporter.printErrors();
    reporter.clear();
    
    // Test 4: Multiple errors
    std::string source4 = "function test() -> Int {\n    let x = 10\n    let y = 20\n    return x + y;\n}";
    reporter.setSource("demo4.first", source4);
    
    first::ErrorCode code4(first::ErrorCodes::MISSING_TOKEN, "syntax");
    reporter.error(first::SourceLocation(2, 5, "demo4.first"), 
                   "Missing semicolon", code4, 1);
    reporter.error(first::SourceLocation(3, 5, "demo4.first"), 
                   "Missing semicolon", code4, 1);
    
    std::cout << "\n=== Test 4: Multiple errors ===\n";
    reporter.printErrors();
    
    return 0;
}
