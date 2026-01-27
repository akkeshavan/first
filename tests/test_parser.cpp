#include "first/compiler.h"
#include "first/error_reporter.h"
#include "test_framework.h"
#include <fstream>
#include <sstream>
#include <vector>

// Helper function to read test file
std::string readTestFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        return "";
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

TEST(parser_simple_program) {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction main() -> Unit {
            print("Hello, World!");
        }
    )";
    
    bool result = compiler.compileFromString(source);
    
    // Should parse successfully (no syntax errors)
    ASSERT(!compiler.getErrorReporter().hasErrors(), 
           "Simple program should parse without errors");
}

TEST(parser_variables) {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let x: Int = 10;
            let y = 20;
            return x + y;
        }
    )";
    
    bool result = compiler.compileFromString(source);
    ASSERT(!compiler.getErrorReporter().hasErrors(),
           "Variable declarations should parse correctly");
}

TEST(parser_types) {
    first::Compiler compiler;
    
    std::string source = R"(
        type Person = {
            name: String,
            age: Int
        };
        
        type Option<T> = Some(T) | None;
    )";
    
    bool result = compiler.compileFromString(source);
    ASSERT(!compiler.getErrorReporter().hasErrors(),
           "Type declarations should parse correctly");
}

TEST(parser_expressions) {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let a = 10;
            let b = 20;
            return (a + b) * 2 / 3;
        }
    )";
    
    bool result = compiler.compileFromString(source);
    ASSERT(!compiler.getErrorReporter().hasErrors(),
           "Expressions should parse correctly");
}

TEST(parser_control_flow) {
    first::Compiler compiler;
    
    std::string source = R"(
        function factorial(n: Int) -> Int {
            if (n <= 1) {
                return 1;
            } else {
                return n * factorial(n - 1);
            }
        }
    )";
    
    bool result = compiler.compileFromString(source);
    ASSERT(!compiler.getErrorReporter().hasErrors(),
           "Control flow should parse correctly");
}

TEST(parser_pattern_matching) {
    first::Compiler compiler;
    
    std::string source = R"(
        function matchOption(opt: Option<Int>) -> Int {
            match opt {
                Some(value) => value
                None => 0
            }
        }
    )";
    
    bool result = compiler.compileFromString(source);
    ASSERT(!compiler.getErrorReporter().hasErrors(),
           "Pattern matching should parse correctly");
}

TEST(parser_interfaces) {
    first::Compiler compiler;
    
    std::string source = R"(
        interface Show<T> {
            show: function(T) -> String;
        };
        
        implementation Show<Int> {
            show = function(x: Int) -> String {
                return x.toString();
            };
        };
    )";
    
    bool result = compiler.compileFromString(source);
    ASSERT(!compiler.getErrorReporter().hasErrors(),
           "Interfaces and implementations should parse correctly");
}

TEST(parser_test_files) {
    first::Compiler compiler;
    
    // Test parsing actual test files
    // Use paths relative to source root (works when running from build directory)
    // Try to find source directory by looking for tests/parser directory
    std::string basePath = "";
    std::vector<std::string> possiblePaths = {
        "../tests/parser/",  // From build/bin/
        "../../tests/parser/",  // From build/
        "tests/parser/"  // From source root
    };
    
    for (const auto& path : possiblePaths) {
        std::ifstream testFile(path + "test_simple.first");
        if (testFile.is_open()) {
            basePath = path;
            testFile.close();
            break;
        }
    }
    
    if (basePath.empty()) {
        // Fallback: try to use absolute path based on current working directory
        std::ifstream testFile("tests/parser/test_simple.first");
        if (testFile.is_open()) {
            basePath = "tests/parser/";
            testFile.close();
        }
    }
    
    std::vector<std::string> testFiles = {
        "test_simple.first",
        "test_variables.first",
        "test_types.first",
        "test_expressions.first",
        "test_control_flow.first",
        "test_functions.first"
    };
    
    int successCount = 0;
    for (const auto& file : testFiles) {
        std::string fullPath = basePath + file;
        compiler.getErrorReporter().clear();
        bool result = compiler.compile(fullPath);
        if (!compiler.getErrorReporter().hasErrors()) {
            successCount++;
        } else {
            std::cerr << "Failed to parse: " << fullPath << "\n";
            compiler.getErrorReporter().printErrors();
        }
    }
    
    ASSERT(successCount == testFiles.size(),
           "All test files should parse successfully");
}
