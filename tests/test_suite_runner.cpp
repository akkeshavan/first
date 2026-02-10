// Data-driven test suite: parsing and typechecking only (no codegen).
// Reads test-suite/manifest.txt and runs each listed .first file through the compiler.
// Expectation: "pass" => no errors, "fail" => has errors.

#include "first/compiler.h"
#include "first/error_reporter.h"
#include "test_framework.h"
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cstring>
#include <cstdlib>
#include <iostream>

#ifndef FIRST_TEST_SUITE_PATH
#define FIRST_TEST_SUITE_PATH ""
#endif

namespace {

std::string readFile(const std::string& path) {
    std::ifstream f(path);
    if (!f.good()) return "";
    std::ostringstream oss;
    oss << f.rdbuf();
    return oss.str();
}

// Trim leading/trailing whitespace
std::string trim(const std::string& s) {
    size_t start = s.find_first_not_of(" \t\r\n");
    if (start == std::string::npos) return "";
    size_t end = s.find_last_not_of(" \t\r\n");
    return s.substr(start, end == std::string::npos ? std::string::npos : end - start + 1);
}

// Parse manifest line: "pass parser name" or "fail typecheck name". Returns false if skip.
bool parseManifestLine(const std::string& line, bool& expectPass, bool& isParser, std::string& name) {
    if (line.empty() || line[0] == '#') return false;
    std::string s = trim(line);
    if (s.empty()) return false;
    size_t a = s.find(' ');
    if (a == std::string::npos) return false;
    std::string kind = s.substr(0, a);
    s = trim(s.substr(a));
    if (kind != "pass" && kind != "fail") return false;
    expectPass = (kind == "pass");
    size_t b = s.find(' ');
    if (b == std::string::npos) return false;
    std::string phase = s.substr(0, b);
    name = trim(s.substr(b));
    if (name.empty()) return false;
    if (phase != "parser" && phase != "typecheck") return false;
    isParser = (phase == "parser");
    return true;
}

void run_test_suite() {
    std::string basePath;
    const char* env = std::getenv("FIRST_TEST_SUITE_PATH");
    if (env && env[0])
        basePath = env;
    else if (!std::string(FIRST_TEST_SUITE_PATH).empty())
        basePath = FIRST_TEST_SUITE_PATH;
    if (basePath.empty()) {
        basePath = "test-suite";
        if (readFile(basePath + "/manifest.txt").empty())
            basePath = "../test-suite";
        if (readFile(basePath + "/manifest.txt").empty())
            basePath = "../../test-suite";
    }
    if (readFile(basePath + "/manifest.txt").empty()) {
        return; // Suite not found: skip silently
    }
    std::string manifestPath = basePath + "/manifest.txt";
    std::string content = readFile(manifestPath);
    if (content.empty()) {
        // Suite not found: skip silently so out-of-tree builds don't fail
        return;
    }

    std::string libPath = basePath + "/../lib";

    // First pass: count total tests
    int total = 0;
    {
        std::istringstream issCount(content);
        std::string line;
        while (std::getline(issCount, line)) {
            bool expectPass = false, isParser = false;
            std::string name;
            if (parseManifestLine(line, expectPass, isParser, name)) total++;
        }
    }
    if (total == 0) return;
    std::cerr << "Running the test suite (" << total << " tests)..." << std::endl;

    std::istringstream iss(content);
    std::string line;
    int run = 0, passed = 0, failed = 0;
    while (std::getline(iss, line)) {
        bool expectPass = false, isParser = false;
        std::string name;
        if (!parseManifestLine(line, expectPass, isParser, name)) continue;

        run++;
        std::cerr << "Running test " << run << " of " << total << " (" << name << ")" << std::endl;

        std::string subdir = isParser ? "parser" : "typecheck";
        std::string passFail = expectPass ? "pass" : "fail";
        std::string filePath = basePath + "/" + subdir + "/" + passFail + "/" + name + ".first";
        std::string source = readFile(filePath);
        if (source.empty()) {
            std::cerr << "FAIL: test suite file not found: " << filePath << "\n";
            tests_run++;
            tests_failed++;
            failed++;
            continue;
        }

        first::Compiler compiler;
        compiler.setInstallLibPath(libPath);
        // Run type checker for typecheck tests (otherwise only parse + AST validation run)
        if (!isParser) compiler.setGenerateIR(true);
        compiler.compileFromString(source, filePath);
        bool hasErrors = compiler.getErrorReporter().hasErrors();
        tests_run++;
        if (expectPass && !hasErrors) {
            tests_passed++;
            passed++;
        } else if (!expectPass && hasErrors) {
            tests_passed++;
            passed++;
        } else {
            tests_failed++;
            failed++;
            if (expectPass) {
                std::cerr << "FAIL: " << name << " (expected pass, got errors):\n";
                compiler.getErrorReporter().printErrors();
            } else {
                std::cerr << "FAIL: " << name << " (expected fail, got no errors)\n";
            }
        }
    }
    if (run > 0) {
        std::cerr << "Test suite: " << passed << "/" << run << " passed";
        if (failed > 0) std::cerr << ", " << failed << " failed";
        std::cerr << std::endl;
    }
}

} // namespace

TEST(test_suite_parser_and_typecheck) {
    run_test_suite();
}
