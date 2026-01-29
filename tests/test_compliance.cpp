// Phase 8.3: Language compliance tests - full pipeline on First source

#include "first/compiler.h"
#include "first/error_reporter.h"
#include "test_framework.h"
#include <llvm/IR/Module.h>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cstdio>

static std::string readFileOrEmpty(const std::string& path) {
    std::ifstream f(path);
    if (!f.good()) return "";
    std::ostringstream oss;
    oss << f.rdbuf();
    return oss.str();
}

TEST(compliance_hello_full_pipeline) {
    const char* src = R"(
interaction main() -> Unit {
    print("Hello");
}
)";
    const char* tmpPath = std::getenv("TMPDIR");
    if (!tmpPath) tmpPath = "/tmp";
    std::string path = std::string(tmpPath) + "/first_compliance_hello.first";
    std::ofstream f(path);
    if (!f.good()) return;
    f << src;
    f.close();
    first::Compiler compiler;
    bool ok = compiler.compileToIR(path);
    ASSERT(ok && !compiler.getErrorReporter().hasErrors(), "Hello should generate IR");
    llvm::Module* m = compiler.getIRModule();
    ASSERT(m != nullptr, "Module should exist");
    ASSERT(m->getFunction("main") != nullptr, "main should exist in IR");
    std::remove(path.c_str());
}

TEST(compliance_simple_first_compiles) {
    // Try to load tests/parser/test_simple.first if we can find it
    std::string content;
    const char* candidates[] = {
        "tests/parser/test_simple.first",
        "../tests/parser/test_simple.first",
        "../../tests/parser/test_simple.first",
    };
    for (const char* p : candidates) {
        content = readFileOrEmpty(p);
        if (!content.empty()) break;
    }
    if (content.empty()) {
        // Skip if file not found (e.g. cwd not repo root)
        return;
    }
    const char* tmpDir = std::getenv("TMPDIR");
    if (!tmpDir) tmpDir = "/tmp";
    std::string path = std::string(tmpDir) + "/first_compliance_simple.first";
    std::ofstream f(path);
    if (!f.good()) return;
    f << content;
    f.close();
    first::Compiler compiler;
    bool ok = compiler.compileToIR(path);
    std::remove(path.c_str());
    ASSERT(ok && !compiler.getErrorReporter().hasErrors(), "test_simple.first should generate IR");
}

TEST(compliance_semantic_restriction_pure_io) {
    // Pure function must not call I/O (semantic analysis runs only in compileToIR)
    const char* src = R"(
function bad() -> Unit {
    print("not allowed");
}
)";
    const char* tmpDir = std::getenv("TMPDIR");
    if (!tmpDir) tmpDir = "/tmp";
    std::string path = std::string(tmpDir) + "/first_compliance_pure_io.first";
    std::ofstream f(path);
    if (!f.good()) return;
    f << src;
    f.close();
    first::Compiler compiler;
    compiler.compileToIR(path);
    std::remove(path.c_str());
    ASSERT(compiler.getErrorReporter().hasErrors(),
           "Calling print in pure function should report error");
}

TEST(compliance_error_messages_undefined_function) {
    const char* src = R"(
interaction main() -> Unit {
    nonexistent();
}
)";
    const char* tmpDir = std::getenv("TMPDIR");
    if (!tmpDir) tmpDir = "/tmp";
    std::string path = std::string(tmpDir) + "/first_compliance_undef.first";
    std::ofstream f(path);
    if (!f.good()) return;
    f << src;
    f.close();
    first::Compiler compiler;
    bool ok = compiler.compileToIR(path);
    std::remove(path.c_str());
    ASSERT(!ok || compiler.getErrorReporter().hasErrors(),
           "Undefined function should be reported");
}
