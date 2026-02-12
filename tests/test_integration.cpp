// Phase 8.2: Integration tests - end-to-end compilation

#include "first/compiler.h"
#include "first/error_reporter.h"
#include "test_framework.h"
#include <llvm/IR/Module.h>
#include <fstream>
#include <cstdlib>

static const char* kHelloSource = R"(
interaction main() -> Unit {
    print("Hello, World!");
}
)";

static const char* kStdlibSource = R"(
interaction main() -> Unit {
    print("hi");
    let x = sin(0.0);
    let n = strLength("abc");
}
)";

static const char* kSyntaxErrorSource = R"(
function broken(
)";

TEST(integration_hello_compiles_to_ir) {
    const char* tmpPath = std::getenv("TMPDIR");
    if (!tmpPath) tmpPath = "/tmp";
    std::string path = std::string(tmpPath) + "/first_test_hello.first";
    std::ofstream f(path);
    ASSERT(f.good(), "Temp file should be writable");
    f << kHelloSource;
    f.close();
    first::Compiler compiler;
    bool ok = compiler.compileToIR(path);
    if (!ok || compiler.getErrorReporter().hasErrors()) {
        compiler.getErrorReporter().printErrors();
    }
    ASSERT(ok, "Hello should compile to IR");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors compiling to IR");
    llvm::Module* m = compiler.getIRModule();
    ASSERT(m != nullptr, "IR module should be produced");
    ASSERT(m->getFunction("main") != nullptr, "IR should contain main");
    std::remove(path.c_str());
}

TEST(integration_stdlib_compiles_to_ir) {
    const char* tmpPath = std::getenv("TMPDIR");
    if (!tmpPath) tmpPath = "/tmp";
    std::string path = std::string(tmpPath) + "/first_test_stdlib.first";
    std::ofstream f(path);
    if (!f.good()) return;
    f << kStdlibSource;
    f.close();
    first::Compiler compiler;
    bool ok = compiler.compileToIR(path);
    ASSERT(ok, "Stdlib should compile to IR");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors compiling to IR");
    llvm::Module* m = compiler.getIRModule();
    ASSERT(m != nullptr, "IR module should be produced");
    std::remove(path.c_str());
}

TEST(integration_syntax_error_fails) {
    first::Compiler compiler;
    bool ok = compiler.compileFromString(kSyntaxErrorSource, "broken.first");
    ASSERT(!ok || compiler.getErrorReporter().hasErrors(),
           "Syntax error should be reported (compile fails or has errors)");
}

TEST(integration_hello_from_string_to_ir) {
    const char* tmpPath = std::getenv("TMPDIR");
    if (!tmpPath) tmpPath = "/tmp";
    std::string path = std::string(tmpPath) + "/first_test_virtual.first";
    std::ofstream f(path);
    if (!f.good()) return;
    f << kHelloSource;
    f.close();
    first::Compiler compiler;
    bool ok = compiler.compileToIR(path);
    ASSERT(ok, "compileToIR should succeed");
    ASSERT(compiler.getIRModule() != nullptr, "IR module should exist");
    std::remove(path.c_str());
}
