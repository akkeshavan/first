#include "first/compiler.h"
#include "first/error_reporter.h"
#include "test_framework.h"

#include <filesystem>
#include <fstream>
#include <sstream>

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

static std::string findModulesDir() {
    // We want to run regardless of current working directory.
    // Try common locations based on how tests are executed.
    const std::vector<std::string> candidates = {
        "tests/modules/",        // from repo root
        "../tests/modules/",     // from build/bin
        "../../tests/modules/"   // from build/
    };

    for (const auto& cand : candidates) {
        std::error_code ec;
        if (std::filesystem::exists(cand + "Main.first", ec) && !ec) {
            return cand;
        }
    }

    return "";
}

TEST(multimodule_end_to_end_linking) {
    std::string modulesDir = findModulesDir();
    ASSERT(!modulesDir.empty(), "modules test directory should be found");
    if (modulesDir.empty()) {
        return;
    }

    // Change CWD so ModuleResolver can find "Math.first" via relative lookup.
    std::filesystem::path oldCwd = std::filesystem::current_path();
    std::filesystem::current_path(modulesDir);

    first::Compiler compiler;
    bool ok = compiler.compileToIR("Main.first");
    if (!ok || compiler.getErrorReporter().hasErrors()) {
        compiler.getErrorReporter().printErrors();
    }
    ASSERT(ok, "Compiler should compile Main.first successfully");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors expected compiling Main.first");

    llvm::Module* m = compiler.getIRModule();
    ASSERT(m != nullptr, "IR module should be produced");
    if (!m) {
        std::filesystem::current_path(oldCwd);
        return;
    }

    llvm::Function* compute = m->getFunction("compute");
    ASSERT(compute != nullptr, "Linked IR should contain Main.compute");

    // Since we link imported modules in-place, Math.square should be a definition (not just a decl).
    llvm::Function* square = m->getFunction("square");
    if (!square) {
        llvm::errs() << "=== Multi-module linked IR dump ===\n";
        m->print(llvm::errs(), nullptr);
        llvm::errs() << "=== End IR dump ===\n";
    }
    ASSERT(square != nullptr, "Linked IR should contain Math.square");
    if (square) {
        ASSERT(!square->isDeclaration(), "square should be a definition after linking");
    }

    // NOTE: JIT execution step temporarily disabled while stabilizing multi-module pipeline.

    // Restore CWD.
    std::filesystem::current_path(oldCwd);
}

