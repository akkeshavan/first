#include "first/semantic/module_resolver.h"
#include "first/ast/program.h"
#include "first/ast/declarations.h"

namespace first {
namespace semantic {

bool ModuleResolver::resolveImports(ast::Program* program) {
    if (!program) {
        return false;
    }
    
    // Clear import stack for this resolution pass
    importStack_.clear();
    
    // Resolve all imports
    bool allResolved = true;
    for (const auto& import : program->getImports()) {
        if (!resolveImport(import.get(), "main")) {
            allResolved = false;
        }
    }
    
    return allResolved;
}

bool ModuleResolver::resolveImport(ast::ImportDecl* importDecl, const std::string& currentModule) {
    if (!importDecl) {
        return false;
    }
    
    // For now, we just validate that imports are well-formed
    // Full module resolution will require file system access and module loading
    // TODO: Extract module name from ImportDecl and resolve it
    
    // Check for circular dependencies
    std::string moduleName = "unknown"; // TODO: Extract from ImportDecl
    if (std::find(importStack_.begin(), importStack_.end(), moduleName) != importStack_.end()) {
        reportCircularDependency(importDecl->getLocation(), moduleName);
        return false;
    }
    
    // Check if module exists
    if (!moduleExists(moduleName)) {
        errorReporter_.error(
            importDecl->getLocation(),
            "Module not found: " + moduleName
        );
        return false;
    }
    
    return true;
}

bool ModuleResolver::checkCircularDependencies(ast::Program* program) {
    if (!program) {
        return false;
    }
    
    importStack_.clear();
    return resolveImports(program);
}

bool ModuleResolver::moduleExists(const std::string& moduleName) {
    // For now, always return true - in a full implementation, this would check
    // the file system or module registry
    // TODO: Implement actual module lookup
    return true;
}

void ModuleResolver::registerModule(const std::string& moduleName, ast::Program* program) {
    modules_[moduleName] = program;
}

void ModuleResolver::reportCircularDependency(const SourceLocation& loc, const std::string& moduleName) {
    std::string chain = "";
    for (const auto& mod : importStack_) {
        if (!chain.empty()) chain += " -> ";
        chain += mod;
    }
    chain += " -> " + moduleName;
    
    errorReporter_.error(
        loc,
        "Circular dependency detected: " + chain
    );
}

} // namespace semantic
} // namespace first
