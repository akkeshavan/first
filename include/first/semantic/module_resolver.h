#pragma once

#include "first/ast/program.h"
#include "first/ast/declarations.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include <string>
#include <unordered_map>
#include <vector>
#include <memory>

namespace first {
namespace semantic {

// Module resolver - handles module imports and resolution
class ModuleResolver {
public:
    ModuleResolver(ErrorReporter& errorReporter)
        : errorReporter_(errorReporter) {}
    
    // Resolve imports in a program
    bool resolveImports(ast::Program* program);
    
    // Register a module (for testing and future multi-file support)
    void registerModule(const std::string& moduleName, ast::Program* program);
    
    // Check for circular dependencies
    bool checkCircularDependencies(ast::Program* program);

private:
    ErrorReporter& errorReporter_;
    std::unordered_map<std::string, ast::Program*> modules_;
    std::vector<std::string> importStack_; // Track import chain for circular dependency detection
    
    // Resolve a single import
    bool resolveImport(ast::ImportDecl* importDecl, const std::string& currentModule);
    
    // Check if module exists
    bool moduleExists(const std::string& moduleName);
    
    // Report circular dependency error
    void reportCircularDependency(const SourceLocation& loc, const std::string& moduleName);
};

} // namespace semantic
} // namespace first
