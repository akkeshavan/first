#pragma once

#include "first/ast/program.h"
#include "first/ast/declarations.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include <string>
#include <unordered_map>
#include <vector>
#include <memory>
#include <set>
#include <string_view>

namespace first {
    class Compiler;
}

namespace first {
namespace semantic {

// Module resolver - handles module imports and resolution
class ModuleResolver {
public:
    ModuleResolver(ErrorReporter& errorReporter);
    ~ModuleResolver();
    
    // Resolve imports in a program
    bool resolveImports(ast::Program* program);
    
    // Register a module (for testing and future multi-file support)
    void registerModule(const std::string& moduleName, ast::Program* program);
    
    // Get a registered module
    ast::Program* getModule(const std::string& moduleName) const;
    
    // Check for circular dependencies
    bool checkCircularDependencies(ast::Program* program);
    
    // Get exported symbols from a module
    std::vector<std::string> getExportedSymbols(const std::string& moduleName) const;
    
    // Load and parse a module file
    // Returns the AST of the loaded module, or nullptr on failure
    // Note: The returned AST is owned by an internal Compiler instance
    ast::Program* loadModule(const std::string& moduleName, const std::string& filePath);
    
    // Get function declaration from a module by name
    ast::FunctionDecl* getFunction(const std::string& moduleName, const std::string& functionName) const;
    
    // Get interaction declaration from a module by name
    ast::InteractionDecl* getInteraction(const std::string& moduleName, const std::string& interactionName) const;
    
    // Get all loaded module names (for IR generation and linking)
    // Only returns modules that were loaded via loadModule(), not just registered
    std::vector<std::string> getLoadedModuleNames() const;
    
    // Check if a module was loaded (has a compiler instance) vs just registered
    bool isModuleLoaded(const std::string& moduleName) const;

private:
    ErrorReporter& errorReporter_;
    std::unordered_map<std::string, ast::Program*> modules_;
    std::set<std::string> loadedModules_; // Track which modules were loaded (have compilers)
    std::vector<std::string> importStack_; // Track import chain for circular dependency detection

    // Storage for compiler instances to keep loaded module ASTs alive.
    // Defined here to avoid manual void* casting and lifetime hazards.
    std::vector<std::unique_ptr<Compiler>> loadedCompilers_;
    
    // Resolve a single import
    bool resolveImport(ast::ImportDecl* importDecl, const std::string& currentModule);
    
    // Check if module exists
    bool moduleExists(const std::string& moduleName);
    
    // Find module file path
    std::string findModulePath(const std::string& moduleName);
    
    // Report circular dependency error
    void reportCircularDependency(const SourceLocation& loc, const std::string& moduleName);
};

} // namespace semantic
} // namespace first
