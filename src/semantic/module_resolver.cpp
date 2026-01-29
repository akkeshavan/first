#include "first/semantic/module_resolver.h"
#include "first/ast/program.h"
#include "first/ast/declarations.h"
#include "first/compiler.h"
#include <fstream>
#include <filesystem>
#include <algorithm>
#include <sstream>
#include <iostream>
#include <unordered_map>
#include <set>

namespace first {
namespace semantic {

ModuleResolver::ModuleResolver(ErrorReporter& errorReporter)
    : errorReporter_(errorReporter) {
}

ModuleResolver::~ModuleResolver() {
    // All owned resources are stored in members and cleaned up automatically.
}

bool ModuleResolver::resolveImports(ast::Program* program) {
    if (!program) {
        return false;
    }
    
    // Do not clear importStack_ here: we may have been called recursively from
    // resolveImport (when resolving a loaded module's imports). Clearing would
    // break the caller's pop_back(). The top-level caller (Compiler) must call
    // clearImportStack() once before resolveImports if a fresh stack is needed.
    
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
    
    // Extract module name from ImportDecl
    std::string moduleName = importDecl->getModuleName();
    
    if (moduleName.empty()) {
        errorReporter_.error(
            importDecl->getLocation(),
            "Import declaration missing module name"
        );
        return false;
    }
    
    // Check for circular dependencies
    if (std::find(importStack_.begin(), importStack_.end(), moduleName) != importStack_.end()) {
        reportCircularDependency(importDecl->getLocation(), moduleName);
        return false;
    }
    
    // Add to import stack
    importStack_.push_back(moduleName);
    
    // Check if module is already loaded
    if (modules_.find(moduleName) != modules_.end()) {
        // Module already loaded, just validate import
        importStack_.pop_back();
        return true;
    }
    
    // Check if module exists and get file path
    std::string modulePath = findModulePath(moduleName);
    if (modulePath.empty()) {
        // Module not found in file system
        // Check if it's already registered (for testing)
        if (modules_.find(moduleName) == modules_.end()) {
            errorReporter_.error(
                importDecl->getLocation(),
                "Module not found: " + moduleName
            );
            importStack_.pop_back();
            return false;
        }
        // Module is already registered
        importStack_.pop_back();
        return true;
    }
    
    // Load and parse the module file
    ast::Program* loadedModule = loadModule(moduleName, modulePath);
    if (!loadedModule) {
        errorReporter_.error(
            importDecl->getLocation(),
            "Failed to load module: " + moduleName
        );
        importStack_.pop_back();
        return false;
    }
    
    // Recursively resolve imports in the loaded module
    if (!resolveImports(loadedModule)) {
        importStack_.pop_back();
        return false;
    }
    
    // Register the module
    registerModule(moduleName, loadedModule);
    
    // Remove from import stack
    importStack_.pop_back();
    
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
    // Check if module is already registered
    if (modules_.find(moduleName) != modules_.end()) {
        return true;
    }
    
    return !findModulePath(moduleName).empty();
}

std::string ModuleResolver::findModulePath(const std::string& moduleName) {
    // Remove quotes if present (module names come from string literals)
    std::string cleanName = moduleName;
    if (cleanName.size() >= 2 && cleanName[0] == '"' && cleanName.back() == '"') {
        cleanName = cleanName.substr(1, cleanName.size() - 2);
    }
    
    // Check file system for module file
    // Convert module name to file path:
    // - "MyModule" -> "./MyModule.first" or "./MyModule/module.first"
    // - "path/to/Module" -> "./path/to/Module.first"
    
    // Try common patterns:
    std::vector<std::string> pathsToTry;
    
    // Pattern 1: Direct file (MyModule.first)
    pathsToTry.push_back(cleanName + ".first");
    
    // Pattern 2: Directory with module.first (MyModule/module.first)
    pathsToTry.push_back(cleanName + "/module.first");
    
    // Pattern 3: Replace dots with slashes (com.example.Module -> com/example/Module.first)
    std::string pathName = cleanName;
    std::replace(pathName.begin(), pathName.end(), '.', '/');
    pathsToTry.push_back(pathName + ".first");
    pathsToTry.push_back(pathName + "/module.first");
    
    // Check if any of these files exist
    for (const auto& path : pathsToTry) {
        std::ifstream file(path);
        if (file.good()) {
            file.close();
            return path;
        }
    }
    
    // Module not found
    return "";
}

void ModuleResolver::registerModule(const std::string& moduleName, ast::Program* program) {
    modules_[moduleName] = program;
}

ast::Program* ModuleResolver::getModule(const std::string& moduleName) const {
    auto it = modules_.find(moduleName);
    if (it != modules_.end()) {
        return it->second;
    }
    return nullptr;
}

std::vector<std::string> ModuleResolver::getExportedSymbols(const std::string& moduleName) const {
    std::vector<std::string> symbols;
    
    ast::Program* program = getModule(moduleName);
    if (!program) {
        return symbols;
    }
    
    // Collect exported function names
    for (const auto& func : program->getFunctions()) {
        if (func->isExported()) {
            symbols.push_back(func->getName());
        }
    }
    
    // Collect exported interaction names
    for (const auto& interaction : program->getInteractions()) {
        if (interaction->isExported()) {
            symbols.push_back(interaction->getName());
        }
    }
    
    // Collect exported type names
    for (const auto& typeDecl : program->getTypeDecls()) {
        if (typeDecl->isExported()) {
            // TypeDecl doesn't have a name yet - this would need to be added
            // For now, we'll skip type exports
        }
    }
    
    return symbols;
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

ast::Program* ModuleResolver::loadModule(const std::string& moduleName, const std::string& filePath) {
    // Read the module file
    std::ifstream file(filePath);
    if (!file.is_open()) {
        errorReporter_.error(
            SourceLocation(1, 1, filePath),
            "Cannot open module file: " + filePath
        );
        return nullptr;
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();
    
    // Create a separate compiler instance for this module
    auto moduleCompiler = std::make_unique<Compiler>();
    
    // Compile the module (this will parse and build AST)
    // Use compileFromStringNoModules to avoid recursive module loading
    // We'll handle imports at the top level
    // Note: We pass resolveModules=false to skip module resolution and IR generation
    // for loaded modules to avoid recursion and complexity
    if (!moduleCompiler->compileFromStringNoModules(source, filePath, false)) {
        // Compilation errors occurred in the module compiler
        // The module compiler has its own ErrorReporter, but we want to report
        // at least a summary error to the main ErrorReporter
        errorReporter_.error(
            SourceLocation(1, 1, filePath),
            "Failed to compile module: " + moduleName
        );
        return nullptr;
    }
    
    // Get the AST
    ast::Program* moduleAST = moduleCompiler->getAST();
    if (!moduleAST) {
        errorReporter_.error(
            SourceLocation(1, 1, filePath),
            "Failed to build AST for module: " + moduleName
        );
        return nullptr;
    }
    
    // Store the compiler instance to keep the AST alive.
    loadedCompilers_.push_back(std::move(moduleCompiler));
    
    // Mark this module as loaded (has a compiler instance)
    loadedModules_.insert(moduleName);
    
    return moduleAST;
}

ast::FunctionDecl* ModuleResolver::getFunction(const std::string& moduleName, const std::string& functionName) const {
    ast::Program* program = getModule(moduleName);
    if (!program) {
        return nullptr;
    }
    
    for (const auto& func : program->getFunctions()) {
        if (func->getName() == functionName) {
            return func.get();
        }
    }
    
    return nullptr;
}

ast::InteractionDecl* ModuleResolver::getInteraction(const std::string& moduleName, const std::string& interactionName) const {
    ast::Program* program = getModule(moduleName);
    if (!program) {
        return nullptr;
    }
    
    for (const auto& interaction : program->getInteractions()) {
        if (interaction->getName() == interactionName) {
            return interaction.get();
        }
    }
    
    return nullptr;
}

std::vector<std::string> ModuleResolver::getLoadedModuleNames() const {
    std::vector<std::string> names;
    names.reserve(loadedModules_.size());
    for (const auto& moduleName : loadedModules_) {
        names.push_back(moduleName);
    }
    return names;
}

bool ModuleResolver::isModuleLoaded(const std::string& moduleName) const {
    return loadedModules_.find(moduleName) != loadedModules_.end();
}

} // namespace semantic
} // namespace first
