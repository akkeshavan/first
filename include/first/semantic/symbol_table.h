#pragma once

#include "first/ast/node.h"
#include "first/ast/types.h"
#include "first/ast/declarations.h"
#include "first/source_location.h"
#include <string>
#include <unordered_map>
#include <vector>
#include <memory>

namespace first {
namespace semantic {

// Symbol kinds
enum class SymbolKind {
    Variable,
    Function,
    Interaction,
    Type,
    GenericParam,
    Module
};

// Symbol entry in the symbol table
class Symbol {
public:
    Symbol(SymbolKind kind, const std::string& name, const SourceLocation& location)
        : kind_(kind), name_(name), location_(location) {}
    
    virtual ~Symbol() = default;
    
    SymbolKind getKind() const { return kind_; }
    const std::string& getName() const { return name_; }
    const SourceLocation& getLocation() const { return location_; }
    
    // Type-specific accessors (return nullptr if not applicable)
    virtual ast::Type* getType() const { return nullptr; }
    virtual ast::FunctionDecl* getFunction() const { return nullptr; }
    virtual ast::InteractionDecl* getInteraction() const { return nullptr; }
    virtual ast::TypeDecl* getTypeDecl() const { return nullptr; }

private:
    SymbolKind kind_;
    std::string name_;
    SourceLocation location_;
};

// Variable symbol
class VariableSymbol : public Symbol {
public:
    VariableSymbol(const std::string& name, 
                   const SourceLocation& location,
                   std::unique_ptr<ast::Type> type,
                   bool isMutable = false)
        : Symbol(SymbolKind::Variable, name, location),
          type_(std::move(type)), isMutable_(isMutable) {}
    
    ast::Type* getType() const override { return type_.get(); }
    bool isMutable() const { return isMutable_; }

private:
    std::unique_ptr<ast::Type> type_;
    bool isMutable_;
};

// Function symbol (supports overloading)
class FunctionSymbol : public Symbol {
public:
    FunctionSymbol(const std::string& name,
                   const SourceLocation& location,
                   ast::FunctionDecl* function)
        : Symbol(SymbolKind::Function, name, location),
          function_(function) {}
    
    ast::FunctionDecl* getFunction() const override { return function_; }

private:
    ast::FunctionDecl* function_; // Non-owning pointer
};

// Interaction symbol
class InteractionSymbol : public Symbol {
public:
    InteractionSymbol(const std::string& name,
                      const SourceLocation& location,
                      ast::InteractionDecl* interaction)
        : Symbol(SymbolKind::Interaction, name, location),
          interaction_(interaction) {}
    
    ast::InteractionDecl* getInteraction() const override { return interaction_; }

private:
    ast::InteractionDecl* interaction_; // Non-owning pointer
};

// Type symbol
class TypeSymbol : public Symbol {
public:
    TypeSymbol(const std::string& name,
               const SourceLocation& location,
               ast::TypeDecl* typeDecl)
        : Symbol(SymbolKind::Type, name, location),
          typeDecl_(typeDecl) {}
    
    ast::TypeDecl* getTypeDecl() const override { return typeDecl_; }

private:
    ast::TypeDecl* typeDecl_; // Non-owning pointer
};

// Generic parameter symbol
class GenericParamSymbol : public Symbol {
public:
    GenericParamSymbol(const std::string& name,
                       const SourceLocation& location)
        : Symbol(SymbolKind::GenericParam, name, location) {}
};

// Symbol scope (represents a lexical scope)
class SymbolScope {
public:
    SymbolScope(SymbolScope* parent = nullptr)
        : parent_(parent) {}
    
    // Insert a symbol (returns false if already exists and not overloadable)
    bool insert(std::unique_ptr<Symbol> symbol);
    
    // Look up a symbol in this scope only (no parent lookup)
    Symbol* lookupLocal(const std::string& name) const;
    
    // Look up a symbol in this scope and parent scopes
    Symbol* lookup(const std::string& name) const;
    
    // Look up all symbols with the given name (for function overloading)
    std::vector<Symbol*> lookupAll(const std::string& name) const;
    
    // Get parent scope
    SymbolScope* getParent() const { return parent_; }
    
    // Check if a symbol exists locally
    bool containsLocal(const std::string& name) const;

private:
    SymbolScope* parent_;
    std::unordered_map<std::string, std::vector<std::unique_ptr<Symbol>>> symbols_;
    
    // Check if symbol kind supports overloading
    static bool supportsOverloading(SymbolKind kind);
};

// Symbol table manager (manages scopes)
class SymbolTable {
public:
    SymbolTable();
    
    // Enter a new scope (returns the new scope)
    SymbolScope* enterScope();
    
    // Exit current scope (returns parent scope)
    SymbolScope* exitScope();
    
    // Get current scope
    SymbolScope* getCurrentScope() const { return currentScope_; }
    
    // Get root scope
    SymbolScope* getRootScope() const { return rootScope_.get(); }
    
    // Insert a symbol into current scope
    bool insert(std::unique_ptr<Symbol> symbol);
    
    // Look up a symbol starting from current scope
    Symbol* lookup(const std::string& name) const;
    
    // Look up all symbols with the given name (for overloading)
    std::vector<Symbol*> lookupAll(const std::string& name) const;
    
    // Check if symbol exists in current scope
    bool containsLocal(const std::string& name) const;

private:
    std::unique_ptr<SymbolScope> rootScope_;
    SymbolScope* currentScope_;
};

} // namespace semantic
} // namespace first
