#include "first/semantic/symbol_table.h"
#include <algorithm>

namespace first {
namespace semantic {

bool SymbolScope::supportsOverloading(SymbolKind kind) {
    return kind == SymbolKind::Function || kind == SymbolKind::Interaction;
}

bool SymbolScope::insert(std::unique_ptr<Symbol> symbol) {
    if (!symbol) {
        return false;
    }
    
    const std::string& name = symbol->getName();
    SymbolKind kind = symbol->getKind();
    
    // Check if symbol already exists
    auto it = symbols_.find(name);
    if (it != symbols_.end()) {
        // If it supports overloading, add to the list
        if (supportsOverloading(kind)) {
            it->second.push_back(std::move(symbol));
            return true;
        } else {
            // Symbol already exists and doesn't support overloading
            return false;
        }
    }
    
    // New symbol - create entry
    symbols_[name] = std::vector<std::unique_ptr<Symbol>>();
    symbols_[name].push_back(std::move(symbol));
    return true;
}

Symbol* SymbolScope::lookupLocal(const std::string& name) const {
    auto it = symbols_.find(name);
    if (it != symbols_.end() && !it->second.empty()) {
        return it->second[0].get(); // Return first match
    }
    return nullptr;
}

Symbol* SymbolScope::lookup(const std::string& name) const {
    // Check current scope first
    Symbol* local = lookupLocal(name);
    if (local) {
        return local;
    }
    
    // Check parent scope
    if (parent_) {
        return parent_->lookup(name);
    }
    
    return nullptr;
}

std::vector<Symbol*> SymbolScope::lookupAll(const std::string& name) const {
    std::vector<Symbol*> results;
    
    // Check current scope
    auto it = symbols_.find(name);
    if (it != symbols_.end()) {
        for (const auto& symbol : it->second) {
            results.push_back(symbol.get());
        }
    }
    
    // Check parent scope
    if (parent_) {
        auto parentResults = parent_->lookupAll(name);
        results.insert(results.end(), parentResults.begin(), parentResults.end());
    }
    
    return results;
}

bool SymbolScope::containsLocal(const std::string& name) const {
    auto it = symbols_.find(name);
    return it != symbols_.end() && !it->second.empty();
}

// SymbolTable implementation

SymbolTable::SymbolTable() {
    rootScope_ = std::make_unique<SymbolScope>(nullptr);
    currentScope_ = rootScope_.get();
}

SymbolScope* SymbolTable::enterScope() {
    currentScope_ = new SymbolScope(currentScope_);
    return currentScope_;
}

SymbolScope* SymbolTable::exitScope() {
    if (currentScope_ && currentScope_->getParent()) {
        SymbolScope* parent = currentScope_->getParent();
        delete currentScope_;
        currentScope_ = parent;
        return currentScope_;
    }
    return currentScope_;
}

bool SymbolTable::insert(std::unique_ptr<Symbol> symbol) {
    if (!currentScope_) {
        return false;
    }
    return currentScope_->insert(std::move(symbol));
}

Symbol* SymbolTable::lookup(const std::string& name) const {
    if (!currentScope_) {
        return nullptr;
    }
    return currentScope_->lookup(name);
}

std::vector<Symbol*> SymbolTable::lookupAll(const std::string& name) const {
    if (!currentScope_) {
        return std::vector<Symbol*>();
    }
    return currentScope_->lookupAll(name);
}

bool SymbolTable::containsLocal(const std::string& name) const {
    if (!currentScope_) {
        return false;
    }
    return currentScope_->containsLocal(name);
}

} // namespace semantic
} // namespace first
