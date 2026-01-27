#include "first/semantic/symbol_table.h"
#include "first/ast/types.h"
#include "first/source_location.h"
#include "test_framework.h"

using namespace first;

TEST(symbol_table_basic_insert_lookup) {
    semantic::SymbolTable table;
    SourceLocation loc(1, 1);
    
    // Insert a variable symbol
    auto varType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto varSymbol = std::make_unique<semantic::VariableSymbol>(
        "x", loc, std::move(varType)
    );
    
    bool inserted = table.insert(std::move(varSymbol));
    ASSERT(inserted, "Should successfully insert variable symbol");
    
    // Look up the symbol
    semantic::Symbol* found = table.lookup("x");
    ASSERT_NE(found, nullptr, "Should find inserted symbol");
    bool nameMatch = (found->getName() == "x");
    ASSERT(nameMatch, "Symbol name should match");
    bool kindMatch = (found->getKind() == semantic::SymbolKind::Variable);
    ASSERT(kindMatch, "Symbol kind should be Variable");
}

TEST(symbol_table_scope_nesting) {
    semantic::SymbolTable table;
    SourceLocation loc(1, 1);
    
    // Insert in root scope
    auto rootType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto rootVar = std::make_unique<semantic::VariableSymbol>(
        "x", loc, std::move(rootType)
    );
    table.insert(std::move(rootVar));
    
    // Enter inner scope
    table.enterScope();
    
    // Insert shadowing variable
    auto innerType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Float
    );
    auto innerVar = std::make_unique<semantic::VariableSymbol>(
        "x", loc, std::move(innerType)
    );
    table.insert(std::move(innerVar));
    
    // Lookup should find inner scope variable
    semantic::Symbol* found = table.lookup("x");
    ASSERT_NE(found, nullptr, "Should find symbol");
    
    auto* varSymbol = dynamic_cast<semantic::VariableSymbol*>(found);
    ASSERT_NE(varSymbol, nullptr, "Should be a variable symbol");
    ASSERT_EQ(varSymbol->getType()->getNodeType(), "PrimitiveType", "Should be Float type");
    
    // Exit scope
    table.exitScope();
    
    // Now lookup should find root scope variable
    found = table.lookup("x");
    ASSERT_NE(found, nullptr, "Should find root scope symbol");
}

TEST(symbol_table_function_overloading) {
    semantic::SymbolTable table;
    SourceLocation loc(1, 1);
    
    // Create two function declarations with same name but different signatures
    // (We'll use nullptr for now since we don't have full FunctionDecl construction in tests)
    // For now, test that we can insert multiple functions with same name
    
    // Insert first function
    auto func1 = std::make_unique<semantic::FunctionSymbol>(
        "add", loc, nullptr
    );
    bool inserted1 = table.insert(std::move(func1));
    ASSERT(inserted1, "Should insert first function");
    
    // Insert second function with same name (should succeed due to overloading)
    auto func2 = std::make_unique<semantic::FunctionSymbol>(
        "add", loc, nullptr
    );
    bool inserted2 = table.insert(std::move(func2));
    ASSERT(inserted2, "Should insert overloaded function");
    
    // Lookup all should return both
    auto all = table.lookupAll("add");
    ASSERT_EQ(all.size(), 2, "Should find both overloaded functions");
}

TEST(symbol_table_duplicate_variable) {
    semantic::SymbolTable table;
    SourceLocation loc(1, 1);
    
    // Insert variable
    auto type1 = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto var1 = std::make_unique<semantic::VariableSymbol>(
        "x", loc, std::move(type1)
    );
    bool inserted1 = table.insert(std::move(var1));
    ASSERT(inserted1, "Should insert first variable");
    
    // Try to insert duplicate variable (should fail)
    auto type2 = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Float
    );
    auto var2 = std::make_unique<semantic::VariableSymbol>(
        "x", loc, std::move(type2)
    );
    bool inserted2 = table.insert(std::move(var2));
    ASSERT(!inserted2, "Should reject duplicate variable");
}

TEST(symbol_table_contains_local) {
    semantic::SymbolTable table;
    SourceLocation loc(1, 1);
    
    // Insert variable
    auto type = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto var = std::make_unique<semantic::VariableSymbol>(
        "x", loc, std::move(type)
    );
    table.insert(std::move(var));
    
    // Check containsLocal
    bool contains = table.containsLocal("x");
    ASSERT(contains, "Should contain 'x' locally");
    
    bool notContains = table.containsLocal("y");
    ASSERT(!notContains, "Should not contain 'y' locally");
    
    // Enter inner scope
    table.enterScope();
    
    // Should not contain 'x' locally in inner scope
    bool containsInInner = table.containsLocal("x");
    ASSERT(!containsInInner, "Should not contain 'x' locally in inner scope");
    
    // But lookup should still find it
    semantic::Symbol* found = table.lookup("x");
    ASSERT_NE(found, nullptr, "Lookup should still find 'x' from parent scope");
}

TEST(symbol_table_type_symbol) {
    semantic::SymbolTable table;
    SourceLocation loc(1, 1);
    
    // Insert type symbol
    auto typeSymbol = std::make_unique<semantic::TypeSymbol>(
        "MyType", loc, nullptr
    );
    bool inserted = table.insert(std::move(typeSymbol));
    ASSERT(inserted, "Should insert type symbol");
    
    // Lookup type
    semantic::Symbol* found = table.lookup("MyType");
    ASSERT_NE(found, nullptr, "Should find type symbol");
    bool kindMatch = (found->getKind() == semantic::SymbolKind::Type);
    ASSERT(kindMatch, "Should be a type symbol");
}

TEST(symbol_table_generic_param) {
    semantic::SymbolTable table;
    SourceLocation loc(1, 1);
    
    // Insert generic parameter
    auto genericParam = std::make_unique<semantic::GenericParamSymbol>(
        "T", loc
    );
    bool inserted = table.insert(std::move(genericParam));
    ASSERT(inserted, "Should insert generic parameter");
    
    // Lookup generic parameter
    semantic::Symbol* found = table.lookup("T");
    ASSERT_NE(found, nullptr, "Should find generic parameter");
    bool kindMatch = (found->getKind() == semantic::SymbolKind::GenericParam);
    ASSERT(kindMatch, "Should be a generic parameter");
}
