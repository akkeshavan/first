#include "test_framework.h"
#include <cstring>

// Test framework variables
int tests_run = 0;
int tests_passed = 0;
int tests_failed = 0;

void run_all_tests();
static bool run_single_test(const char* name);

int main(int argc, char** argv) {
    std::cout << "Running First Compiler Tests...\n\n";
    
    if (argc >= 2 && argv[1] && std::strlen(argv[1]) > 0) {
        if (run_single_test(argv[1])) {
            std::cout << "\n";
            std::cout << "Tests run: " << tests_run << "\n";
            std::cout << "Tests passed: " << tests_passed << "\n";
            std::cout << "Tests failed: " << tests_failed << "\n";
            return tests_failed == 0 ? 0 : 1;
        }
        std::cerr << "Unknown test: " << argv[1] << "\n";
        return 2;
    }
    
    run_all_tests();
    
    std::cout << "\n";
    std::cout << "Tests run: " << tests_run << "\n";
    std::cout << "Tests passed: " << tests_passed << "\n";
    std::cout << "Tests failed: " << tests_failed << "\n";
    
    return tests_failed == 0 ? 0 : 1;
}

// Declare all test functions
extern void test_error_reporter_basic();
extern void test_error_reporter_warnings();
extern void test_error_reporter_clear();
extern void test_source_location_basic();
extern void test_source_location_default();
extern void test_source_location_to_string();
extern void test_lexer_simple_program();
extern void test_lexer_integer_literals();
extern void test_parser_simple_program();
extern void test_parser_variables();
extern void test_parser_types();
extern void test_parser_expressions();
extern void test_parser_control_flow();
extern void test_parser_pattern_matching();
extern void test_parser_interfaces();
extern void test_parser_test_files();
extern void test_error_code_display();
extern void test_error_with_notes_and_help();
extern void test_multi_line_span();
extern void test_tab_expansion();
extern void test_ast_base_node();
extern void test_ast_literal_types();
extern void test_ast_binary_expr();
extern void test_ast_variable_expr();
extern void test_ast_function_call_expr();
extern void test_ast_variable_decl();
extern void test_ast_primitive_type();
extern void test_ast_array_type();
extern void test_ast_program_node();
extern void test_ast_parameter();
extern void test_ast_function_decl();
extern void test_ast_function_signature();
extern void test_ast_interaction_decl();
extern void test_ast_function_with_generics();
extern void test_ast_integration_simple_program();
extern void test_ast_integration_multiple_statements();
extern void test_ast_integration_with_errors();
extern void test_ast_validator_valid_program();
extern void test_ast_validator_binary_expr_missing_operand();
extern void test_ast_validator_unary_expr_missing_operand();
extern void test_ast_validator_array_type_missing_element();
extern void test_ast_validator_function_call();
extern void test_symbol_table_basic_insert_lookup();
extern void test_symbol_table_scope_nesting();
extern void test_symbol_table_function_overloading();
extern void test_symbol_table_duplicate_variable();
extern void test_symbol_table_contains_local();
extern void test_symbol_table_type_symbol();
extern void test_symbol_table_generic_param();
extern void test_type_checker_literal_int();
extern void test_type_checker_literal_float();
extern void test_type_checker_binary_add_int();
extern void test_type_checker_binary_add_float();
extern void test_type_checker_binary_comparison();
extern void test_type_checker_unary_neg();
extern void test_type_checker_types_equal();
extern void test_type_checker_is_assignable();
extern void test_type_checker_array_type_equality();
extern void test_type_checker_array_type_inequality();
extern void test_type_checker_array_assignability();
extern void test_type_checker_nested_array_types();
extern void test_type_checker_generic_type_equality();
extern void test_type_checker_parameterized_type_equality();
extern void test_type_checker_parameterized_type_multiple_args();
extern void test_type_checker_function_type_equality();
extern void test_type_checker_function_vs_interaction_type();
extern void test_type_checker_function_call_argument_matching();
extern void test_type_checker_function_call_with_promotion();
extern void test_type_checker_higher_order_function_type();
extern void test_type_checker_generic_params_must_appear_in_parameter_types();
extern void test_type_checker_generic_params_in_parameter_types_allowed();
extern void test_semantic_checker_mutable_var_in_pure_function();
extern void test_semantic_checker_mutable_var_in_interaction();
extern void test_semantic_checker_io_in_pure_function();
extern void test_semantic_checker_io_in_interaction();
extern void test_semantic_checker_immutable_var_in_pure_function();
extern void test_simple_function();
extern void test_arithmetic();
extern void test_variables();
extern void test_int_type();
extern void test_float_type();
extern void test_bool_type();
extern void test_unit_type();
extern void test_string_type();
extern void test_int_arithmetic();
extern void test_float_arithmetic();
extern void test_mixed_arithmetic();
extern void test_arithmetic_operations();
extern void test_comparison_operations();
extern void test_logical_operations();
extern void test_unary_operations();
extern void test_operator_precedence();
extern void test_nested_expressions();
extern void test_function_calls();
extern void test_complex_expression();
extern void test_if_statement();
extern void test_if_without_else();
extern void test_nested_if();
extern void test_short_circuit_and();
extern void test_short_circuit_or();
extern void test_function_with_parameters();
extern void test_function_call();
extern void test_recursive_function();
extern void test_function_with_local_variables();
extern void test_void_function();
extern void test_function_default_return();
extern void test_multiple_functions();
extern void test_interaction_declaration();
extern void test_mutable_variable();
extern void test_assignment();
extern void test_assignment_in_loop();
extern void test_interaction_with_parameters();
extern void test_multiple_assignments();
extern void test_array_literal();
extern void test_array_indexing();
extern void test_array_type();
extern void test_record_literal();
extern void test_field_access();
extern void test_record_type();
extern void test_record_pattern_match();
extern void test_constructor_expr();
extern void test_adt_type();
extern void test_match_expr();
extern void test_lambda_expr_ast_building();
extern void test_closure_ir_generation();
extern void test_closure_no_captures();
extern void test_closure_multiple_parameters();
extern void test_closure_invocation();
extern void test_generic_type_parsing();
extern void test_generic_type_ast_building();
extern void test_generic_type_parameter_ast_building();
extern void test_generic_type_ir_generation();
extern void test_generic_type_parameter_ir_generation();
extern void test_multiple_type_arguments();
extern void test_nested_generic_types();
extern void test_type_substitution();
extern void test_type_substitution_parameterized();
extern void test_type_substitution_array();
extern void test_monomorphize_function_name();
extern void test_multimodule_end_to_end_linking();
extern void test_integration_hello_compiles_to_ir();
extern void test_integration_stdlib_compiles_to_ir();
extern void test_integration_syntax_error_fails();
extern void test_integration_hello_from_string_to_ir();
extern void test_compliance_hello_full_pipeline();
extern void test_compliance_simple_first_compiles();
extern void test_compliance_semantic_restriction_pure_io();
extern void test_compliance_error_messages_undefined_function();
extern void test_test_suite_parser_and_typecheck();

// Run a single test by name (e.g. "multimodule_end_to_end_linking").
// Returns true if the test was found and run, false if unknown name.
static bool run_single_test(const char* name) {
    if (!name || !*name) return false;
    if (std::strcmp(name, "multimodule_end_to_end_linking") == 0) {
        std::cout << "Running single test: multimodule_end_to_end_linking\n";
        test_multimodule_end_to_end_linking();
        return true;
    }
    if (std::strcmp(name, "integration_hello_compiles_to_ir") == 0) {
        test_integration_hello_compiles_to_ir();
        return true;
    }
    if (std::strcmp(name, "integration_stdlib_compiles_to_ir") == 0) {
        test_integration_stdlib_compiles_to_ir();
        return true;
    }
    if (std::strcmp(name, "integration_syntax_error_fails") == 0) {
        test_integration_syntax_error_fails();
        return true;
    }
    if (std::strcmp(name, "integration_hello_from_string_to_ir") == 0) {
        test_integration_hello_from_string_to_ir();
        return true;
    }
    if (std::strcmp(name, "compliance_hello_full_pipeline") == 0) {
        test_compliance_hello_full_pipeline();
        return true;
    }
    if (std::strcmp(name, "compliance_simple_first_compiles") == 0) {
        test_compliance_simple_first_compiles();
        return true;
    }
    if (std::strcmp(name, "compliance_semantic_restriction_pure_io") == 0) {
        test_compliance_semantic_restriction_pure_io();
        return true;
    }
    if (std::strcmp(name, "compliance_error_messages_undefined_function") == 0) {
        test_compliance_error_messages_undefined_function();
        return true;
    }
    if (std::strcmp(name, "test_suite_parser_and_typecheck") == 0) {
        test_test_suite_parser_and_typecheck();
        return true;
    }
    return false;
}

void run_all_tests() {
    std::cout << "Testing ErrorReporter...\n";
    test_error_reporter_basic();
    test_error_reporter_warnings();
    test_error_reporter_clear();
    test_error_code_display();
    test_error_with_notes_and_help();
    test_multi_line_span();
    test_tab_expansion();
    
    std::cout << "Testing SourceLocation...\n";
    test_source_location_basic();
    test_source_location_default();
    test_source_location_to_string();
    
    std::cout << "Testing Lexer...\n";
    test_lexer_simple_program();
    test_lexer_integer_literals();
    
    std::cout << "Testing Parser...\n";
    test_parser_simple_program();
    test_parser_variables();
    test_parser_types();
    test_parser_expressions();
    test_parser_control_flow();
    test_parser_pattern_matching();
    test_parser_interfaces();
    test_parser_test_files();
    
    std::cout << "Testing AST...\n";
    test_ast_base_node();
    test_ast_literal_types();
    test_ast_binary_expr();
    test_ast_variable_expr();
    test_ast_function_call_expr();
    test_ast_variable_decl();
    test_ast_primitive_type();
    test_ast_array_type();
    test_ast_program_node();
    test_ast_parameter();
    test_ast_function_decl();
    test_ast_function_signature();
    test_ast_interaction_decl();
    test_ast_function_with_generics();
    
    std::cout << "Testing AST Integration...\n";
    test_ast_integration_simple_program();
    test_ast_integration_multiple_statements();
    test_ast_integration_with_errors();
    
    std::cout << "Testing AST Validation...\n";
    test_ast_validator_valid_program();
    test_ast_validator_binary_expr_missing_operand();
    test_ast_validator_unary_expr_missing_operand();
    test_ast_validator_array_type_missing_element();
    test_ast_validator_function_call();
    
    std::cout << "Testing Symbol Table...\n";
    test_symbol_table_basic_insert_lookup();
    test_symbol_table_scope_nesting();
    test_symbol_table_function_overloading();
    test_symbol_table_duplicate_variable();
    test_symbol_table_contains_local();
    test_symbol_table_type_symbol();
    test_symbol_table_generic_param();
    
    std::cout << "Testing Type Checker...\n";
    test_type_checker_literal_int();
    test_type_checker_literal_float();
    test_type_checker_binary_add_int();
    test_type_checker_binary_add_float();
    test_type_checker_binary_comparison();
    test_type_checker_unary_neg();
    test_type_checker_types_equal();
    test_type_checker_is_assignable();
    
    std::cout << "Testing Composite Types...\n";
    test_type_checker_array_type_equality();
    test_type_checker_array_type_inequality();
    test_type_checker_array_assignability();
    test_type_checker_nested_array_types();
    test_type_checker_generic_type_equality();
    test_type_checker_parameterized_type_equality();
    test_type_checker_parameterized_type_multiple_args();
    
    std::cout << "Testing Function Types...\n";
    test_type_checker_function_type_equality();
    test_type_checker_function_vs_interaction_type();
    test_type_checker_function_call_argument_matching();
    test_type_checker_function_call_with_promotion();
    test_type_checker_higher_order_function_type();
    test_type_checker_generic_params_must_appear_in_parameter_types();
    test_type_checker_generic_params_in_parameter_types_allowed();
    
    std::cout << "Testing Semantic Restrictions...\n";
    test_semantic_checker_mutable_var_in_pure_function();
    test_semantic_checker_mutable_var_in_interaction();
    test_semantic_checker_io_in_pure_function();
    test_semantic_checker_io_in_interaction();
    test_semantic_checker_immutable_var_in_pure_function();
    
    std::cout << "Testing IR Generator...\n";
    test_simple_function();
    test_arithmetic();
    test_variables();
    
    std::cout << "Testing Primitive Type IR Generation...\n";
    test_int_type();
    test_float_type();
    test_bool_type();
    test_unit_type();
    test_string_type();
    test_int_arithmetic();
    test_float_arithmetic();
    test_mixed_arithmetic();
    
    std::cout << "Testing Expression IR Generation...\n";
    test_arithmetic_operations();
    test_comparison_operations();
    test_logical_operations();
    test_unary_operations();
    test_operator_precedence();
    test_nested_expressions();
    test_function_calls();
    test_complex_expression();
    
    std::cout << "Testing Control Flow IR Generation...\n";
    test_if_statement();
    test_if_without_else();
    test_nested_if();
    test_short_circuit_and();
    test_short_circuit_or();
    
    std::cout << "Testing Function IR Generation...\n";
    test_function_with_parameters();
    test_function_call();
    test_recursive_function();
    test_function_with_local_variables();
    test_void_function();
    test_function_default_return();
    test_multiple_functions();
    
    std::cout << "Testing Interaction IR Generation...\n";
    test_interaction_declaration();
    test_mutable_variable();
    test_assignment();
    test_assignment_in_loop();
    test_interaction_with_parameters();
    test_multiple_assignments();
    
    std::cout << "Testing Array IR Generation...\n";
    test_array_literal();
    test_array_indexing();
    test_array_type();
    
    std::cout << "Testing Record IR Generation...\n";
    test_record_literal();
    test_field_access();
    test_record_type();
    test_record_pattern_match();
    
    std::cout << "Testing ADT IR Generation...\n";
    test_constructor_expr();
    test_adt_type();
    test_match_expr();
    test_lambda_expr_ast_building();
    test_closure_ir_generation();
    test_closure_no_captures();
    test_closure_multiple_parameters();
    test_closure_invocation();
    
    std::cout << "Testing Generic Code Generation...\n";
    test_generic_type_parsing();
    test_generic_type_ast_building();
    test_generic_type_parameter_ast_building();
    test_generic_type_ir_generation();
    test_generic_type_parameter_ir_generation();
    test_multiple_type_arguments();
    test_nested_generic_types();
    test_type_substitution();
    test_type_substitution_parameterized();
    test_type_substitution_array();
    test_monomorphize_function_name();

    std::cout << "Testing Multi-Module Compilation...\n";
    test_multimodule_end_to_end_linking();

    std::cout << "Testing Integration (Phase 8.2)...\n";
    test_integration_hello_compiles_to_ir();
    test_integration_stdlib_compiles_to_ir();
    test_integration_syntax_error_fails();
    test_integration_hello_from_string_to_ir();

    std::cout << "Testing Compliance (Phase 8.3)...\n";
    test_compliance_hello_full_pipeline();
    test_compliance_simple_first_compiles();
    test_compliance_semantic_restriction_pure_io();
    test_compliance_error_messages_undefined_function();

    std::cout << "Testing test-suite (parser + typecheck)...\n";
    test_test_suite_parser_and_typecheck();
}
