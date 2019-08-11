open OUnit2
include Interpreter_tests
include Parser_declaration_tests
include Parser_expression_tests
include Parser_statement_tests
include Scanner_tests

let () =
  run_test_tt_main ("Scanner tests" >::: scanner_tests) ;
  run_test_tt_main ("Parser expression tests" >::: parser_expression_tests) ;
  run_test_tt_main ("Parser statement tests" >::: parser_statement_tests) ;
  run_test_tt_main
    ("Interpreter declaration tests" >::: parser_declaration_tests) ;
  run_test_tt_main ("Interpreter tests" >::: interpreter_tests)
