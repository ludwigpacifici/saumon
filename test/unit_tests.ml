open OUnit2
include Scanner_tests
include Parser_tests
include Interpreter_tests

let () =
  run_test_tt_main ("Scanner tests" >::: scanner_tests) ;
  run_test_tt_main ("Parser tests" >::: parser_tests) ;
  run_test_tt_main ("Interpreter tests" >::: interpreter_tests)
