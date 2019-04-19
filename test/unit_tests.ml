open OUnit2
include Scanner_tests
include Parser_tests

let () =
  run_test_tt_main ("Scanner test" >::: scanner_tests) ;
  run_test_tt_main ("Parser test" >::: parser_tests)
