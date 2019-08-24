let () =
  Alcotest.run "Saumon unit tests"
    [ ("Scanner tests", Scanner_tests.all)
    ; ("Parser expression tests", Parser_expression_tests.all)
    ; ("Parser statement tests", Parser_statement_tests.all)
    ; ("Parser declaration tests", Parser_declaration_tests.all)
    ; ("Interpreter expression tests", Interpreter_expression_tests.all)
    ; ("Interpreter statement tests", Interpreter_statement_tests.all) ]
