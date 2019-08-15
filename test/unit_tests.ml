let () =
  Alcotest.run "Saumon unit tests"
    [ ("Scanner tests", Scanner_tests.all)
    ; ("Interpreter tests", Interpreter_tests.all)
    ; ("Parser expression tests", Parser_expression_tests.all)
    ; ("Parser statement tests", Parser_statement_tests.all)
    ; ("Parser declaration tests", Parser_declaration_tests.all) ]
