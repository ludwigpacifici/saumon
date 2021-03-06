let () =
  Alcotest.run "Saumon unit tests"
    [ ("Scanner tests", Scanner_tests.all)
    ; ("Parser expression tests", Parser_expression_tests.all)
    ; ("Parser statement tests", Parser_statement_tests.all)
    ; ("Parser declaration tests", Parser_declaration_tests.all)
    ; ("Parser block tests", Parser_block_tests.all)
    ; ("Parser conditional tests", Parser_conditional_tests.all)
    ; ("Parser while tests", Parser_while_tests.all)
    ; ("Parser for tests", Parser_for_tests.all)
    ; ("Interpreter expression tests", Interpreter_expression_tests.all)
    ; ("Interpreter statement tests", Interpreter_statement_tests.all)
    ; ("Interpreter block tests", Interpreter_block_tests.all)
    ; ("Interpreter conditional tests", Interpreter_conditional_tests.all)
    ; ("Interpreter while tests", Interpreter_while_tests.all)
    ; ("Environment tests", Environment_tests.all) ]
