open Alcotest
open Saumon

let ast_literal = testable Ast.pp_literal Ast.equal_literal
let ast_program = testable Ast.Program.pp Ast.Program.equal
let scanner_error = testable Scanner.pp_error Scanner.equal_error
let token = testable Token.pp Token.equal
let value = testable Value.pp Value.equal
let value_error = testable Interpreter.pp_error Interpreter.equal_error
