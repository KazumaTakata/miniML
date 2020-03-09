open Mylib
open OUnit2

let test_CharToTokenType =
  [
    ( "test charToTokenType" >:: fun _ ->
      assert_equal Lex.LSQUARE (Lex.charToTokenType '[')
        ~printer:Lex.tokenTypeToString );
    ( "test charToTokenType" >:: fun _ ->
      assert_equal Lex.LSQUARE (Lex.charToTokenType '[')
        ~printer:Lex.tokenTypeToString );
    ( "test charToTokenType" >:: fun _ ->
      assert_equal Lex.LSQUARE (Lex.charToTokenType '[')
        ~printer:Lex.tokenTypeToString );
  ]

let tests = "test suite for Lex" >::: test_CharToTokenType

let _ = run_test_tt_main tests
