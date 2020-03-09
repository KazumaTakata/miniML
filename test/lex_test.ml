open Mylib
open OUnit2

let test_CharToTokenType =
  [
    ( "test charToTokenType" >:: fun _ ->
      assert_equal Lex.LSQUARE (Lex.charToTokenType '[')
        ~printer:Lex.tokenTypeToString );
    ( "test charToTokenType" >:: fun _ ->
      assert_equal Lex.RSQUARE (Lex.charToTokenType ']')
        ~printer:Lex.tokenTypeToString );
  ]

let test_lookupIdent =
  [
    ( "test lookupIdent" >:: fun _ ->
      assert_equal Lex.FUNCTION (Lex.lookupIdent "fn")
        ~printer:Lex.tokenTypeToString );
  ]

let test_isLetter =
  [
    ( "test isLetter" >:: fun _ ->
      assert_equal true (Lex.isLetter 'a') ~printer:Bool.to_string );
    ( "test isLetter" >:: fun _ ->
      assert_equal false (Lex.isLetter '(') ~printer:Bool.to_string );
  ]

let test_isDigit =
  [
    ( "test isLetter" >:: fun _ ->
      assert_equal true (Lex.isDigit '0') ~printer:Bool.to_string );
    ( "test isLetter" >:: fun _ ->
      assert_equal false (Lex.isDigit '(') ~printer:Bool.to_string );
  ]

let test_isLetterOrDigit =
  [
    ( "test isLetterOrDigit1" >:: fun _ ->
      assert_equal true (Lex.isLetterOrDigit '0') ~printer:Bool.to_string );
    ( "test isLetterOrDigit2" >:: fun _ ->
      assert_equal true (Lex.isLetterOrDigit 'x') ~printer:Bool.to_string );
  ]

let create_lex ((position : int), (if_end : bool), (input : string)) : Lex.lexer
    =
  { input; position; ch = input.[position]; if_end }

let lex_to_string (lexer : Lex.lexer) =
  String.concat ""
    [
      "{";
      lexer.input;
      ", ";
      string_of_int lexer.position;
      ", ";
      String.make 1 lexer.ch;
      ", ";
      Bool.to_string lexer.if_end;
      "}";
    ]

let test_ifLexEnd (input : string) =
  [
    ( "test ifLexInRange1" >:: fun _ ->
      assert_equal false
        (Lex.ifLexEnd (create_lex (0, false, input)))
        ~printer:Bool.to_string );
    ( "test ifLexInRange2" >:: fun _ ->
      assert_equal true
        (Lex.ifLexEnd (create_lex (String.length input - 1, false, input)))
        ~printer:Bool.to_string );
  ]

let test_advanceLex input =
  [
    ( "test advanceLex1" >:: fun _ ->
      assert_equal
        (create_lex (1, false, input))
        (Lex.advanceLex (create_lex (0, false, input)))
        ~printer:lex_to_string );
    ( "test advanceLex2" >:: fun _ ->
      assert_equal
        (create_lex (String.length input - 1, true, input))
        (Lex.advanceLex (create_lex (String.length input - 1, false, input)))
        ~printer:lex_to_string );
  ]

let test_skipWhitespace input =
  [
    ( "test skipWhitespace1" >:: fun _ ->
      assert_equal
        (create_lex (1, false, input))
        (Lex.skipWhitespace (create_lex (0, false, input)))
        ~printer:lex_to_string );
  ]

let test_readUntil input =
  [
    ( "test readUntil1" >:: fun _ ->
      assert_equal
        (create_lex (3, false, input))
        (Lex.readUntil (create_lex (0, false, input)) Lex.isDigit)
        ~printer:lex_to_string );
  ]

let test_getNumberOrString input =
  [
    ( "test getNumberOrString" >:: fun _ ->
      assert_equal
        ("aa", create_lex (3, false, input))
        (Lex.getNumberOrString (create_lex (0, false, input)) Lex.isDigit) );
  ]

let tests =
  "test suite for Lex"
  >::: test_CharToTokenType @ test_lookupIdent @ test_isLetter @ test_isDigit
       @ test_isLetterOrDigit @ test_ifLexEnd "(3+3)" @ test_advanceLex "(3+3)"
       @ test_skipWhitespace " (3+3)"
       @ test_readUntil "332ij"
       @ test_getNumberOrString "332ij"

let _ = run_test_tt_main tests
