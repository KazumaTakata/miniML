open Base
open Core

type tokenType =
  | IDENT
  | INT
  | ASSIGN
  | PLUS
  | MUL
  | GT
  | LT
  | COMMA
  | SEMICOLON
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LSQUARE
  | RSQUARE
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
  | FOR

let charToTokenType (ch : char) : tokenType =
  match ch with
  | '=' -> ASSIGN
  | ';' -> SEMICOLON
  | ':' -> COLON
  | '(' -> LPAREN
  | ')' -> RPAREN
  | ',' -> COMMA
  | '+' -> PLUS
  | '>' -> GT
  | '<' -> LT
  | '*' -> MUL
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '[' -> LSQUARE
  | ']' -> RSQUARE

type token = { typeOfToken : tokenType; literal : string }

type lexer = {
  input : string;
  position : int;
  ch : char;
  keywords : (string * tokenType) list;
}

type tokenAndLexer = { token : token option; lexer : lexer }

let newTokenAndLexer (tok : token option) (lex : lexer) : tokenAndLexer =
  { token = tok; lexer = lex }

let isLetter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let isDigit = function '0' .. '9' -> true | _ -> false

let isLetterOrDigit (ch : char) : bool = isLetter ch || isDigit ch

let ifLexInIsRange (lex : lexer) : bool =
  if lex.position < String.length lex.input then true else false

let advanceLex (lex : lexer) : lexer option =
  if ifLexInIsRange lex then
    Some
      {
        input = lex.input;
        position = lex.position + 1;
        ch = lex.input.[lex.position + 1];
        keywords = lex.keywords;
      }
  else None

let rec skipWhitespace (lex : lexer) : lexer =
  match lex.ch with
  | ' ' | '\t' | '\n' | '\r' -> (
      let newlex_option = advanceLex lex in
      match newlex_option with
      | Some newlex -> skipWhitespace newlex
      | None -> lex )
  | _ -> lex

let rec readUntil (lex : lexer) (fn : char -> bool) : lexer =
  if fn lex.ch then
    let newlex_option = advanceLex lex in
    match newlex_option with Some newlex -> readUntil newlex fn | None -> lex
  else lex

let readAlphaNumeric (lex : lexer) (fn : char -> bool) : string * lexer =
  let pos = lex.position in
  let newlex = readUntil lex fn in
  let substring = String.sub newlex.input ~pos ~len:(newlex.position - pos) in
  (substring, newlex)

let lookupIdent (literal : string) (lex : lexer) : tokenType =
  let tokentype = List.Assoc.find ~equal:String.equal lex.keywords literal in
  match tokentype with None -> IDENT | Some x -> x

let nextToken (lex : lexer) : tokenAndLexer =
  let newlex = skipWhitespace lex in
  match newlex.ch with
  | '=' | ';' | ':' | '(' | ')' | ',' | '+' | '>' | '<' | '*' | '{' | '}' | '['
  | ']' ->
      newTokenAndLexer
        {
          typeOfToken = charToTokenType newlex.ch;
          literal = Char.escaped newlex.ch;
        }
        (advanceLex newlex)
  | _ ->
      if isLetter newlex.ch then
        let lexerandstring = readAlphaNumeric newlex isLetterOrDigit in
        let tokentype =
          lookupIdent lexerandstring.string lexerandstring.lexer
        in
        newTokenAndLexer
          { typeOfToken = tokentype; literal = lexerandstring.string }
          lexerandstring.lexer
      else if isDigit newlex.ch then
        let lexerandstring = readAlphaNumeric newlex isDigit in
        newTokenAndLexer
          { typeOfToken = INT; literal = lexerandstring.string }
          lexerandstring.lexer
      else Out_channel.output_string stdout "illegal input\n"

let newLexer input_code =
  {
    input = input_code;
    position = 0;
    ch = input_code.[0];
    keywords =
      [
        ("fn", FUNCTION);
        ("let", LET);
        ("true", TRUE);
        ("false", FALSE);
        ("if", IF);
        ("else", ELSE);
        ("return", RETURN);
        ("for", FOR);
      ];
  }
