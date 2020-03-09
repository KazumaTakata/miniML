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
  | DUMMY

let tokenTypeToString (token_type : tokenType) : string =
  match token_type with
  | IDENT -> "IDENT"
  | INT -> "INT"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MUL -> "MUL"
  | GT -> "GT"
  | LT -> "LT"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LSQUARE -> "LSQUARE"
  | RSQUARE -> "RSQUARE"
  | FUNCTION -> "FUNCTION"
  | LET -> "LET"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | FOR -> "FOR"
  | _ -> "DUMMY"

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
  | _ -> DUMMY

let lookupIdent (literal : string) : tokenType =
  let keywords =
    [
      ("fn", FUNCTION);
      ("let", LET);
      ("true", TRUE);
      ("false", FALSE);
      ("if", IF);
      ("else", ELSE);
      ("return", RETURN);
      ("for", FOR);
    ]
  in
  let tokentype = List.Assoc.find ~equal:String.equal keywords literal in
  match tokentype with None -> IDENT | Some x -> x

type token = { typeOfToken : tokenType; literal : string }

type lexer = { input : string; position : int; ch : char; if_end : bool }

type tokenAndLexer = { token : token option; lexer : lexer }

let newTokenAndLexer (tok : token option) (lex : lexer) : tokenAndLexer =
  { token = tok; lexer = lex }

let isLetter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let isDigit = function '0' .. '9' -> true | _ -> false

let isLetterOrDigit (ch : char) : bool = isLetter ch || isDigit ch

let ifLexEnd (lex : lexer) : bool =
  if lex.position = String.length lex.input - 1 then true else false

let advanceLex (lex : lexer) : lexer =
  if not (ifLexEnd lex) then
    {
      input = lex.input;
      position = lex.position + 1;
      ch = lex.input.[lex.position + 1];
      if_end = false;
    }
  else
    {
      input = lex.input;
      position = lex.position;
      ch = lex.input.[lex.position];
      if_end = true;
    }

let rec skipWhitespace (lex : lexer) : lexer =
  if lex.if_end then lex
  else
    match lex.ch with
    | ' ' | '\t' | '\n' | '\r' -> skipWhitespace lex
    | _ -> lex

let rec readUntil (lex : lexer) (fn : char -> bool) : lexer =
  if fn lex.ch then readUntil lex fn else lex

let readUntilWithCondition (lex : lexer) (fn : char -> bool) : string * lexer =
  let pos = lex.position in
  let newlex = readUntil lex fn in
  let substring = String.sub newlex.input ~pos ~len:(newlex.position - pos) in
  (substring, newlex)

let nextToken (lex : lexer) : tokenAndLexer =
  let newlex = skipWhitespace lex in
  if newlex.if_end then newTokenAndLexer None newlex
  else
    match newlex.ch with
    | '=' | ';' | ':' | '(' | ')' | ',' | '+' | '>' | '<' | '*' | '{' | '}'
    | '[' | ']' ->
        let token =
          {
            typeOfToken = charToTokenType newlex.ch;
            literal = Char.escaped newlex.ch;
          }
        in
        newTokenAndLexer (Some token) (advanceLex newlex)
    | _ ->
        if isLetter newlex.ch then
          let token_string, lexer =
            readUntilWithCondition newlex isLetterOrDigit
          in
          let tokentype = lookupIdent token_string in
          let token =
            Some { typeOfToken = tokentype; literal = token_string }
          in
          newTokenAndLexer token lexer
        else
          let token_string, lexer = readUntilWithCondition newlex isDigit in
          let token = Some { typeOfToken = INT; literal = token_string } in
          newTokenAndLexer token lexer

let newLexer input_code =
  { input = input_code; position = 0; ch = input_code.[0]; if_end = false }
