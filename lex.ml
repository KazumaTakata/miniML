open Base
open Core

type tokenType =
  | ILLEGAL
  | EOF
  | IDENT
  | INT
  | ASSIGN
  | PLUS
  | MUL
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN

type token = {typeOfToken: tokenType; literal: string}

let tokentype_to_string (tokentype : tokenType) : string =
  match tokentype with
  | ILLEGAL ->
      "ILLEGAL"
  | EOF ->
      "EOF"
  | IDENT ->
      "IDENT"
  | INT ->
      "INT"
  | ASSIGN ->
      "ASSIGN"
  | PLUS ->
      "PLUS"
  | MUL ->
      "MUL"
  | COMMA ->
      "COMMA"
  | SEMICOLON ->
      "SEMICOLON"
  | LPAREN ->
      "LPAREN"
  | RPAREN ->
      "RPAREN"
  | LBRACE ->
      "LBRACE"
  | RBRACE ->
      "RBRACE"
  | FUNCTION ->
      "FUNCTION"
  | LET ->
      "LET"
  | TRUE ->
      "TRUE"
  | FALSE ->
      "FALSE"
  | IF ->
      "IF"
  | ELSE ->
      "ELSE"
  | RETURN ->
      "RETURN"

let token_to_string (tok : token) : string =
  "{ " ^ tokentype_to_string tok.typeOfToken ^ "," ^ tok.literal ^ " }\n"

let rec tokenlist_to_string (toklist : token list) : string =
  match toklist with
  | [] ->
      ""
  | hd :: tl ->
      token_to_string hd ^ tokenlist_to_string tl

type lexer =
  { input: string
  ; position: int
  ; readPosition: int
  ; ch: char
  ; keywords: (string * tokenType) list }

type tokenAndLexer = {token: token; lexer: lexer}

type lexerAndString = {string: string; lexer: lexer}

type lexerAndTokenlist = {tokenlist: token list; lexer: lexer}

let newTokenAndLexer (tok : token) (lex : lexer) : tokenAndLexer =
  {token= tok; lexer= lex}

let newLexer input_code =
  { input= input_code
  ; position= 0
  ; readPosition= 1
  ; ch= input_code.[0]
  ; keywords=
      [ ("fn", FUNCTION)
      ; ("let", LET)
      ; ("true", TRUE)
      ; ("false", FALSE)
      ; ("if", IF)
      ; ("else", ELSE)
      ; ("return", RETURN) ] }

let readChar (lex : lexer) : lexer =
  if lex.readPosition >= String.length lex.input then
    { input= lex.input
    ; position= lex.position + 1
    ; readPosition= lex.readPosition + 1
    ; ch= '\x00'
    ; keywords= lex.keywords }
  else
    { input= lex.input
    ; position= lex.position + 1
    ; readPosition= lex.readPosition + 1
    ; ch= lex.input.[lex.position + 1]
    ; keywords= lex.keywords }

let isLetter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let isDigit = function '0' .. '9' -> true | _ -> false

let rec skipWhitespace (lex : lexer) : lexer =
  match lex.ch with
  | ' ' ->
      skipWhitespace (readChar lex)
  | '\t' ->
      skipWhitespace (readChar lex)
  | '\n' ->
      skipWhitespace (readChar lex)
  | '\r' ->
      skipWhitespace (readChar lex)
  | _ ->
      lex

let rec readUntil (lex : lexer) (fn : char -> bool) : lexer =
  if fn lex.ch then
    let newlex = readChar lex in
    readUntil newlex fn
  else lex

let readIdentifier (lex : lexer) (fn : char -> bool) : lexerAndString =
  let pos = lex.position in
  let newlex = readUntil lex fn in
  let substring = String.sub newlex.input ~pos ~len:(newlex.position - pos) in
  {string= substring; lexer= newlex}

let lookupIdent (literal : string) (lex : lexer) : tokenType =
  let tokentype = List.Assoc.find ~equal:String.equal lex.keywords literal in
  match tokentype with None -> IDENT | Some x -> x

let nextToken (lex : lexer) : tokenAndLexer =
  let newlex = skipWhitespace lex in
  match newlex.ch with
  | '=' ->
      newTokenAndLexer
        {typeOfToken= ASSIGN; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | ';' ->
      newTokenAndLexer
        {typeOfToken= SEMICOLON; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | '(' ->
      newTokenAndLexer
        {typeOfToken= LPAREN; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | ')' ->
      newTokenAndLexer
        {typeOfToken= RPAREN; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | ',' ->
      newTokenAndLexer
        {typeOfToken= COMMA; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | '+' ->
      newTokenAndLexer
        {typeOfToken= PLUS; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | '*' ->
      newTokenAndLexer
        {typeOfToken= MUL; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | '{' ->
      newTokenAndLexer
        {typeOfToken= LBRACE; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | '}' ->
      newTokenAndLexer
        {typeOfToken= RBRACE; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | '\x00' ->
      newTokenAndLexer
        {typeOfToken= EOF; literal= Char.escaped newlex.ch}
        (readChar newlex)
  | _ ->
      if isLetter newlex.ch then
        let lexerandstring = readIdentifier newlex isLetter in
        let tokentype =
          lookupIdent lexerandstring.string lexerandstring.lexer
        in
        newTokenAndLexer
          {typeOfToken= tokentype; literal= lexerandstring.string}
          lexerandstring.lexer
      else if isDigit newlex.ch then
        let lexerandstring = readIdentifier newlex isDigit in
        newTokenAndLexer
          {typeOfToken= INT; literal= lexerandstring.string}
          lexerandstring.lexer
      else
        newTokenAndLexer
          {typeOfToken= ILLEGAL; literal= Char.escaped newlex.ch}
          (readChar newlex)

let rec tokenize (lexerandtokenlist : lexerAndTokenlist) : lexerAndTokenlist =
  let tokenandlexer = nextToken lexerandtokenlist.lexer in
  match tokenandlexer.token.typeOfToken with
  | EOF ->
      lexerandtokenlist
  | _ ->
      tokenize
        { tokenlist=
            List.append lexerandtokenlist.tokenlist [tokenandlexer.token]
        ; lexer= tokenandlexer.lexer }
