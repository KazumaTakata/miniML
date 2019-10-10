type tokenType =
  | ILLEGAL
  | EOF
  | IDENT
  | INT
  | ASSIGN
  | PLUS
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET

type token = {typeOfToken: tokenType; literal: string}

type lexer = {input: string; position: int; readPosition: int; ch: char}

type tokenAndLexer = {token: token; lexer: lexer}

let newTokenAndLexer (tok : token) (lex : lexer) : tokenAndLexer =
  {token= tok; lexer= lex}

let newLexer input_code =
  {input= input_code; position= -1; readPosition= 0; ch= '\x00'}

let readChar (lex : lexer) : lexer =
  { input= lex.input
  ; position= lex.position + 1
  ; readPosition= lex.readPosition + 1
  ; ch= lex.input.[lex.position + 1] }

let nextToken (lex : lexer) : tokenAndLexer =
  let newlex = readChar lex in
  match newlex.ch with
  | '=' ->
      newTokenAndLexer
        {typeOfToken= ASSIGN; literal= Char.escaped newlex.ch}
        newlex
  | ';' ->
      newTokenAndLexer
        {typeOfToken= SEMICOLON; literal= Char.escaped newlex.ch}
        newlex
  | '(' ->
      newTokenAndLexer
        {typeOfToken= LPAREN; literal= Char.escaped newlex.ch}
        newlex
  | ')' ->
      newTokenAndLexer
        {typeOfToken= RPAREN; literal= Char.escaped newlex.ch}
        newlex
  | ',' ->
      newTokenAndLexer
        {typeOfToken= COMMA; literal= Char.escaped newlex.ch}
        newlex
  | '+' ->
      newTokenAndLexer
        {typeOfToken= PLUS; literal= Char.escaped newlex.ch}
        newlex
  | '{' ->
      newTokenAndLexer
        {typeOfToken= LBRACE; literal= Char.escaped newlex.ch}
        newlex
  | '}' ->
      newTokenAndLexer
        {typeOfToken= RBRACE; literal= Char.escaped newlex.ch}
        newlex
  | '\x00' ->
      newTokenAndLexer
        {typeOfToken= EOF; literal= Char.escaped newlex.ch}
        newlex
  | _ ->
      newTokenAndLexer
        {typeOfToken= EOF; literal= Char.escaped newlex.ch}
        newlex
