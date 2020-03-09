let parse (lexer : Lex.lexer) = Lex.nextToken lexer

let token_assert ((token : Lex.token option), (token_type : Lex.tokenType)) =
  match token with
  | Some token ->
      if token.typeOfToken != token_type then failwith "Invalid_input"
  | None -> failwith "Invalid_input"

let parse_Args (lexer : Lex.lexer) = []

let parse_Decl (lexer : Lex.lexer) =
  let token, lexer = Lex.nextToken lexer in
  token_assert (token, Lex.IDENT);
  let args = parse_Args lexer in
  token_assert (token, Lex.ASSIGN)
