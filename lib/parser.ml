type op = Plus | Mul | Sub | Equal

type if_expr_node = {
  cond_expr : bi_expr_node;
  then_expr : bi_expr_node;
  else_expr : bi_expr_node;
}

and lambda_node = { args : string list; body : bi_expr_node }

and expr_node =
  | Int of int
  | Bool of bool
  | Id of string
  | Call of string * bi_expr_node list
  | Nested of bi_expr_node
  | If of if_expr_node
  | Lambda of lambda_node

and bi_expr_node = { left : expr_node; right : expr_node; op : op }

type decl_node = { func_name : string; argument : string; expr : expr_node }

let fail_message
    ((got_token_type : Lex.tokenType), (expected_token_type : Lex.tokenType)) :
    string =
  String.concat ""
    [
      "got:";
      Lex.tokenTypeToString got_token_type;
      "expected:";
      Lex.tokenTypeToString expected_token_type;
    ]

let token_assert ((token : Lex.token option), (token_type : Lex.tokenType)) =
  match token with
  | Some token ->
      if token.typeOfToken != token_type then
        failwith (fail_message (token.typeOfToken, token_type))
  | None -> failwith "token is None"

let parse_Args ((lexer : Lex.lexer), (args : string list)) :
    string list * Lex.lexer =
  let token_option, lexer = Lex.nextToken lexer in
  match token_option with
  | Some token ->
      if token.typeOfToken = Lex.IDENT then (args @ [ token.literal ], lexer)
      else (args, lexer)
  | None -> failwith "token is None"

let parse_Expr (lexer : Lex.lexer) : expr_node * Lex.lexer =
  let cur_token_option, lexer = Lex.nextToken lexer in
  match cur_token_option with
  | Some token -> (
      match token.typeOfToken with
      | INT -> (Int (int_of_string token.literal), lexer) )
  | None -> failwith "token is None"

let parse_Op (lexer : Lex.lexer) : op * Lex.lexer =
  let cur_token_option, lexer = Lex.nextToken lexer in
  match cur_token_option with
  | Some token -> ( match token.typeOfToken with Lex.PLUS -> (Plus, lexer) )
  | None -> failwith "token is None"

let parse_bi_Expr (lexer : Lex.lexer) : bi_expr_node * Lex.lexer =
  let left, lexer = parse_Expr lexer in
  let bi_op, lexer = parse_Op lexer in
  let right, lexer = parse_Expr lexer in
  ({ left; op = bi_op; right }, lexer)

let parse_Decl (lexer : Lex.lexer) : bi_expr_node * Lex.lexer =
  let token, lexer = Lex.nextToken lexer in
  token_assert (token, Lex.IDENT);
  let _, lexer = parse_Args (lexer, []) in
  token_assert (token, Lex.ASSIGN);
  let bi_expr, lexer = parse_bi_Expr lexer in
  (bi_expr, lexer)
