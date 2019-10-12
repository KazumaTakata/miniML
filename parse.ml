type tokenHolder = {tokenlist: Lex.token list; pos: int}

type astNumberNode = {value: int}

type astIdentifierNode = AstIdentNode of string

type astExpressionNode = AstNumberNode of string

type astLetNode = astIdentifierNode * astExpressionNode

type astReturnNode = {value: astExpressionNode}

type astStatementNode =
  | AstLetNode of astLetNode
  | AstReturnNode of astReturnNode

type astProgramNode = astStatementNode list

exception Foo of string

let getTokenList (program : string) : tokenHolder =
  let ee = Lex.newLexer program in
  let tokenized = Lex.tokenize {tokenlist= []; lexer= ee} in
  {tokenlist= tokenized.tokenlist; pos= 0}

let getCurToken (tokenH : tokenHolder) : Lex.token =
  List.nth tokenH.tokenlist tokenH.pos

let advanceToken (tokenH : tokenHolder) : tokenHolder =
  {tokenlist= tokenH.tokenlist; pos= tokenH.pos + 1}

let tokenTypeAssertAndAdvance (tokenH : tokenHolder)
    (typeoftoken : Lex.tokenType) : tokenHolder =
  let currentToken = getCurToken tokenH in
  match currentToken.typeOfToken with
  | x when x = typeoftoken ->
      advanceToken tokenH
  | _ ->
      raise (Foo "eeee")

let peekTokenIs (tokenH : tokenHolder) (typeoftoken : Lex.tokenType) : bool =
  let peekToken = List.nth tokenH.tokenlist (tokenH.pos + 1) in
  match peekToken.typeOfToken with
  | x when x = typeoftoken ->
      true
  | _ ->
      false

let parseExpression (tokenH : tokenHolder) (rbp : int) : astExpressionNode =
  if peekTokenIs tokenH Lex.SEMICOLON then
    let curtoken = getCurToken tokenH in
    match curtoken.typeOfToken with
    | Lex.INT ->
        AstNumberNode curtoken.literal
    | Lex.IDENT ->
        AstNumberNode curtoken.literal
  else AstNumberNode ""

let parseLetStatement (tokenH : tokenHolder) : astStatementNode =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LET in
  let ident = getCurToken tokenH in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.IDENT in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.ASSIGN in
  let value = parseExpression tokenH 0 in
  AstLetNode (AstIdentNode ident.literal, value)

let parse (tokenH : tokenHolder) =
  let currentToken = getCurToken tokenH in
  match currentToken.typeOfToken with Lex.LET -> parseLetStatement tokenH
