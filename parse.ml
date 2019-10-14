type tokenHolder = {tokenlist: Lex.token list; pos: int}

type astIdentifierNode = {value: string}

type astExpressionNode =
  | AstInfixNode of Lex.tokenType * astExpressionNode * astExpressionNode
  | AstNumberNode of int
  | AstIdentNode of astIdentifierNode

type astLetNode = astIdentifierNode * astExpressionNode

type astReturnNode = {value: astExpressionNode}

type astStatementNode =
  | AstLetNode of astLetNode
  | AstReturnNode of astReturnNode
  | AstExpressionNode of astExpressionNode

type astProgramNode = astStatementNode list

type expressionNodeAndTokenHolder = astExpressionNode * tokenHolder

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

let getBindingPower (token : Lex.token) : int =
  match token.typeOfToken with
  | Lex.PLUS ->
      10
  | Lex.MUL ->
      20
  | Lex.SEMICOLON ->
      0

let genAstTerminalNode (token : Lex.token) : astExpressionNode =
  match token.typeOfToken with
  | Lex.INT ->
      AstNumberNode (Pervasives.int_of_string token.literal)
  | Lex.IDENT ->
      AstIdentNode {value= token.literal}

(*mutually recursive function https://ocaml.org/learn/tutorials/labels.html*)
let rec led (tokenH : tokenHolder) (left : astExpressionNode) :
    expressionNodeAndTokenHolder =
  let curtoken = getCurToken tokenH in
  let rbp = getBindingPower curtoken in
  let tokenH = advanceToken tokenH in
  let right, tokenH = parseExpression tokenH rbp in
  (AstInfixNode (curtoken.typeOfToken, left, right), tokenH)

and foldl (rbp : int) (lbp : int) (left : astExpressionNode)
    (tokenH : tokenHolder) : expressionNodeAndTokenHolder =
  if rbp < lbp then
    let left, tokenH = led tokenH left in
    let curtoken = getCurToken tokenH in
    let lbp = getBindingPower curtoken in
    foldl rbp lbp left tokenH
  else (left, tokenH)

and parseExpression (tokenH : tokenHolder) (rbp : int) :
    expressionNodeAndTokenHolder =
  if peekTokenIs tokenH Lex.SEMICOLON then
    let curtoken = getCurToken tokenH in
    let tokenH = advanceToken tokenH in
    match curtoken.typeOfToken with
    | Lex.INT ->
        (AstNumberNode (Pervasives.int_of_string curtoken.literal), tokenH)
    | Lex.IDENT ->
        (AstIdentNode {value= curtoken.literal}, tokenH)
    | _ ->
        raise (Foo "eeee")
  else
    let curtoken = getCurToken tokenH in
    let left = genAstTerminalNode curtoken in
    let tokenH = advanceToken tokenH in
    let operator = getCurToken tokenH in
    let lbp = getBindingPower operator in
    foldl rbp lbp left tokenH

let parseLetStatement (tokenH : tokenHolder) : astStatementNode =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LET in
  let ident = getCurToken tokenH in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.IDENT in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.ASSIGN in
  let value, tokenH = parseExpression tokenH 0 in
  AstLetNode ({value= ident.literal}, value)

let parseExpressionStatement (tokenH tokenHolder) : astStatementNode = 
    let astexpressionnode = parseExpression tokenH 0 in
    AstExpressionNode astexpressionnode

let parse (tokenH : tokenHolder) =
  let currentToken = getCurToken tokenH in
  match currentToken.typeOfToken with
  | Lex.LET ->
      parseLetStatement tokenH
  | Lex.INT -> parseExpressionStatement tokenH
  
      parseExpression tokenH
