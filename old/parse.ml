type tokenHolder = {tokenlist: Lex.token list; pos: int}

type astIdentifierNode = {value: string}

type astExpressionNode =
  | AstInfixNode of Lex.tokenType * astExpressionNode * astExpressionNode
  | AstNumberNode of int
  | AstIdentNode of astIdentifierNode
  | AstFuncCallNode of astFuncCallNode
  | AstListNode of astListNode
  | AstDictNode of (astExpressionNode * astExpressionNode) list

and astFuncCallNode = astIdentifierNode * astExpressionNode list

and astListNode = astExpressionNode list

type astLetNode = astIdentifierNode * astExpressionNode

type astAssignNode = astIdentifierNode * astExpressionNode

type astReturnNode = {value: astExpressionNode}

type astStatementNode =
  | AstLetNode of astLetNode
  | AstReturnNode of astReturnNode
  | AstExpressionNode of astExpressionNode
  | AstFunctionNode of astFunctionNode
  | AstIfNode of astIfNode
  | AstAssignNode of astAssignNode
  | AstForNode of astForNode

and astFunctionNode =
  astIdentifierNode * astIdentifierNode list * astStatementNode list

and astIfNode = astExpressionNode * astStatementNode list

and astForNode =
  astStatementNode
  * astExpressionNode
  * astStatementNode
  * astStatementNode list

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
  | Lex.GT ->
      5
  | Lex.LT ->
      5
  | _ ->
      0

let rec genAstTerminalNode (token : Lex.token) (tokenH : tokenHolder) :
    astExpressionNode * tokenHolder =
  match token.typeOfToken with
  | Lex.INT ->
      let tokenH = advanceToken tokenH in
      (AstNumberNode (Stdlib.int_of_string token.literal), tokenH)
  | Lex.IDENT -> (
      let tokenH = advanceToken tokenH in
      let curtoken = getCurToken tokenH in
      match curtoken.typeOfToken with
      | x when x = Lex.LPAREN ->
          let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LPAREN in
          let arglist, tokenH = parseFuncCallArgument tokenH [] in
          let astfunccallnode : astFuncCallNode =
            ({value= token.literal}, arglist)
          in
          (AstFuncCallNode astfunccallnode, tokenH)
      | _ ->
          (AstIdentNode {value= token.literal}, tokenH) )

and parseFuncCallArgument (tokenH : tokenHolder)
    (arglist : astExpressionNode list) : astExpressionNode list * tokenHolder =
  let curtoken = getCurToken tokenH in
  match curtoken.typeOfToken with
  | Lex.IDENT | Lex.INT ->
      let arg, tokenH = genAstTerminalNode curtoken tokenH in
      let arglist = arglist @ [arg] in
      parseFuncCallArgument tokenH arglist
  | Lex.COMMA ->
      let tokenH = advanceToken tokenH in
      parseFuncCallArgument tokenH arglist
  | Lex.RPAREN ->
      let tokenH = advanceToken tokenH in
      (arglist, tokenH)

and parseFuncArgument (tokenH : tokenHolder) (arglist : astIdentifierNode list)
    : astIdentifierNode list * tokenHolder =
  let curtoken = getCurToken tokenH in
  let tokenH = advanceToken tokenH in
  match curtoken.typeOfToken with
  | Lex.IDENT ->
      let arg : astIdentifierNode = {value= curtoken.literal} in
      let arglist = arglist @ [arg] in
      parseFuncArgument tokenH arglist
  | Lex.COMMA ->
      parseFuncArgument tokenH arglist
  | Lex.RPAREN ->
      (arglist, tokenH)

let handleSEMICOLON (tokenH : tokenHolder) (rbp : int) : tokenHolder =
  let curtoken = getCurToken tokenH in
  match curtoken.typeOfToken with
  | Lex.SEMICOLON ->
      if rbp = 0 then
        let tokenH = tokenTypeAssertAndAdvance tokenH Lex.SEMICOLON in
        tokenH
      else tokenH
  | _ ->
      tokenH

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

and parseList (tokenH : tokenHolder) : astExpressionNode * tokenHolder =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LSQUARE in
  let explist, tokenH = parseListElement tokenH [] in
  (AstListNode explist, tokenH)

and parseListElement (tokenH : tokenHolder)
    (astexpressions : astExpressionNode list) :
    astExpressionNode list * tokenHolder =
  let astExp, tokenH = parseExpression tokenH 0 in
  let curtoken = getCurToken tokenH in
  let tokenH = advanceToken tokenH in
  let astexpressions = astexpressions @ [astExp] in
  match curtoken.typeOfToken with
  | Lex.COMMA ->
      parseListElement tokenH astexpressions
  | Lex.RSQUARE ->
      (astexpressions, tokenH)

and parseDictElement (tokenH : tokenHolder)
    (astkeyvalues : (astExpressionNode * astExpressionNode) list) :
    (astExpressionNode * astExpressionNode) list * tokenHolder =
  let curtoken = getCurToken tokenH in
  match curtoken.typeOfToken with
  | Lex.RBRACE ->
      let tokenH = tokenTypeAssertAndAdvance tokenH Lex.RBRACE in
      (astkeyvalues, tokenH)
  | _ ->
      let keyExp, tokenH = parseExpression tokenH 0 in
      let tokenH = tokenTypeAssertAndAdvance tokenH Lex.COLON in
      let valueExp, tokenH = parseExpression tokenH 0 in
      parseDictElement tokenH (astkeyvalues @ [(keyExp, valueExp)])

and parseDict (tokenH : tokenHolder) : astExpressionNode * tokenHolder =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LBRACE in
  let keyvalues, tokenH = parseDictElement tokenH [] in
  (AstDictNode keyvalues, tokenH)

and parseExpression (tokenH : tokenHolder) (rbp : int) :
    expressionNodeAndTokenHolder =
  let curtoken = getCurToken tokenH in
  match curtoken.typeOfToken with
  | Lex.LSQUARE ->
      let exp, tokenH = parseList tokenH in
      let tokenH = handleSEMICOLON tokenH rbp in
      (exp, tokenH)
  | Lex.LBRACE ->
      let exp, tokenH = parseDict tokenH in
      let tokenH = handleSEMICOLON tokenH rbp in
      (exp, tokenH)
  | _ ->
      let left, tokenH = genAstTerminalNode curtoken tokenH in
      let operator = getCurToken tokenH in
      let lbp = getBindingPower operator in
      let expression, tokenH = foldl rbp lbp left tokenH in
      let tokenH = handleSEMICOLON tokenH rbp in
      (expression, tokenH)

let parseLetStatement (tokenH : tokenHolder) : astStatementNode * tokenHolder =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LET in
  let ident = getCurToken tokenH in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.IDENT in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.ASSIGN in
  let value, tokenH = parseExpression tokenH 0 in
  (AstLetNode ({value= ident.literal}, value), tokenH)

let parseAssignStatement (tokenH : tokenHolder) :
    astStatementNode * tokenHolder =
  let ident = getCurToken tokenH in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.IDENT in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.ASSIGN in
  let value, tokenH = parseExpression tokenH 0 in
  (AstAssignNode ({value= ident.literal}, value), tokenH)

let parseReturnStatement (tokenH : tokenHolder) :
    astStatementNode * tokenHolder =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.RETURN in
  let expression, tokenH = parseExpression tokenH 0 in
  (AstReturnNode {value= expression}, tokenH)

let parseExpressionStatement (tokenH : tokenHolder) :
    astStatementNode * tokenHolder =
  let astexpressionnode, tokenH = parseExpression tokenH 0 in
  (AstExpressionNode astexpressionnode, tokenH)

let rec parseFuncBody (tokenH : tokenHolder)
    (statements : astStatementNode list) : astStatementNode list * tokenHolder
    =
  let curtoken = getCurToken tokenH in
  match curtoken.typeOfToken with
  | Lex.RBRACE ->
      (statements, tokenH)
  | _ ->
      let parsedstatement, tokenH = parseStatement tokenH in
      parseFuncBody tokenH (statements @ [parsedstatement])

and parseFunctionStatement (tokenH : tokenHolder) :
    astStatementNode * tokenHolder =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.FUNCTION in
  let ident = getCurToken tokenH in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.IDENT in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LPAREN in
  let arglist, tokenH = parseFuncArgument tokenH [] in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LBRACE in
  let body, tokenH = parseFuncBody tokenH [] in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.RBRACE in
  let astfunctionnode : astFunctionNode =
    ({value= ident.literal}, arglist, body)
  in
  (AstFunctionNode astfunctionnode, tokenH)

and parseStatement (tokenH : tokenHolder) : astStatementNode * tokenHolder =
  let currentToken = getCurToken tokenH in
  if peekTokenIs tokenH Lex.ASSIGN then parseAssignStatement tokenH
  else
    match currentToken.typeOfToken with
    | Lex.LET ->
        parseLetStatement tokenH
    | Lex.RETURN ->
        parseReturnStatement tokenH
    | Lex.INT | Lex.IDENT | Lex.LSQUARE | Lex.LBRACE ->
        parseExpressionStatement tokenH
    | Lex.FUNCTION ->
        parseFunctionStatement tokenH
    | Lex.IF ->
        parseIfStatement tokenH
    | Lex.FOR ->
        parseForStatement tokenH

and parseForStatement (tokenH : tokenHolder) : astStatementNode * tokenHolder =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.FOR in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LPAREN in
  let initExp, tokenH = parseStatement tokenH in
  let condExp, tokenH = parseExpression tokenH 0 in
  let updataExp, tokenH = parseStatement tokenH in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.RPAREN in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LBRACE in
  let astBody, tokenH = parseFuncBody tokenH [] in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.RBRACE in
  let astfornode : astForNode = (initExp, condExp, updataExp, astBody) in
  (AstForNode astfornode, tokenH)

and parseIfStatement (tokenH : tokenHolder) : astStatementNode * tokenHolder =
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.IF in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LPAREN in
  let astexp, tokenH = parseExpression tokenH 0 in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.RPAREN in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.LBRACE in
  let astBody, tokenH = parseFuncBody tokenH [] in
  let tokenH = tokenTypeAssertAndAdvance tokenH Lex.RBRACE in
  let astifnode : astIfNode = (astexp, astBody) in
  (AstIfNode astifnode, tokenH)

let rec parseStatements (tokenH : tokenHolder)
    (statements : astStatementNode list) : astStatementNode list * tokenHolder
    =
  if List.length tokenH.tokenlist <= tokenH.pos then (statements, tokenH)
  else
    let parsedstatement, tokenH = parseStatement tokenH in
    parseStatements tokenH (statements @ [parsedstatement])
