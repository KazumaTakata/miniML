open Base
open Core

type evalObject = EvalIntObject of int | EvalBoolObject of bool

type objectType = INT_OBJ | BOOL_OBJ

type symbolTable = (string * evalObject) list

type evalEnvironment = {table: symbolTable}

type evalObjectAndEnv = evalObject * evalEnvironment

let typeOfObject (obj : evalObject) : objectType =
  match obj with EvalIntObject _ -> INT_OBJ | EvalBoolObject _ -> BOOL_OBJ

let inspectObject (obj : evalObject) =
  match obj with
  | EvalIntObject x ->
      printf "%d\n" x
  | EvalBoolObject x ->
      printf "%b\n" x

(*let evalLet (node: astLetNode) : evalObject = *)
(*let ident, express = node in*)

let evalPlus (evalleft : evalObject) (evalright : evalObject) : evalObject =
  match (evalleft, evalright) with
  | EvalIntObject x, EvalIntObject y ->
      EvalIntObject (x + y)

let evalMul (evalleft : evalObject) (evalright : evalObject) : evalObject =
  match (evalleft, evalright) with
  | EvalIntObject x, EvalIntObject y ->
      EvalIntObject (x * y)

let evalInfix (optype : Lex.tokenType) (evalleft : evalObject)
    (evalright : evalObject) : evalObject =
  match optype with
  | Lex.PLUS ->
      evalPlus evalleft evalright
  | Lex.MUL ->
      evalMul evalleft evalright

let rec evalExpression (node : Parse.astExpressionNode) (env : evalEnvironment)
    : evalObjectAndEnv =
  match node with
  | AstNumberNode x ->
      (EvalIntObject x, env)
  | AstInfixNode (optoken, astExpleft, astExpright) ->
      let evalleft, env = evalExpression astExpleft env in
      let evalright, env = evalExpression astExpright env in
      (evalInfix optoken evalleft evalright, env)

let rec evalLet (node : Parse.astLetNode) (env : evalEnvironment) :
    evalObjectAndEnv =
  let ident, expression = node in
  let expressionObj, env = evalExpression expression env in
  let table =
    List.Assoc.add ~equal:String.equal env.table ident.value expressionObj
  in
  (expressionObj, {table})

let rec evalStatement (node : Parse.astStatementNode) (env : evalEnvironment) :
    evalObjectAndEnv =
  match node with
  | AstExpressionNode node ->
      evalExpression node env
  | AstLetNode node ->
      evalLet node env

let genEnvironment : evalEnvironment =
  let symboltable : symbolTable = [] in
  {table= symboltable}

let rec evalProgram (node : Parse.astProgramNode) (env : evalEnvironment) :
    evalObjectAndEnv =
  match node with hd :: tl -> evalStatement hd env
