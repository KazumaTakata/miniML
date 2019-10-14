open Format

type evalObject = EvalIntObject of int | EvalBoolObject of bool

type objectType = INT_OBJ | BOOL_OBJ

let typeOfObject (obj : evalObject) : objectType =
  match obj with EvalIntObject _ -> INT_OBJ | EvalBoolObject _ -> BOOL_OBJ

let inspectObject (obj : evalObject) =
  match obj with
  | EvalIntObject x ->
      (Printf.printf "%s", "fe")
  | EvalBoolObject x ->
      (Printf.printf "%s", "fe")

(*let evalLet (node: astLetNode) : evalObject = *)
(*let ident, express = node in*)

let rec evalExpression (node : Parse.astExpressionNode) : evalObject =
  match node with AstNumberNode x -> EvalIntObject x

let rec evalStatement (node : Parse.astStatementNode) : evalObject =
  match node with AstExpressionNode node -> evalExpression node

let rec evalProgram (node : Parse.astProgramNode) : evalObject =
  match node with hd :: tl -> evalStatement hd
