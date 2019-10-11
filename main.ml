open Base
open Stdio
let () =
  let ee = Lex.newLexer "let ee = " in
  let ee2 = Lex.tokenize {tokenlist= []; lexer= ee} in
  printf "hellow"
