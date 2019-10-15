open Core

let setTermColor =
  Out_channel.output_string stdout "\027[34m" ;
  Out_channel.flush stdout

let rec mainLoop (_ : string) =
  setTermColor ;
  match In_channel.input_line In_channel.stdin with
  | None ->
      failwith "No timezone provided"
  | Some x -> (
      let ee = Lex.newLexer x in
      let tokenlist_lexer = Lex.tokenize {tokenlist= []; lexer= ee} in
      let token = List.nth tokenlist_lexer.tokenlist 0 in
      match token with
      | None ->
          failwith "No timezone provided"
      | Some token ->
          let str = Lex.token_to_string token in
          Out_channel.output_string stdout str ;
          Out_channel.flush stdout ;
          mainLoop "" )

let () = mainLoop "e"
