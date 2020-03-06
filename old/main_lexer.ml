open Core

let setTermColor =
  Out_channel.output_string stdout "\027[31m" ;
  Out_channel.flush stdout

let rec mainLoop (_ : string) =
  setTermColor ;
  Out_channel.output_string stdout "type in command\n--------\n" ;
  Out_channel.flush stdout ;
  match In_channel.input_line In_channel.stdin with
  | None ->
      failwith "No timezone provided"
  | Some x ->
      let ee = Lex.newLexer x in
      let tokenlist_lexer = Lex.tokenize {tokenlist= []; lexer= ee} in
      let str = Lex.tokenlist_to_string tokenlist_lexer.tokenlist in
      Out_channel.output_string stdout str ;
      Out_channel.flush stdout ;
      mainLoop ""

let () = mainLoop ""
